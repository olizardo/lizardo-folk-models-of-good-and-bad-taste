library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(stringr)
library(nnet)
library(car)
library(FactoMineR)
library(factoextra)
library(jsonlite)
library(corrplot)
library(ggeffects)
library(ggsci)

config <- fromJSON("config.json")
good_model_dir <- config$good_model
bad_model_dir <- config$bad_model

dir.create("Plots", showWarnings = FALSE)
dir.create("Tabs", showWarnings = FALSE)

# 1. Wald Table for Demographics
raw_survey <- read_csv("data/FolkTaste_CleanData.csv", show_col_types = FALSE)
if(grepl("What do you mean", raw_survey$GoodTaste_Def[1])) {
  raw_survey <- raw_survey[-1, ]
}
raw_survey <- raw_survey %>% filter(Taste_Possibility == "YES")
raw_survey$python_index <- 0:(nrow(raw_survey)-1)

raw_survey$Age <- as.numeric(raw_survey$Age)
# Handle unrealistic ages (e.g. < 18) by setting them to NA so they are dropped in modeling
raw_survey$Age[raw_survey$Age < 18] <- NA

# Collapse EducationLevel_Coded PhD (6) into Masters (5) to prevent separation in mlogit due to N=4
raw_survey$EducationLevel_Coded[raw_survey$EducationLevel_Coded == 6] <- 5
raw_survey$EducationLevel_Coded <- as.factor(raw_survey$EducationLevel_Coded)
raw_survey$Political <- as.numeric(raw_survey$Political)


# ADD python_index globally since it's used multiple times
# (already added above)

# Generate descriptive statistics table
desc_data <- raw_survey %>% drop_na(Age, Gender, EducationLevel_Coded, Political) %>% filter(Gender %in% c("A man", "A woman"))

latex_desc <- "\\begin{table}[htpb]\n\\centering\n\\begin{tabular}{lrrr}\n\\toprule\n"
latex_desc <- paste0(latex_desc, "Variable & Mean / \\% & SD & Range \\\\\n\\midrule\n")
latex_desc <- paste0(latex_desc, "\\textbf{Numeric Variables} & & & \\\\\n")
latex_desc <- paste0(latex_desc, sprintf("Age & %.2f & %.2f & [%.0f, %.0f] \\\\\n", mean(desc_data$Age), sd(desc_data$Age), min(desc_data$Age), max(desc_data$Age)))
latex_desc <- paste0(latex_desc, sprintf("Political Ideology (1-7) & %.2f & %.2f & [%.0f, %.0f] \\\\\n", mean(desc_data$Political), sd(desc_data$Political), min(desc_data$Political), max(desc_data$Political)))
latex_desc <- paste0(latex_desc, "\\midrule\n\\textbf{Categorical Variables} & & & \\\\\n")

g_prop <- prop.table(table(desc_data$Gender)) * 100
latex_desc <- paste0(latex_desc, sprintf("Gender: Man & %.1f\\%% & - & - \\\\\n", g_prop["A man"]))
latex_desc <- paste0(latex_desc, sprintf("Gender: Woman & %.1f\\%% & - & - \\\\\n", g_prop["A woman"]))

e_prop <- prop.table(table(desc_data$EducationLevel_Coded)) * 100
latex_desc <- paste0(latex_desc, sprintf("Education: High School & %.1f\\%% & - & - \\\\\n", e_prop["2"]))
latex_desc <- paste0(latex_desc, sprintf("Education: Some College & %.1f\\%% & - & - \\\\\n", e_prop["3"]))
latex_desc <- paste0(latex_desc, sprintf("Education: College Degree & %.1f\\%% & - & - \\\\\n", e_prop["4"]))
latex_desc <- paste0(latex_desc, sprintf("Education: Graduate Degree & %.1f\\%% & - & - \\\\\n", e_prop["5"]))

latex_desc <- paste0(latex_desc, "\\bottomrule\n\\end{tabular}\n")
latex_desc <- paste0(latex_desc, "\\caption{Descriptive Statistics of Socio-Demographic Variables in Multinomial Models ($N = ", nrow(desc_data), "$)}\n")
latex_desc <- paste0(latex_desc, "\\label{tab:descriptives}\n\\end{table}\n")

write(latex_desc, "Tabs/descriptive_statistics.tex")

run_multinom <- function(prob_file, name) {
  probs <- read_csv(prob_file, show_col_types = FALSE)
  topic_cols <- setdiff(names(probs), "original_index")
  probs$primary_topic <- apply(probs[, topic_cols], 1, function(row) topic_cols[which.max(row)])
  
  probs_valid <- probs %>% filter(!primary_topic %in% c("-1"))
  
  merged_df <- merge(raw_survey, probs_valid, by.x="python_index", by.y="original_index")
  merged_df$primary_topic <- as.factor(merged_df$primary_topic)
  
  # Removed ParentEducation_Coded because it is missing for 85% of respondents (Drops N to 31 if included)
  model_data <- merged_df %>% drop_na(Age, Gender, EducationLevel_Coded, Political) %>% filter(Gender %in% c("A man", "A woman"))
  
  if (length(unique(model_data$primary_topic)) > 1) {
    model_data$primary_topic <- relevel(model_data$primary_topic, ref=names(sort(table(model_data$primary_topic), decreasing=TRUE)[1]))
    m <- multinom(primary_topic ~ poly(Age, 2) + Gender + EducationLevel_Coded + Political, data=model_data, trace=FALSE)
    w <- Anova(m, type="III")
    
    df <- as.data.frame(w)
    df$Predictor <- rownames(df)
    df$Model <- name
    return(df)
  }
  return(NULL)
}

generate_margin_plots <- function(model_dir, name, is_good) {
  # Get topic names internally to avoid scope issues
  get_topic_names_local <- function(dir) {
    info <- read_csv(file.path(dir, "topic_info.csv"), show_col_types = FALSE)
    meaningful_labels <- c(
      "0_similar_say_usually" = "Shared Taste",
      "1_art_appreciate_quality" = "Cultivated Discernment",
      "2_look_appealing_clothing" = "Personal Style",
      "3_pleasing_choices_aesthetically" = "Aesthetic Curation",
      "0_likes_different_opinion" = "Relational Distance",
      "1_choices_music_clothing" = "Questionable Consumption",
      "2_usually_quality_ugly" = "Lack of Refinement",
      "3_care_really_look" = "Aesthetic Neglect"
    )
    names_mapped <- meaningful_labels[info$Name]
    names_mapped[is.na(names_mapped)] <- str_to_title(str_replace_all(str_replace(info$Name[is.na(names_mapped)], "^\\d+_", ""), "_", " "))
    map <- setNames(names_mapped, as.character(info$Topic))
    map["-1"] <- "Outlier"
    map
  }

  prob_file <- file.path(model_dir, "document_topic_probabilities.csv")
  probs <- read_csv(prob_file, show_col_types = FALSE)
  topic_cols <- setdiff(names(probs), "original_index")
  probs$primary_topic <- apply(probs[, topic_cols], 1, function(row) topic_cols[which.max(row)])
  probs_valid <- probs %>% filter(!primary_topic %in% c("-1"))
  
  merged_df <- merge(raw_survey, probs_valid, by.x="python_index", by.y="original_index")
  
  # MAP TO MEANINGFUL LABELS
  t_map <- get_topic_names_local(model_dir)
  merged_df$primary_topic <- t_map[as.character(merged_df$primary_topic)]
  merged_df$primary_topic <- as.factor(merged_df$primary_topic)
  
  model_data <- merged_df %>% drop_na(Age, Gender, EducationLevel_Coded, Political) %>% filter(Gender %in% c("A man", "A woman"))
  # Sort table to pick reference level dynamically (largest group)
  model_data$primary_topic <- relevel(model_data$primary_topic, ref=names(sort(table(model_data$primary_topic), decreasing=TRUE)[1]))
  
  m <- multinom(primary_topic ~ poly(Age, 2) + Gender + EducationLevel_Coded + Political, data=model_data, trace=FALSE)
  
  suffix <- ifelse(is_good, "good", "bad")
  
  # 1. AGE PLOT
  eff_age <- ggeffect(m, terms = "Age [all]")
  eff_age$response.level <- str_wrap(eff_age$response.level, width = 20)
  
  p_age <- ggplot(eff_age, aes(x = x, y = predicted, group = response.level)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = response.level), alpha = 0.2) +
    geom_line(aes(color = response.level), linewidth = 1.2) +
    scale_fill_d3() +
    scale_color_d3() +
    labs(
      title = paste("Predicted Probabilities of", name, "by Age"),
      x = "Age",
      y = "Predicted Probability",
      color = "Cultural Model",
      fill = "Cultural Model"
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "none") +
    facet_wrap(~ response.level)
    
  ggsave(file.path("Plots", paste0("pred_age_ci_faceted_", suffix, ".png")), p_age, width = 10, height = 7, dpi = 300)
  
  # 2. GENDER PLOT
  eff_gender <- ggeffect(m, terms = "Gender")
  eff_gender$response.level <- str_wrap(eff_gender$response.level, width = 20)
  
  p_gender <- ggplot(eff_gender, aes(x = response.level, y = predicted, fill = x)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), position = position_dodge(width = 0.9), width = 0.2) +
    scale_fill_d3() +
    coord_flip() +
    labs(
      title = paste("Predicted Probabilities of", name, "by Gender"),
      x = "Cultural Model",
      y = "Predicted Probability",
      fill = "Gender"
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom")
    
  fn <- paste0("pred_gender_ci_", str_replace_all(name, " ", "_"), "_flipped.png")
  ggsave(file.path("Plots", fn), p_gender, width = 10, height = 7, dpi = 300)
}

# Run the marginal plots generation
generate_margin_plots(good_model_dir, "Good Taste Models", TRUE)
generate_margin_plots(bad_model_dir, "Bad Taste Models", FALSE)

res1 <- run_multinom(file.path(good_model_dir, "document_topic_probabilities.csv"), "Good Taste Def")
res2 <- run_multinom(file.path(bad_model_dir, "document_topic_probabilities.csv"), "Bad Taste Def")

all_res <- bind_rows(res1, res2)
all_res <- all_res %>% select(Model, Predictor, `LR Chisq`, Df, `Pr(>Chisq)`)

latex_str <- "\\begin{table}[htpb]\n\\centering\n\\begin{tabular}{llrrr}\n\\toprule\n"
latex_str <- paste0(latex_str, "Model & Predictor & LR $\\chi^2$ & Df & $p$-value \\\\\n\\midrule\n")
for (i in 1:nrow(all_res)) {
  pval <- all_res$`Pr(>Chisq)`[i]
  pval_str <- if(pval < 0.001) "$<$ 0.001" else sprintf("%.4f", pval)
  
  pred_label <- all_res$Predictor[i]
  if(pred_label == "poly(Age, 2)") pred_label <- "Age (Polynomial)"
  if(pred_label == "EducationLevel_Coded") pred_label <- "Education Level"
  if(pred_label == "ParentEducation_Coded") pred_label <- "Parent's Education"
  if(pred_label == "Political") pred_label <- "Political Ideology"
  
  latex_str <- paste0(latex_str, sprintf("%s & %s & %.2f & %d & %s \\\\\n", 
                                         all_res$Model[i], pred_label, all_res$`LR Chisq`[i], all_res$Df[i], pval_str))
}
latex_str <- paste0(latex_str, "\\bottomrule\n\\end{tabular}\n\\caption{Multinomial Logistic Regression Likelihood Ratio Tests for Topic Predictors}\n\\label{tab:wald_models}\n\\end{table}\n")
writeLines(latex_str, "Tabs/wald_models.tex")


# 2. Pearson Residual Plots
good_def_probs <- read_csv(file.path(good_model_dir, "document_topic_probabilities.csv"), show_col_types = FALSE)
bad_def_probs <- read_csv(file.path(bad_model_dir, "document_topic_probabilities.csv"), show_col_types = FALSE)

get_primary <- function(probs, name) {
  topic_cols <- setdiff(names(probs), "original_index")
  probs$primary <- apply(probs[, topic_cols], 1, function(row) topic_cols[which.max(row)])
  res <- probs %>% select(original_index, primary)
  names(res)[2] <- name
  return(res)
}

# Extract topic names dynamically from topic_info.csv
get_topic_names <- function(model_dir) {
  info <- read_csv(file.path(model_dir, "topic_info.csv"), show_col_types = FALSE)
  
  meaningful_labels <- c(
    "0_similar_say_usually" = "Shared Taste",
    "1_art_appreciate_quality" = "Cultivated Discernment",
    "2_look_appealing_clothing" = "Personal Style",
    "3_pleasing_choices_aesthetically" = "Aesthetic Curation",
    "0_likes_different_opinion" = "Relational Distance",
    "1_choices_music_clothing" = "Questionable Consumption",
    "2_usually_quality_ugly" = "Lack of Refinement",
    "3_care_really_look" = "Aesthetic Neglect"
  )
  
  names_mapped <- meaningful_labels[info$Name]
  names_mapped[is.na(names_mapped)] <- str_to_title(str_replace_all(str_replace(info$Name[is.na(names_mapped)], "^\\d+_", ""), "_", " "))
  
  map <- setNames(names_mapped, as.character(info$Topic))
  map["-1"] <- "Outlier"
  map
}

t_g_d <- get_topic_names(good_model_dir)
t_b_d <- get_topic_names(bad_model_dir)

gd <- get_primary(good_def_probs, "Good_Def")
bd <- get_primary(bad_def_probs, "Bad_Def")

df <- gd %>% inner_join(bd, by="original_index")
df <- df %>%
  filter(Good_Def != "-1", Bad_Def != "-1") %>%
  mutate(
    Good_Def = t_g_d[as.character(Good_Def)],
    Bad_Def = t_b_d[as.character(Bad_Def)]
  )

tb <- table(df$Good_Def, df$Bad_Def)
if (nrow(tb) > 1 && ncol(tb) > 1) {
  res <- chisq.test(tb)
  residuals_df <- as.data.frame(as.table(res$residuals))
  names(residuals_df) <- c("Var1", "Var2", "Residual")
  
  p <- ggplot(residuals_df, aes(x = Var1, y = Var2, fill = Residual)) +
    geom_tile(color = "white") +
    geom_text(aes(label = round(Residual, 2)), color = "black", size = 3.5, fontface="bold") +
    scale_fill_gradient2(low = "indianred", mid = "white", high = "steelblue", midpoint = 0) +
    scale_x_discrete(position = "bottom") +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.x = element_text(size = 9, face = "bold", angle=20, hjust=1),
      axis.text.y = element_text(size = 9, face = "bold")
    ) +
    labs(
      title = "Pearson Residuals: Good Def vs Bad Def",
      x = "Good Taste Def",
      y = "Bad Taste Def",
      fill = "Residual"
    )
  ggsave("Plots/schema_corr_Good_Def_Bad_Def.png", plot = p, width = 8, height = 6)
}


# Run for domain counts (called from external script now)
# (PCA and MANOVA were removed in favor of a count-based ANOVA)


print("R script executed successfully.")
library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(ggplot2)
library(jsonlite)

config <- fromJSON("config.json")
good_model_dir <- config$good_model
bad_model_dir <- config$bad_model

# 1. Load Data
raw_survey <- read_csv("data/FolkTaste_CleanData.csv", show_col_types = FALSE)
if(grepl("What do you mean", raw_survey$GoodTaste_Def[1])) {
  raw_survey <- raw_survey[-1, ]
}
raw_survey <- raw_survey %>% filter(Taste_Possibility == "YES")
raw_survey$python_index <- 0:(nrow(raw_survey)-1)

# 2. Get Primary Topics
get_primary <- function(path, name) {
  probs <- read_csv(path, show_col_types = FALSE)
  tc <- setdiff(names(probs), "original_index")
  probs[[name]] <- apply(probs[, tc], 1, function(row) tc[which.max(row)])
  probs %>% select(original_index, !!sym(name))
}

gd <- get_primary(file.path(good_model_dir, "document_topic_probabilities.csv"), "Good_Def")
bd <- get_primary(file.path(bad_model_dir, "document_topic_probabilities.csv"), "Bad_Def")

df <- raw_survey %>%
  left_join(gd, by=c("python_index"="original_index")) %>%
  left_join(bd, by=c("python_index"="original_index"))

df$Good_Def[df$Good_Def == "-1"] <- NA
df$Bad_Def[df$Bad_Def == "-1"] <- NA

# 3. Create the new variable: count of domains with score >= 2
dist_cols <- grep("Domains_Distinction1_", names(df), value = TRUE)

df$Domain_Count <- apply(df[, dist_cols], 1, function(row) {
  sum(as.numeric(row) >= 2, na.rm = TRUE)
})

# 4. Run ANOVAs
run_domain_anova <- function(topic_col, label) {
  test_df <- df %>% select(!!sym(topic_col), Domain_Count) %>% drop_na()
  
  if (length(unique(test_df[[topic_col]])) > 1) {
    formula <- as.formula(paste0("Domain_Count ~ ", topic_col))
    aov_res <- aov(formula, data = test_df)
    s <- summary(aov_res)[[1]]
    f_val <- s[["F value"]][1]
    p_val <- s[["Pr(>F)"]][1]
    df1 <- s[["Df"]][1]
    df2 <- s[["Df"]][2]
    
    # Calculate means and SE for plot
    means_df <- test_df %>%
      group_by(!!sym(topic_col)) %>%
      summarise(
        mean_val = mean(Domain_Count),
        se = sd(Domain_Count)/sqrt(n()),
        .groups = "drop"
      )
    
    return(list(
      stats = data.frame(Model=label, F_val=f_val, df1=df1, df2=df2, p_val=p_val),
      means = means_df
    ))
  }
  return(NULL)
}

res_good <- run_domain_anova("Good_Def", "Good Taste Def")
res_bad <- run_domain_anova("Bad_Def", "Bad Taste Def")

stats_df <- bind_rows(res_good$stats, res_bad$stats)

# Generate LaTeX table
stats_df$Format_F <- sprintf("%.2f", stats_df$F_val)
stats_df$Format_p <- sprintf("%.3f", stats_df$p_val)
stats_df$Format_p <- ifelse(stats_df$p_val < 0.001, paste0(stats_df$Format_p, "$^{***}$"), 
                    ifelse(stats_df$p_val < 0.01, paste0(stats_df$Format_p, "$^{**}$"),
                    ifelse(stats_df$p_val < 0.05, paste0(stats_df$Format_p, "$^{*}$"), stats_df$Format_p)))

latex_str <- "\\begin{table}[htpb]\n\\centering\n\\begin{tabular}{lcccc}\n\\toprule\n"
latex_str <- paste0(latex_str, "Predictor & $F$ & num Df & den Df & $p$-value \\\\\n\\midrule\n")
for (i in 1:nrow(stats_df)) {
  latex_str <- paste0(latex_str, sprintf("%s & %s & %d & %d & %s \\\\\n", 
        stats_df$Model[i], stats_df$Format_F[i], stats_df$df1[i], stats_df$df2[i], stats_df$Format_p[i]))
}
latex_str <- paste0(latex_str, "\\bottomrule\n\\multicolumn{5}{l}{\\footnotesize $^{*}p<0.05; ^{**}p<0.01; ^{***}p<0.001$} \\\\\n")
latex_str <- paste0(latex_str, "\\end{tabular}\n\\caption{ANOVA Results: Number of Domains (score $\\geq 2$) by Taste Cultural Model.}\n\\label{tab:anova_domain_counts}\n\\end{table}\n")

writeLines(latex_str, "Tabs/anova_domain_counts.tex")

# 5. Generate Plots
plot_means <- function(means_data, x_col, title, filename, map_dict) {
  # Apply names
  means_data[[x_col]] <- map_dict[as.character(means_data[[x_col]])]
  
  # Format names
  means_data[[x_col]] <- str_replace_all(means_data[[x_col]], " ", "\n")
  
  p <- ggplot(means_data, aes(x = !!sym(x_col), y = mean_val, group = 1)) +
    geom_line(color = "steelblue", linewidth = 1) +
    geom_point(color = "steelblue", size = 3) +
    geom_errorbar(aes(ymin = mean_val - 1.96*se, ymax = mean_val + 1.96*se), width = 0.2, color="steelblue") +
    theme_minimal() +
    labs(
      title = title,
      x = "Cultural Model",
      y = "Mean # of Domains (\u2265 2)"
    ) +
    theme(
      text = element_text(size = 14),
      axis.text.x = element_text(angle = 0, hjust = 0.5)
    )
  ggsave(filename, plot = p, width = 8, height = 5)
}

# Extract topic names dynamically from topic_info.csv
get_topic_names <- function(model_dir) {
  info <- read_csv(file.path(model_dir, "topic_info.csv"), show_col_types = FALSE)
  
  meaningful_labels <- c(
    "0_similar_say_usually" = "Shared Taste",
    "1_art_appreciate_quality" = "Cultivated Discernment",
    "2_look_appealing_clothing" = "Personal Style",
    "3_pleasing_choices_aesthetically" = "Aesthetic Curation",
    "0_likes_different_opinion" = "Relational Distance",
    "1_choices_music_clothing" = "Questionable Consumption",
    "2_usually_quality_ugly" = "Lack of Refinement",
    "3_care_really_look" = "Aesthetic Neglect"
  )
  
  names_mapped <- meaningful_labels[info$Name]
  names_mapped[is.na(names_mapped)] <- str_to_title(str_replace_all(str_replace(info$Name[is.na(names_mapped)], "^\\d+_", ""), "_", " "))
  
  map <- setNames(names_mapped, as.character(info$Topic))
  map["-1"] <- "Outlier"
  map
}

t_g_d <- get_topic_names(good_model_dir)
t_b_d <- get_topic_names(bad_model_dir)

dir.create("Plots", showWarnings = FALSE)
if (!is.null(res_good)) {
  plot_means(res_good$means, "Good_Def", "Mean Number of Distinguishable Domains\nby Good Taste Cultural Model", "Plots/domain_counts_good.png", t_g_d)
}
if (!is.null(res_bad)) {
  plot_means(res_bad$means, "Bad_Def", "Mean Number of Distinguishable Domains\nby Bad Taste Cultural Model", "Plots/domain_counts_bad.png", t_b_d)
}

print("Domain counts ANOVA and plots generated successfully.")
library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(jsonlite)

config <- fromJSON("config.json")
good_model_dir <- config$good_model
bad_model_dir <- config$bad_model

# 1. Load Data
raw_survey <- read_csv("data/FolkTaste_CleanData.csv", show_col_types = FALSE)
if(grepl("What do you mean", raw_survey$GoodTaste_Def[1])) {
  raw_survey <- raw_survey[-1, ]
}
raw_survey <- raw_survey %>% filter(Taste_Possibility == "YES")
raw_survey$python_index <- 0:(nrow(raw_survey)-1)

# 2. Get Primary Topics
get_primary <- function(path, name) {
  probs <- read_csv(path, show_col_types = FALSE)
  tc <- setdiff(names(probs), "original_index")
  probs[[name]] <- apply(probs[, tc], 1, function(row) tc[which.max(row)])
  probs %>% select(original_index, !!sym(name))
}

gd <- get_primary(file.path(good_model_dir, "document_topic_probabilities.csv"), "Good_Def")
bd <- get_primary(file.path(bad_model_dir, "document_topic_probabilities.csv"), "Bad_Def")

df <- raw_survey %>%
  left_join(gd, by=c("python_index"="original_index")) %>%
  left_join(bd, by=c("python_index"="original_index"))

# Remove outliers for ANOVA
df$Good_Def[df$Good_Def == "-1"] <- NA
df$Bad_Def[df$Bad_Def == "-1"] <- NA

dist_cols <- grep("Domains_Distinction1_", names(df), value = TRUE)

run_anova_for_model <- function(topic_col) {
  results <- list()
  for (col in dist_cols) {
    domain_name <- str_remove(col, "Domains_Distinction1_")
    
    test_df <- df %>% select(!!sym(topic_col), !!sym(col)) %>% drop_na()
    test_df[[col]] <- as.numeric(test_df[[col]])
    
    if (length(unique(test_df[[topic_col]])) > 1) {
      formula <- as.formula(paste0("`", col, "` ~ ", topic_col))
      aov_res <- aov(formula, data = test_df)
      s <- summary(aov_res)[[1]]
      f_val <- s[["F value"]][1]
      p_val <- s[["Pr(>F)"]][1]
      results[[domain_name]] <- data.frame(Domain=domain_name, F_val=f_val, p_val=p_val)
    }
  }
  res_df <- bind_rows(results)
  res_df$p_adj <- p.adjust(res_df$p_val, method="BH")
  res_df
}

res_gd <- run_anova_for_model("Good_Def")
res_bd <- run_anova_for_model("Bad_Def")

# Format into a single table
combined <- res_gd %>% select(Domain)
format_cell <- function(f, p_adj) {
  sig <- ifelse(p_adj < 0.001, "$^{***}$", ifelse(p_adj < 0.01, "$^{**}$", ifelse(p_adj < 0.05, "$^{*}$", "")))
  sprintf("%.2f%s (%.3f)", f, sig, p_adj)
}

combined$Good_Def <- format_cell(res_gd$F_val, res_gd$p_adj)
combined$Bad_Def <- format_cell(res_bd$F_val, res_bd$p_adj)

dir.create("Tabs", showWarnings = FALSE)

# Write to LaTeX
latex_str <- "\\begin{table}[htpb]\n\\centering\n\\resizebox{\\textwidth}{!}{\n\\begin{tabular}{lcc}\n\\toprule\n"
latex_str <- paste0(latex_str, "Domain & Good Taste Def & Bad Taste Def \\\\\n")
latex_str <- paste0(latex_str, "& $F$ ($p_{adj}$) & $F$ ($p_{adj}$) \\\\\n\\midrule\n")

for (i in 1:nrow(combined)) {
  latex_str <- paste0(latex_str, sprintf("%s & %s & %s \\\\\n", 
                                         str_replace_all(combined$Domain[i], "_", "\\\\_"), 
                                         combined$Good_Def[i], combined$Bad_Def[i]))
}

latex_str <- paste0(latex_str, "\\bottomrule\n\\multicolumn{3}{l}{\\footnotesize $^{*}p<0.05; ^{**}p<0.01; ^{***}p<0.001$ (Benjamini-Hochberg FDR corrected)} \\\\\n")
latex_str <- paste0(latex_str, "\\end{tabular}\n}\n\\caption{One-Way ANOVA Results: Mean Differences in Domain Distinction Ratings by Taste Cultural Model. Values are $F$-statistics with FDR-adjusted $p$-values in parentheses.}\n\\label{tab:anova_distinction}\n\\end{table}\n")

writeLines(latex_str, "Tabs/anova_distinction.tex")
library(tidytext)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(stringr)
library(jsonlite)

config <- fromJSON("config.json")
good_model_dir <- config$good_model
bad_model_dir <- config$bad_model

dir.create("Plots", showWarnings = FALSE)

# --- TOPIC FREQUENCIES AND KEYWORDS ---
plot_topics_and_keywords <- function(model_dir, title_prefix, color, prefix) {
  # 1. Frequencies
  info <- read_csv(file.path(model_dir, "topic_info.csv"), show_col_types = FALSE) %>% filter(Topic != -1)
  
  # Format names
  meaningful_labels <- c(
    "0_similar_say_usually" = "Shared Taste",
    "1_art_appreciate_quality" = "Cultivated Discernment",
    "2_look_appealing_clothing" = "Personal Style",
    "3_pleasing_choices_aesthetically" = "Aesthetic Curation",
    "0_likes_different_opinion" = "Relational Distance",
    "1_choices_music_clothing" = "Questionable Consumption",
    "2_usually_quality_ugly" = "Lack of Refinement",
    "3_care_really_look" = "Aesthetic Neglect"
  )
  info$NameClean <- meaningful_labels[info$Name]
  info$NameClean[is.na(info$NameClean)] <- str_to_title(str_replace_all(str_replace(info$Name[is.na(info$NameClean)], "^\\d+_", ""), "_", " "))
  
  p_topics <- ggplot(info, aes(x = reorder(NameClean, Count), y = Count)) +
    geom_bar(stat = "identity", fill = color) +
    coord_flip() +
    theme_minimal(base_size = 14) +
    labs(title = paste(title_prefix, "(Definitions)"), x = "Cultural Model", y = "Count")
  ggsave(sprintf("Plots/%s_topics.png", prefix), plot = p_topics, width = 8, height = 6)
  
  # 2. Keywords
  clean_keywords <- function(rep_str) {
    words <- str_extract_all(rep_str, "'([^']+)'")[[1]]
    words <- str_replace_all(words, "'", "")
    return(words)
  }
  
  word_list <- list()
  for (i in 1:nrow(info)) {
    words <- clean_keywords(info$Representation[i])
    # Give them a dummy score decreasing from 10 to 1 for plotting order
    scores <- seq(length(words), 1, length.out = length(words))
    df_words <- data.frame(
      Topic = info$NameClean[i],
      Word = words,
      Score = scores
    )
    word_list[[i]] <- df_words
  }
  
  words_df <- bind_rows(word_list)
  
  p_keywords <- ggplot(words_df, aes(x = reorder_within(Word, Score, Topic), y = Score)) +
    geom_bar(stat = "identity", fill = color, show.legend = FALSE) +
    scale_x_reordered() +
    coord_flip() +
    facet_wrap(~ Topic, scales = "free_y") +
    theme_minimal(base_size = 12) +
    theme(
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    ) +
    labs(title = paste("Top Keywords per", title_prefix, "Cultural Model"), x = "Keyword", y = "Relative Prominence (NMF Weight)")
  
  ggsave(sprintf("Plots/%s_keywords.png", prefix), plot = p_keywords, width = 10, height = 6)
}
plot_topics_and_keywords(good_model_dir, "Good Taste", "steelblue", "good_taste")
plot_topics_and_keywords(bad_model_dir, "Bad Taste", "indianred", "bad_taste")

# --- RE-GENERATE CA PLOTS ---
library(FactoMineR)
library(factoextra)

plot_ca <- function(model_dir, title_prefix, prefix) {
  # Get names for plotting
  info <- read_csv(file.path(model_dir, "topic_info.csv"), show_col_types = FALSE)
  meaningful_labels <- c(
    "0_similar_say_usually" = "Shared Taste",
    "1_art_appreciate_quality" = "Cultivated Discernment",
    "2_look_appealing_clothing" = "Personal Style",
    "3_pleasing_choices_aesthetically" = "Aesthetic Curation",
    "0_likes_different_opinion" = "Relational Distance",
    "1_choices_music_clothing" = "Questionable Consumption",
    "2_usually_quality_ugly" = "Lack of Refinement",
    "3_care_really_look" = "Aesthetic Neglect"
  )
  names_mapped <- meaningful_labels[info$Name]
  names_mapped[is.na(names_mapped)] <- str_to_title(str_replace_all(str_replace(info$Name[is.na(names_mapped)], "^\\d+_", ""), "_", " "))
  
  map <- setNames(names_mapped, as.character(info$Topic))
  
  probs <- read_csv(file.path(model_dir, "document_topic_probabilities.csv"), show_col_types = FALSE)
  probs_clean <- probs %>% select(-original_index)
  colnames(probs_clean) <- unname(map[as.character(0:(ncol(probs_clean)-1))])
  ca_data <- probs_clean + 1e-6
  assigned_raw <- as.character(apply(probs_clean, 1, which.max) - 1)
  assigned <- as.factor(unname(map[assigned_raw]))
  
  res.ca <- CA(ca_data, graph = FALSE)
  p_ca <- fviz_ca_biplot(res.ca, geom.row = "point", col.row = assigned, col.col = "black", 
                         title = paste("CA:", title_prefix, "Taste Cultural Models"), palette = "jco", legend.title = "Assigned Cultural Model")
  p_ca <- p_ca + theme(legend.position = "bottom", legend.direction="vertical")
  ggsave(sprintf("Plots/%s.png", prefix), plot = p_ca, width = 10, height = 7)
}
plot_ca(good_model_dir, "Good", "ca_good_taste")
plot_ca(bad_model_dir, "Bad", "ca_bad_taste")
raw_survey <- read_csv("data/FolkTaste_CleanData.csv", show_col_types = FALSE)
if(grepl("What do you mean", raw_survey$GoodTaste_Def[1])) raw_survey <- raw_survey[-1, ]
raw_survey <- raw_survey %>% filter(Taste_Possibility == "YES")

gd_probs <- read_csv(file.path(good_model_dir, "document_topic_probabilities.csv"), show_col_types = FALSE)
bd_probs <- read_csv(file.path(bad_model_dir, "document_topic_probabilities.csv"), show_col_types = FALSE)

get_primary <- function(probs, name) {
  tc <- setdiff(names(probs), "original_index")
  probs$primary <- apply(probs[, tc], 1, function(row) tc[which.max(row)])
  res <- probs %>% select(original_index, primary)
  names(res)[2] <- name
  res
}

gd <- get_primary(gd_probs, "Good_Def")
bd <- get_primary(bd_probs, "Bad_Def")

df <- gd %>% inner_join(bd, by="original_index")

# Extract topic names dynamically from topic_info.csv
get_topic_names <- function(model_dir) {
  info <- read_csv(file.path(model_dir, "topic_info.csv"), show_col_types = FALSE)
  
  meaningful_labels <- c(
    "0_similar_say_usually" = "Shared Taste",
    "1_art_appreciate_quality" = "Cultivated Discernment",
    "2_look_appealing_clothing" = "Personal Style",
    "3_pleasing_choices_aesthetically" = "Aesthetic Curation",
    "0_likes_different_opinion" = "Relational Distance",
    "1_choices_music_clothing" = "Questionable Consumption",
    "2_usually_quality_ugly" = "Lack of Refinement",
    "3_care_really_look" = "Aesthetic Neglect"
  )
  
  names_mapped <- meaningful_labels[info$Name]
  names_mapped[is.na(names_mapped)] <- str_to_title(str_replace_all(str_replace(info$Name[is.na(names_mapped)], "^\\d+_", ""), "_", " "))
  
  map <- setNames(names_mapped, as.character(info$Topic))
  map["-1"] <- "Outlier"
  map
}

t_g_d <- get_topic_names(good_model_dir)
t_b_d <- get_topic_names(bad_model_dir)

df <- df %>%
  filter(Good_Def != "-1", Bad_Def != "-1") %>%
  mutate(
    Good_Def = t_g_d[as.character(Good_Def)],
    Bad_Def = t_b_d[as.character(Bad_Def)]
  )

combos <- list(c("Good_Def", "Bad_Def"))

for (c in combos) {
  v1 <- c[1]
  v2 <- c[2]
  tb <- table(df[[v1]], df[[v2]])
  if(nrow(tb)>1 && ncol(tb)>1) {
    res <- chisq.test(tb)
    r_df <- as.data.frame(as.table(res$residuals))
    names(r_df) <- c("Var1", "Var2", "Residual")
    
    # Format labels with newlines so they fit horizontally
    r_df$Var1 <- str_replace_all(r_df$Var1, "/", "/\n")
    r_df$Var1 <- str_replace_all(r_df$Var1, " ", "\n")
    
    p <- ggplot(r_df, aes(x = Var1, y = Var2, fill = Residual)) +
      geom_tile(color = "white") +
      geom_text(aes(label = round(Residual, 2)), color = "black", size = 6, fontface = "bold") +
      scale_fill_gradient2(low = "indianred", mid = "white", high = "steelblue", midpoint = 0) +
      scale_x_discrete(position = "top") +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0, size = 12, face = "bold"),
            axis.text.y = element_text(size = 12, face = "bold")) +
      labs(
        title = sprintf("Pearson Residuals: %s vs %s", str_replace(v1, "_", " "), str_replace(v2, "_", " ")),
        x = str_replace(v1, "_", " "),
        y = str_replace(v2, "_", " "),
        fill = "Residual"
      )
    ggsave(sprintf("Plots/schema_corr_%s_%s.png", v1, v2), plot=p, width=10, height=7)
  }
}

# --- RE-GENERATE HEATMAPS (DEFINITIONS ONLY) ---
df_g_d <- get_primary(gd_probs, "Good_Def")
df_b_d <- get_primary(bd_probs, "Bad_Def")

raw_survey$python_index <- 0:(nrow(raw_survey)-1)
m_df <- raw_survey %>%
  left_join(df_g_d, by=c("python_index"="original_index")) %>%
  left_join(df_b_d, by=c("python_index"="original_index")) %>%
  mutate(
    Good_Def = t_g_d[as.character(Good_Def)],
    Bad_Def = t_b_d[as.character(Bad_Def)]
  )

distinction_cols <- grep("Domains_Distinction1_", names(m_df), value = TRUE)

create_heatmap <- function(data, group_col, title, filename) {
  d_clean <- data %>% filter(!is.na(!!sym(group_col)) & !!sym(group_col) != "Outlier")
  
  dist_means <- d_clean %>%
    select(all_of(group_col), all_of(distinction_cols)) %>%
    pivot_longer(cols = starts_with("Domains_Distinction1_"), names_to = "Domain", values_to = "Score") %>%
    mutate(Domain = str_remove(Domain, "Domains_Distinction1_"), Score = as.numeric(Score)) %>%
    filter(!is.na(Score)) %>%
    group_by(!!sym(group_col), Domain) %>%
    summarize(Probability = mean(Score >= 2, na.rm = TRUE), .groups = 'drop')
  
  domain_order <- dist_means %>% group_by(Domain) %>% summarize(Overall = mean(Probability)) %>% arrange(Overall) %>% pull(Domain)
  dist_means <- dist_means %>% mutate(Domain = factor(Domain, levels = domain_order))
  
  # Add newlines to make sure they fit horizontally
  dist_means[[group_col]] <- str_replace_all(dist_means[[group_col]], "/", "/\n")
  dist_means[[group_col]] <- str_replace_all(dist_means[[group_col]], " ", "\n")
  
  global_mean <- mean(dist_means$Probability, na.rm = TRUE)
  
  p <- ggplot(dist_means, aes(x = !!sym(group_col), y = Domain, fill = Probability)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sprintf("%.2f", Probability)), color = "black", size = 4) +
    scale_fill_gradient2(low = "steelblue", mid = "white", high = "indianred", midpoint = global_mean) +
    scale_x_discrete(position = "top") +
    theme_minimal(base_size = 14) +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0)) +
    labs(title = title, x = "Cultural Model", y = "Cultural Domain", fill = "Probability\n(Score \u2265 2)")
  
  ggsave(filename, plot = p, width = 11, height = 7)
}

create_heatmap(m_df, "Good_Def", "Average Distinction Rating by Domain (Good Taste Definitions)", "Plots/distinction_domains.png")
create_heatmap(m_df, "Bad_Def", "Average Distinction Rating by Domain (Bad Taste Definitions)", "Plots/distinction_domains_bad.png")
