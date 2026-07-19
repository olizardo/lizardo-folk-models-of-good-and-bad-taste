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

writeLines(latex_str, "report/Tabs/anova_domain_counts.tex")

# 5. Generate Plots
plot_means <- function(means_data, x_col, title, filename, map_dict) {
  # Add "Topic " prefix to make it look nicer
  means_data[[x_col]] <- map_dict[as.character(means_data[[x_col]])]
  
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
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  ggsave(filename, plot = p, width = 6, height = 5)
}

# Extract topic names dynamically from topic_info.csv
get_topic_names <- function(model_dir) {
  info <- read_csv(file.path(model_dir, "topic_info.csv"), show_col_types = FALSE)
  names_clean <- str_replace_all(str_replace(info$Name, "^\\d+_", ""), "_", " ")
  names_title <- str_to_title(names_clean)
  map <- setNames(names_title, as.character(info$Topic))
  map["-1"] <- "Outlier"
  map
}

t_g_d <- get_topic_names(good_model_dir)
t_b_d <- get_topic_names(bad_model_dir)

dir.create("report/Plots", showWarnings = FALSE)
if (!is.null(res_good)) {
  plot_means(res_good$means, "Good_Def", "Mean Number of Distinguishable Domains\nby Good Taste Cultural Model", "report/Plots/domain_counts_good.png", t_g_d)
}
if (!is.null(res_bad)) {
  plot_means(res_bad$means, "Bad_Def", "Mean Number of Distinguishable Domains\nby Bad Taste Cultural Model", "report/Plots/domain_counts_bad.png", t_b_d)
}

print("Domain counts ANOVA and plots generated successfully.")
