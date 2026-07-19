
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(stringr)
library(nnet)
library(car)
library(FactoMineR)
library(factoextra)

dir.create("report/Plots", showWarnings = FALSE)
dir.create("report/Tabs", showWarnings = FALSE)

# 1. Wald Table for Demographics
raw_survey <- read_csv("data/FolkTaste_CleanData.csv", show_col_types = FALSE)
if(grepl("What do you mean", raw_survey$GoodTaste_Def[1])) {
  raw_survey <- raw_survey[-1, ]
}
raw_survey <- raw_survey %>% filter(Taste_Possibility == "YES")
raw_survey$Age <- as.numeric(raw_survey$Age)
raw_survey$EducationLevel_Coded <- as.numeric(raw_survey$EducationLevel_Coded)
raw_survey$Political <- as.numeric(raw_survey$Political)

# ADD python_index globally since it's used multiple times
raw_survey$python_index <- 0:(nrow(raw_survey)-1)

run_multinom <- function(prob_file, name) {
  probs <- read_csv(prob_file, show_col_types = FALSE)
  topic_cols <- setdiff(names(probs), "original_index")
  probs$primary_topic <- apply(probs[, topic_cols], 1, function(row) topic_cols[which.max(row)])
  
  probs_valid <- probs %>% filter(!primary_topic %in% c("-1"))
  
  merged_df <- merge(raw_survey, probs_valid, by.x="python_index", by.y="original_index")
  merged_df$primary_topic <- as.factor(merged_df$primary_topic)
  
  model_data <- merged_df %>% drop_na(Age, Gender, EducationLevel_Coded, Political) %>% filter(Gender %in% c("A man", "A woman"))
  
  if (length(unique(model_data$primary_topic)) > 1) {
    model_data$primary_topic <- relevel(model_data$primary_topic, ref=names(sort(table(model_data$primary_topic), decreasing=TRUE)[1]))
    m <- multinom(primary_topic ~ Age + Gender + EducationLevel_Coded + Political, data=model_data, trace=FALSE)
    w <- Anova(m, type="III")
    
    df <- as.data.frame(w)
    df$Predictor <- rownames(df)
    df$Model <- name
    return(df)
  }
  return(NULL)
}

res1 <- run_multinom("good_taste_def_model_min10/document_topic_probabilities.csv", "Good Taste Def")
res2 <- run_multinom("bad_taste_def_model_min5/document_topic_probabilities.csv", "Bad Taste Def")

all_res <- bind_rows(res1, res2)
all_res <- all_res %>% select(Model, Predictor, `LR Chisq`, Df, `Pr(>Chisq)`)

latex_str <- "\\begin{table}[htpb]\n\\centering\n\\resizebox{\\textwidth}{!}{\n\\begin{tabular}{llrrr}\n\\toprule\n"
latex_str <- paste0(latex_str, "Model & Predictor & Wald $\\chi^2$ & Df & $p$-value \\\\\n\\midrule\n")
for (i in 1:nrow(all_res)) {
  pval <- all_res$`Pr(>Chisq)`[i]
  pval_str <- if(pval < 0.001) "< 0.001" else sprintf("%.4f", pval)
  latex_str <- paste0(latex_str, sprintf("%s & %s & %.2f & %d & %s \\\\\n", 
                                         all_res$Model[i], gsub("_", "\\\\_", all_res$Predictor[i]), all_res$`LR Chisq`[i], all_res$Df[i], pval_str))
}
latex_str <- paste0(latex_str, "\\bottomrule\n\\end{tabular}\n}\n\\caption{Multinomial Logistic Regression Wald Tests for Topic Predictors}\n\\label{tab:wald_models}\n\\end{table}\n")
writeLines(latex_str, "report/Tabs/wald_models.tex")


# 2. Pearson Residual Plots
good_def_probs <- read_csv("good_taste_def_model_min10/document_topic_probabilities.csv", show_col_types = FALSE)
bad_def_probs <- read_csv("bad_taste_def_model_min5/document_topic_probabilities.csv", show_col_types = FALSE)

get_primary <- function(probs, name) {
  topic_cols <- setdiff(names(probs), "original_index")
  probs$primary <- apply(probs[, topic_cols], 1, function(row) topic_cols[which.max(row)])
  res <- probs %>% select(original_index, primary)
  names(res)[2] <- name
  return(res)
}

gd <- get_primary(good_def_probs, "Good_Def")
bd <- get_primary(bad_def_probs, "Bad_Def")

df <- gd %>% inner_join(bd, by="original_index")
df <- df %>%
  filter(Good_Def != "-1", Bad_Def != "-1") %>%
  mutate(
    Good_Def = paste("Topic", Good_Def),
    Bad_Def = paste("Topic", Bad_Def)
  )

tb <- table(df$Good_Def, df$Bad_Def)
if (nrow(tb) > 1 && ncol(tb) > 1) {
  res <- chisq.test(tb)
  residuals_df <- as.data.frame(as.table(res$residuals))
  names(residuals_df) <- c("Var1", "Var2", "Residual")
  
  p <- ggplot(residuals_df, aes(x = Var1, y = Var2, fill = Residual)) +
    geom_tile(color = "white") +
    geom_text(aes(label = round(Residual, 2)), color = "black", size = 4) +
    scale_fill_gradient2(low = "indianred", mid = "white", high = "steelblue", midpoint = 0) +
    scale_x_discrete(position = "top") +
    theme_minimal(base_size = 12) +
    labs(
      title = "Pearson Residuals: Good Def vs Bad Def",
      x = "Good Taste Def",
      y = "Bad Taste Def",
      fill = "Residual"
    )
  ggsave("report/Plots/schema_corr_Good_Def_Bad_Def.png", plot = p, width = 8, height = 6)
}


# 3. PCA of Distinction Domains and Topic ANOVA
dist_cols <- grep("Domains_Distinction1_", names(raw_survey), value = TRUE)
dist_data <- raw_survey %>% select(all_of(dist_cols)) %>% mutate_all(as.numeric)
dist_data_imp <- as.data.frame(lapply(dist_data, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x)))
names(dist_data_imp) <- str_remove(names(dist_data_imp), "Domains_Distinction1_")

res.pca <- PCA(dist_data_imp, scale.unit = TRUE, graph = FALSE)
p_pca_var <- fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, title = "PCA of Cultural Domains Distinction Ratings")
ggsave("report/Plots/pca_domain_variables.png", plot = p_pca_var, width = 8, height = 6)

raw_survey$Dim1 <- res.pca$ind$coord[,1]
raw_survey$Dim2 <- res.pca$ind$coord[,2]
raw_survey$Dim3 <- res.pca$ind$coord[,3]

df_pca <- raw_survey %>%
  left_join(gd, by=c("python_index"="original_index")) %>%
  left_join(bd, by=c("python_index"="original_index"))

df_pca$Good_Def[df_pca$Good_Def == "-1"] <- NA
df_pca$Bad_Def[df_pca$Bad_Def == "-1"] <- NA

run_anova_for_dim <- function(dim_col) {
  results <- list()
  for (model_col in c("Good_Def", "Bad_Def")) {
    test_df <- df_pca %>% select(!!sym(model_col), !!sym(dim_col)) %>% drop_na()
    if (length(unique(test_df[[model_col]])) > 1) {
      formula <- as.formula(paste0("`", dim_col, "` ~ ", model_col))
      aov_res <- aov(formula, data = test_df)
      s <- summary(aov_res)[[1]]
      f_val <- s[["F value"]][1]
      p_val <- s[["Pr(>F)"]][1]
      results[[model_col]] <- data.frame(Model=model_col, F_val=f_val, p_val=p_val)
    }
  }
  res_df <- bind_rows(results)
  res_df$Dimension <- dim_col
  res_df
}

res_dim1 <- run_anova_for_dim("Dim1")
res_dim2 <- run_anova_for_dim("Dim2")
res_dim3 <- run_anova_for_dim("Dim3")

all_res_pca <- bind_rows(res_dim1, res_dim2, res_dim3)
all_res_pca$Format <- sprintf("%.2f (%.3f)", all_res_pca$F_val, all_res_pca$p_val)
all_res_pca$Format <- ifelse(all_res_pca$p_val < 0.001, paste0(all_res_pca$Format, "$^{***}$"), 
                  ifelse(all_res_pca$p_val < 0.01, paste0(all_res_pca$Format, "$^{**}$"),
                  ifelse(all_res_pca$p_val < 0.05, paste0(all_res_pca$Format, "$^{*}$"), all_res_pca$Format)))

wide_res <- all_res_pca %>% select(Dimension, Model, Format) %>%
  pivot_wider(names_from = Model, values_from = Format)

latex_str_pca <- "\\begin{table}[htpb]\n\\centering\n\\begin{tabular}{lcc}\n\\toprule\n"
latex_str_pca <- paste0(latex_str_pca, "Dimension & Good Taste Def & Bad Taste Def \\\\\n\\midrule\n")
for (i in 1:nrow(wide_res)) {
  latex_str_pca <- paste0(latex_str_pca, sprintf("%s & %s & %s \\\\\n", 
        wide_res$Dimension[i], wide_res$Good_Def[i], wide_res$Bad_Def[i]))
}
latex_str_pca <- paste0(latex_str_pca, "\\bottomrule\n\\multicolumn{3}{l}{\\footnotesize $^{*}p<0.05; ^{**}p<0.01; ^{***}p<0.001$} \\\\\n")
latex_str_pca <- paste0(latex_str_pca, "\\end{tabular}\n\\caption{ANOVA Results: Mean Differences in PCA Dimensions of Distinction Ratings by Topic Schema.}\n\\label{tab:anova_pca}\n\\end{table}\n")

writeLines(latex_str_pca, "report/Tabs/anova_pca.tex")

print("R script executed successfully.")
