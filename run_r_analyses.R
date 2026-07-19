
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

config <- fromJSON("config.json")
good_model_dir <- config$good_model
bad_model_dir <- config$bad_model

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

res1 <- run_multinom(file.path(good_model_dir, "document_topic_probabilities.csv"), "Good Taste Def")
res2 <- run_multinom(file.path(bad_model_dir, "document_topic_probabilities.csv"), "Bad Taste Def")

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
good_def_probs <- read_csv(file.path(good_model_dir, "document_topic_probabilities.csv"), show_col_types = FALSE)
bad_def_probs <- read_csv(file.path(bad_model_dir, "document_topic_probabilities.csv"), show_col_types = FALSE)

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


run_pca_and_manova <- function(var_prefix, title_prefix, file_suffix) {
  # 1. Prepare Data
  dist_cols <- grep(paste0("^", var_prefix), names(raw_survey), value = TRUE)
  if(length(dist_cols) == 0) return(NULL)
  
  dist_data <- raw_survey %>% select(all_of(dist_cols)) %>% mutate_all(as.numeric)
  dist_data_imp <- as.data.frame(lapply(dist_data, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x)))
  names(dist_data_imp) <- str_remove(names(dist_data_imp), var_prefix)
  
  # 2. Run PCA
  res.pca <- PCA(dist_data_imp, scale.unit = TRUE, graph = FALSE)
  p_pca_var <- fviz_pca_var(res.pca, col.var = "contrib",
               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
               repel = TRUE, title = paste("PCA of", title_prefix, "Ratings"))
  ggsave(paste0("report/Plots/pca_", file_suffix, ".png"), plot = p_pca_var, width = 8, height = 6)
  
  # 3. Add Dimensions to survey
  raw_survey$Dim1 <- res.pca$ind$coord[,1]
  raw_survey$Dim2 <- res.pca$ind$coord[,2]
  raw_survey$Dim3 <- res.pca$ind$coord[,3]
  
  # 4. Correlation Plot
  pca_cors <- cor(dist_data_imp, raw_survey[, c("Dim1", "Dim2", "Dim3")], use = "pairwise.complete.obs")
  png(paste0("report/Plots/pca_correlations_", file_suffix, ".png"), width = 800, height = 1000, res=120)
  corrplot(pca_cors, method="color", addCoef.col = "black", tl.col="black", tl.srt=45,
           number.cex=0.7, title=paste("Correlations:", title_prefix, "and PCA Dims"), mar=c(0,0,2,0))
  dev.off()
  
  # 5. Join with Topic Schemas
  df_pca <- raw_survey %>%
    left_join(gd, by=c("python_index"="original_index")) %>%
    left_join(bd, by=c("python_index"="original_index"))
  
  df_pca$Good_Def[df_pca$Good_Def == "-1"] <- NA
  df_pca$Bad_Def[df_pca$Bad_Def == "-1"] <- NA
  
  # 6. MANOVA
  run_manova <- function(model_col) {
    test_df <- df_pca %>% select(Dim1, Dim2, Dim3, !!sym(model_col)) %>% drop_na()
    if (length(unique(test_df[[model_col]])) > 1) {
      Y <- cbind(test_df$Dim1, test_df$Dim2, test_df$Dim3)
      formula <- as.formula(paste0("Y ~ ", model_col))
      fit <- manova(formula, data = test_df)
      s <- summary(fit, test = "Pillai")
      
      pillai <- s$stats[1, "Pillai"]
      f_val <- s$stats[1, "approx F"]
      p_val <- s$stats[1, "Pr(>F)"]
      df1 <- s$stats[1, "num Df"]
      df2 <- s$stats[1, "den Df"]
      data.frame(Model=model_col, Pillai=pillai, F_val=f_val, df1=df1, df2=df2, p_val=p_val)
    }
  }
  
  res_good <- run_manova("Good_Def")
  res_bad <- run_manova("Bad_Def")
  
  all_res_pca <- bind_rows(res_good, res_bad)
  all_res_pca$Format_F <- sprintf("%.2f", all_res_pca$F_val)
  all_res_pca$Format_p <- sprintf("%.3f", all_res_pca$p_val)
  all_res_pca$Format_p <- ifelse(all_res_pca$p_val < 0.001, paste0(all_res_pca$Format_p, "$^{***}$"), 
                    ifelse(all_res_pca$p_val < 0.01, paste0(all_res_pca$Format_p, "$^{**}$"),
                    ifelse(all_res_pca$p_val < 0.05, paste0(all_res_pca$Format_p, "$^{*}$"), all_res_pca$Format_p)))
  
  # 7. Write Latex Table
  latex_str_pca <- "\\begin{table}[htpb]\n\\centering\n\\begin{tabular}{lcccccc}\n\\toprule\n"
  latex_str_pca <- paste0(latex_str_pca, "Predictor & Pillai's Trace & approx $F$ & num Df & den Df & $p$-value \\\\\n\\midrule\n")
  for (i in 1:nrow(all_res_pca)) {
    m_name <- ifelse(all_res_pca$Model[i] == "Good_Def", "Good Taste Def", "Bad Taste Def")
    latex_str_pca <- paste0(latex_str_pca, sprintf("%s & %.3f & %s & %d & %d & %s \\\\\n", 
          m_name, all_res_pca$Pillai[i], all_res_pca$Format_F[i], all_res_pca$df1[i], all_res_pca$df2[i], all_res_pca$Format_p[i]))
  }
  latex_str_pca <- paste0(latex_str_pca, "\\bottomrule\n\\multicolumn{6}{l}{\\footnotesize $^{*}p<0.05; ^{**}p<0.01; ^{***}p<0.001$} \\\\\n")
  latex_str_pca <- paste0(latex_str_pca, "\\end{tabular}\n\\caption{MANOVA Results: Differences in PCA Dimensions (1-3) of ", title_prefix, " Ratings by Taste Cultural Model.}\n\\label{tab:anova_pca_", file_suffix, "}\n\\end{table}\n")
  
  writeLines(latex_str_pca, paste0("report/Tabs/anova_pca_", file_suffix, ".tex"))
}

# Run for all three sets of variables
run_pca_and_manova("Domains_Distinction1_", "Cultural Domains Distinction 1", "distinction1")
run_pca_and_manova("Domains_Distinction2_", "Cultural Domains Distinction 2", "distinction2")
run_pca_and_manova("Importance_", "Cultural Domains Importance", "importance")
run_pca_and_manova("Knowledge_", "Cultural Domains Knowledge", "knowledge")

print("R script executed successfully.")
