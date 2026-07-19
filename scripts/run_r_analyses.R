
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
raw_survey$python_index <- 0:(nrow(raw_survey)-1)

raw_survey$Age <- as.numeric(raw_survey$Age)
raw_survey$EducationLevel_Coded <- as.factor(raw_survey$EducationLevel_Coded)
raw_survey$ParentEducation_Coded <- as.factor(raw_survey$ParentEducation_Coded)
raw_survey$Political <- as.numeric(raw_survey$Political)

# Impute unrealistic ages using a linear model based on other demographics
imp_data <- raw_survey %>% select(python_index, Age, Gender, EducationLevel_Coded, ParentEducation_Coded, Political)
imp_data$Age[imp_data$Age < 18] <- NA
age_mod <- lm(Age ~ Gender + EducationLevel_Coded + ParentEducation_Coded + Political, data = imp_data)
# get the python_indices of the rows that need imputation
to_impute_idx <- imp_data$python_index[is.na(imp_data$Age)]
pred_ages <- predict(age_mod, newdata = imp_data[is.na(imp_data$Age), ])

# match the predictions to the rows safely
for (i in seq_along(to_impute_idx)) {
  raw_survey$Age[raw_survey$python_index == to_impute_idx[i]] <- pred_ages[i]
}


# ADD python_index globally since it's used multiple times
# (already added above)

run_multinom <- function(prob_file, name) {
  probs <- read_csv(prob_file, show_col_types = FALSE)
  topic_cols <- setdiff(names(probs), "original_index")
  probs$primary_topic <- apply(probs[, topic_cols], 1, function(row) topic_cols[which.max(row)])
  
  probs_valid <- probs %>% filter(!primary_topic %in% c("-1"))
  
  merged_df <- merge(raw_survey, probs_valid, by.x="python_index", by.y="original_index")
  merged_df$primary_topic <- as.factor(merged_df$primary_topic)
  
  model_data <- merged_df %>% drop_na(Age, Gender, EducationLevel_Coded, ParentEducation_Coded, Political) %>% filter(Gender %in% c("A man", "A woman"))
  
  if (length(unique(model_data$primary_topic)) > 1) {
    model_data$primary_topic <- relevel(model_data$primary_topic, ref=names(sort(table(model_data$primary_topic), decreasing=TRUE)[1]))
    m <- multinom(primary_topic ~ poly(Age, 2) + Gender + EducationLevel_Coded + ParentEducation_Coded + Political, data=model_data, trace=FALSE)
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
    geom_text(aes(label = round(Residual, 2)), color = "black", size = 4, fontface="bold") +
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
  ggsave("report/Plots/schema_corr_Good_Def_Bad_Def.png", plot = p, width = 8, height = 6)
}


# Run for domain counts (called from external script now)
# (PCA and MANOVA were removed in favor of a count-based ANOVA)


print("R script executed successfully.")
