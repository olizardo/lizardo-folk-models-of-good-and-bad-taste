library(dplyr)
library(tidyr)
library(readr)
library(nnet)
library(car)

# Load data
raw_survey <- read_csv("data/FolkTaste_CleanData.csv", show_col_types = FALSE)
survey_df <- raw_survey[-1, ] # drop question row
valid_idx <- which(!is.na(survey_df$GoodTaste_Def))
analysis_df <- survey_df[valid_idx, ]

# Load probabilities
good_probs <- read_csv("good_taste_model_min10/document_topic_probabilities.csv", show_col_types = FALSE)
bad_probs <- read_csv("bad_taste_model_min15/document_topic_probabilities.csv", show_col_types = FALSE)

# Assign hard clusters
analysis_df$GoodTopic_Id <- apply(good_probs, 1, which.max) - 1
analysis_df$BadTopic_Id <- apply(bad_probs, 1, which.max) - 1

good_labels <- c("0: Mainstream/Consensus", "1: Aesthetics/Nuance", "2: Moral/Behavioral")
bad_labels <- c("0: Personal Disagreement", "1: Low Quality/Materialist", "2: Loud/Sophomoric/Offensive")

analysis_df$GoodTasteTopic <- factor(analysis_df$GoodTopic_Id, labels = good_labels)
analysis_df$BadTasteTopic <- factor(analysis_df$BadTopic_Id, labels = bad_labels)

analysis_df$Age <- as.numeric(analysis_df$Age)
analysis_df$EducationLevel_Coded <- as.numeric(analysis_df$EducationLevel_Coded)
analysis_df$Political <- as.numeric(analysis_df$Political)

model_data <- analysis_df %>% filter(!is.na(Age), !is.na(EducationLevel_Coded), !is.na(Political), Gender %in% c("A man", "A woman"))

m_good <- multinom(GoodTasteTopic ~ Age + Gender + EducationLevel_Coded + Political, data = model_data, Hess = TRUE, trace = FALSE)
m_bad <- multinom(BadTasteTopic ~ Age + Gender + EducationLevel_Coded + Political, data = model_data, Hess = TRUE, trace = FALSE)

wald_good <- car::Anova(m_good, type = "II", test.statistic = "Wald")
wald_bad <- car::Anova(m_bad, type = "II", test.statistic = "Wald")

# Format output as markdown table
cat("### Good Taste Model Predictors\n\n")
cat("| Predictor | Wald Chisq | Df | Pr(>Chisq) |\n")
cat("|---|---|---|---|\n")
for(i in 1:nrow(wald_good)) {
  cat(sprintf("| %s | %.3f | %d | %.4f |\n", rownames(wald_good)[i], wald_good$`LR Chisq`[i], wald_good$Df[i], wald_good$`Pr(>Chisq)`[i]))
}

cat("\n### Bad Taste Model Predictors\n\n")
cat("| Predictor | Wald Chisq | Df | Pr(>Chisq) |\n")
cat("|---|---|---|---|\n")
for(i in 1:nrow(wald_bad)) {
  cat(sprintf("| %s | %.3f | %d | %.4f |\n", rownames(wald_bad)[i], wald_bad$`LR Chisq`[i], wald_bad$Df[i], wald_bad$`Pr(>Chisq)`[i]))
}
