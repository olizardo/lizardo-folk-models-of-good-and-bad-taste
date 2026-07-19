library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(nnet)
library(jsonlite)
library(ggeffects)

config <- fromJSON("config.json")
good_model_dir <- config$good_model
bad_model_dir <- config$bad_model

raw_survey <- read_csv("data/FolkTaste_CleanData.csv", show_col_types = FALSE)
if(grepl("What do you mean", raw_survey$GoodTaste_Def[1])) raw_survey <- raw_survey[-1, ]
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
to_impute_idx <- imp_data$python_index[is.na(imp_data$Age)]
pred_ages <- predict(age_mod, newdata = imp_data[is.na(imp_data$Age), ])

for (i in seq_along(to_impute_idx)) {
  raw_survey$Age[raw_survey$python_index == to_impute_idx[i]] <- pred_ages[i]
}

get_topic_names <- function(model_dir) {
  df <- read_csv(file.path(model_dir, "topic_info.csv"), show_col_types=FALSE)
  res <- df$Name
  names(res) <- as.character(df$Topic)
  return(res)
}

good_names <- get_topic_names(good_model_dir)
bad_names <- get_topic_names(bad_model_dir)

format_name <- function(n) {
  parts <- strsplit(n, "_")[[1]]
  paste(tools::toTitleCase(parts[-1]), collapse=" ")
}

run_and_plot <- function(prob_file, topic_names, title_prefix, file_prefix) {
  probs <- read_csv(prob_file, show_col_types = FALSE)
  topic_cols <- setdiff(names(probs), "original_index")
  probs$primary_topic <- apply(probs[, topic_cols], 1, function(row) topic_cols[which.max(row)])
  probs_valid <- probs %>% filter(!primary_topic %in% c("-1"))
  
  merged_df <- merge(raw_survey, probs_valid, by.x="python_index", by.y="original_index")
  
  merged_df$topic_label <- sapply(as.character(merged_df$primary_topic), function(x) format_name(topic_names[x]))
  merged_df$topic_label <- as.factor(merged_df$topic_label)
  
  model_data <- merged_df %>% drop_na(Age, Gender, EducationLevel_Coded, ParentEducation_Coded, Political) %>% filter(Gender %in% c("A man", "A woman"))
  model_data$Gender <- as.factor(model_data$Gender)
  
  m <- multinom(topic_label ~ poly(Age, 2) + Gender + EducationLevel_Coded + ParentEducation_Coded + Political, data=model_data, trace=FALSE)
  
  # Predict Age
  eff_age <- predict_response(m, terms = "Age [all]")
  p_age <- plot(eff_age) + labs(title = paste("Predicted Probabilities by Age:", title_prefix), x = "Age", y = "Predicted Probability") + theme_minimal()
  ggsave(paste0("report/Plots/", file_prefix, "_age_eff.png"), plot = p_age, width = 8, height = 6)
  
  # Predict Gender
  eff_gender <- predict_response(m, terms = "Gender")
  p_gen <- plot(eff_gender) + labs(title = paste("Predicted Probabilities by Gender:", title_prefix), x = "Gender", y = "Predicted Probability") + theme_minimal()
  ggsave(paste0("report/Plots/", file_prefix, "_gender_eff.png"), plot = p_gen, width = 8, height = 6)
  
  # Predict EducationLevel_Coded
  eff_edu <- predict_response(m, terms = "EducationLevel_Coded")
  p_edu <- plot(eff_edu) + labs(title = paste("Predicted Probabilities by Education:", title_prefix), x = "Education Level (Coded)", y = "Predicted Probability") + theme_minimal()
  ggsave(paste0("report/Plots/", file_prefix, "_educ_eff.png"), plot = p_edu, width = 8, height = 6)
  
  # Predict ParentEducation_Coded
  eff_pedu <- predict_response(m, terms = "ParentEducation_Coded")
  p_pedu <- plot(eff_pedu) + labs(title = paste("Predicted Probabilities by Parent's Ed:", title_prefix), x = "Parent's Education Level (Coded)", y = "Predicted Probability") + theme_minimal()
  ggsave(paste0("report/Plots/", file_prefix, "_peduc_eff.png"), plot = p_pedu, width = 8, height = 6)
  
  # Predict Political
  eff_pol <- predict_response(m, terms = "Political [all]")
  p_pol <- plot(eff_pol) + labs(title = paste("Predicted Probabilities by Politics:", title_prefix), x = "Political Orientation (1=Liberal, 7=Conservative)", y = "Predicted Probability") + theme_minimal()
  ggsave(paste0("report/Plots/", file_prefix, "_pol_eff.png"), plot = p_pol, width = 8, height = 6)
}

run_and_plot(file.path(good_model_dir, "document_topic_probabilities.csv"), good_names, "Good Taste", "good_taste")
run_and_plot(file.path(bad_model_dir, "document_topic_probabilities.csv"), bad_names, "Bad Taste", "bad_taste")
print("Plots generated successfully!")
