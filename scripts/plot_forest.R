library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(nnet)
library(jsonlite)
library(broom)

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
  
  # Set reference level to most frequent class
  model_data$topic_label <- relevel(model_data$topic_label, ref=names(sort(table(model_data$topic_label), decreasing=TRUE)[1]))
  
  m <- multinom(topic_label ~ poly(Age, 2) + Gender + EducationLevel_Coded + ParentEducation_Coded + Political, data=model_data, trace=FALSE)
  
  tidied <- tidy(m, conf.int = TRUE)
  
  # Clean up labels
  tidied <- tidied %>% filter(term != "(Intercept)")
  tidied$term <- gsub("EducationLevel_Coded", "Educ: ", tidied$term)
  tidied$term <- gsub("ParentEducation_Coded", "Parent Educ: ", tidied$term)
  tidied$term <- gsub("GenderA woman", "Gender: Woman", tidied$term)
  tidied$term <- gsub("poly\\(Age, 2\\)1", "Age (Poly 1)", tidied$term)
  tidied$term <- gsub("poly\\(Age, 2\\)2", "Age (Poly 2)", tidied$term)
  
  # Reverse order of terms for better y-axis plotting
  tidied$term <- factor(tidied$term, levels = rev(unique(tidied$term)))
  
  p <- ggplot(tidied, aes(x = estimate, y = term, xmin = conf.low, xmax = conf.high, color = y.level)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "darkgray", size = 1) +
    geom_pointrange(position = position_dodge(width = 0.5), size = 0.6) +
    facet_wrap(~ y.level, ncol = 1) +
    labs(title = paste("Multinomial Logit Coefficients:", title_prefix),
         subtitle = paste("Reference Category:", levels(model_data$topic_label)[1]),
         x = "Log-Odds Estimate (95% CI)", 
         y = "", 
         color = "Topic") +
    theme_minimal(base_size = 14) +
    theme(legend.position = "none",
          strip.text = element_text(face = "bold", size = 12))
          
  ggsave(paste0("report/Plots/", file_prefix, "_forest.png"), plot = p, width = 9, height = 9)
}

run_and_plot(file.path(good_model_dir, "document_topic_probabilities.csv"), good_names, "Good Taste", "good_taste")
run_and_plot(file.path(bad_model_dir, "document_topic_probabilities.csv"), bad_names, "Bad Taste", "bad_taste")
print("Forest plots generated successfully!")
