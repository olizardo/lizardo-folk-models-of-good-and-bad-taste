library(dplyr)
library(tidyr)
library(nnet)
library(car)

survey_df <- read.csv("data/FolkTaste_CleanData.csv")
if (grepl("What do you mean", survey_df$GoodTaste_Def[1])) {
  survey_df <- survey_df[-1, ]
}
survey_df <- survey_df %>% filter(Taste_Possibility == "YES")

survey_df$Age <- as.numeric(survey_df$Age)
survey_df$EducationLevel_Coded <- as.numeric(survey_df$EducationLevel_Coded)
survey_df$Political <- as.numeric(survey_df$Political)

analyze_model <- function(prob_file, name) {
  cat("\n==========================================\n")
  cat("Analyzing: ", name, "\n")
  
  probs <- read.csv(prob_file)
  
  topic_cols <- setdiff(names(probs), "original_index")
  probs$primary_topic <- apply(probs[, topic_cols], 1, function(row) {
    topic_cols[which.max(row)]
  })
  
  probs_valid <- probs %>% filter(!primary_topic %in% c("-1", "X.1"))
  
  survey_df$python_index <- 0:(nrow(survey_df)-1)
  
  merged_df <- merge(survey_df, probs_valid, by.x="python_index", by.y="original_index")
  merged_df$primary_topic <- as.factor(merged_df$primary_topic)
  
  model_data <- merged_df %>% drop_na(Age, Gender, EducationLevel_Coded, Political)
  model_data <- model_data %>% filter(Gender %in% c("Male", "Female"))
  
  cat("Valid N for modeling: ", nrow(model_data), "\n")
  cat("Topic distribution:\n")
  print(table(model_data$primary_topic))
  
  if (length(unique(model_data$primary_topic)) > 1) {
    # Set the largest group as reference
    model_data$primary_topic <- relevel(model_data$primary_topic, ref=names(sort(table(model_data$primary_topic), decreasing=TRUE)[1]))
    
    m <- multinom(primary_topic ~ Age + Gender + EducationLevel_Coded + Political, data=model_data, trace=FALSE)
    cat("\nWald Tests:\n")
    print(Anova(m, type="III"))
  } else {
    cat("Only one topic left after filtering outliers/missing data.\n")
  }
}

analyze_model("good_taste_def_model_min10/document_topic_probabilities.csv", "Good Taste Def")
analyze_model("bad_taste_def_model_min5/document_topic_probabilities.csv", "Bad Taste Def")
analyze_model("good_taste_example_model_min5/document_topic_probabilities.csv", "Good Taste Example")
analyze_model("bad_taste_example_model_min10/document_topic_probabilities.csv", "Bad Taste Example")
