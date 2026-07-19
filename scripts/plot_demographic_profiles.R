library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(stringr)
library(jsonlite)

config <- fromJSON("config.json")
good_model_dir <- config$good_model
bad_model_dir <- config$bad_model

raw_survey <- read_csv("data/FolkTaste_CleanData.csv", show_col_types = FALSE)
if(grepl("What do you mean", raw_survey$GoodTaste_Def[1])) raw_survey <- raw_survey[-1, ]
raw_survey <- raw_survey %>% filter(Taste_Possibility == "YES")
raw_survey$python_index <- 0:(nrow(raw_survey)-1)

raw_survey$Age <- as.numeric(raw_survey$Age)
raw_survey$Political <- as.numeric(raw_survey$Political)
raw_survey$EducationLevel_Coded <- as.numeric(raw_survey$EducationLevel_Coded)
raw_survey$ParentEducation_Coded <- as.numeric(raw_survey$ParentEducation_Coded)

# Impute unrealistic ages using a linear model based on other demographics
imp_data <- raw_survey %>% select(python_index, Age, Gender, EducationLevel_Coded, ParentEducation_Coded, Political)
imp_data$Age[imp_data$Age < 18] <- NA
age_mod <- lm(Age ~ Gender + EducationLevel_Coded + ParentEducation_Coded + Political, data = imp_data)
to_impute_idx <- imp_data$python_index[is.na(imp_data$Age)]
pred_ages <- predict(age_mod, newdata = imp_data[is.na(imp_data$Age), ])

for (i in seq_along(to_impute_idx)) {
  raw_survey$Age[raw_survey$python_index == to_impute_idx[i]] <- pred_ages[i]
}

# Create binary indicators
raw_survey <- raw_survey %>%
  mutate(
    is_older = ifelse(Age > 45, 1, 0),
    is_woman = ifelse(Gender == "A woman", 1, ifelse(Gender == "A man", 0, NA)),
    is_ba = ifelse(EducationLevel_Coded >= 4, 1, 0),
    is_parent_ba = ifelse(ParentEducation_Coded >= 4, 1, 0),
    is_liberal = ifelse(Political < 3, 1, 0),
    is_conservative = ifelse(Political > 5, 1, 0)
  )

# Get primary topics
get_primary <- function(model_dir) {
  probs <- read_csv(file.path(model_dir, "document_topic_probabilities.csv"), show_col_types = FALSE)
  topic_cols <- setdiff(names(probs), "original_index")
  probs$primary <- apply(probs[, topic_cols], 1, function(row) topic_cols[which.max(row)])
  
  df_info <- read_csv(file.path(model_dir, "topic_info.csv"), show_col_types=FALSE)
  res_names <- df_info$Name
  names(res_names) <- as.character(df_info$Topic)
  
  probs$topic_name <- sapply(as.character(probs$primary), function(x) {
    if(x == "-1") return("-1")
    parts <- strsplit(res_names[x], "_")[[1]]
    paste(tools::toTitleCase(parts[-1]), collapse=" ")
  })
  
  probs %>% select(original_index, primary, topic_name)
}

gd <- get_primary(good_model_dir) %>% rename(Good_Topic = topic_name)
bd <- get_primary(bad_model_dir) %>% rename(Bad_Topic = topic_name)

df_all <- raw_survey %>%
  left_join(gd, by=c("python_index"="original_index")) %>%
  left_join(bd, by=c("python_index"="original_index")) %>%
  filter(!is.na(is_woman), Good_Topic != "-1", Bad_Topic != "-1")

# Standardize variables at the sample level for Z-scores
df_all <- df_all %>%
  mutate(
    z_older = scale(is_older)[,1],
    z_woman = scale(is_woman)[,1],
    z_ba = scale(is_ba)[,1],
    z_pba = scale(is_parent_ba)[,1],
    z_lib = scale(is_liberal)[,1],
    z_con = scale(is_conservative)[,1]
  )

plot_profile_heatmap <- function(topic_col, title_prefix, file_prefix) {
  agg <- df_all %>%
    group_by(!!sym(topic_col)) %>%
    summarise(
      n = n(),
      pct_older = mean(is_older, na.rm=TRUE) * 100,
      pct_woman = mean(is_woman, na.rm=TRUE) * 100,
      pct_ba = mean(is_ba, na.rm=TRUE) * 100,
      pct_pba = mean(is_parent_ba, na.rm=TRUE) * 100,
      pct_lib = mean(is_liberal, na.rm=TRUE) * 100,
      pct_con = mean(is_conservative, na.rm=TRUE) * 100,
      
      z_older = mean(z_older, na.rm=TRUE),
      z_woman = mean(z_woman, na.rm=TRUE),
      z_ba = mean(z_ba, na.rm=TRUE),
      z_pba = mean(z_pba, na.rm=TRUE),
      z_lib = mean(z_lib, na.rm=TRUE),
      z_con = mean(z_con, na.rm=TRUE),
      
      se_z_older = sd(z_older, na.rm=TRUE)/sqrt(n),
      se_z_woman = sd(z_woman, na.rm=TRUE)/sqrt(n),
      se_z_ba = sd(z_ba, na.rm=TRUE)/sqrt(n),
      se_z_pba = sd(z_pba, na.rm=TRUE)/sqrt(n),
      se_z_lib = sd(z_lib, na.rm=TRUE)/sqrt(n),
      se_z_con = sd(z_con, na.rm=TRUE)/sqrt(n),
      
      .groups="drop"
    )
    
  # Pivot to long format
  long_z <- agg %>%
    select(!!sym(topic_col), starts_with("z_")) %>%
    pivot_longer(cols = -!!sym(topic_col), names_to = "Variable", values_to = "Z_Score")
    
  long_raw <- agg %>%
    select(!!sym(topic_col), pct_older, pct_woman, pct_ba, pct_pba, pct_lib, pct_con) %>%
    pivot_longer(cols = -!!sym(topic_col), names_to = "Raw_Var", values_to = "Raw_Value") %>%
    mutate(
      Variable = case_when(
        Raw_Var == "pct_older" ~ "z_older",
        Raw_Var == "pct_woman" ~ "z_woman",
        Raw_Var == "pct_ba" ~ "z_ba",
        Raw_Var == "pct_pba" ~ "z_pba",
        Raw_Var == "pct_lib" ~ "z_lib",
        Raw_Var == "pct_con" ~ "z_con"
      ),
      Label = case_when(
        Raw_Var == "pct_older" ~ sprintf("%.0f%%", Raw_Value),
        Raw_Var == "pct_woman" ~ sprintf("%.0f%%", Raw_Value),
        Raw_Var == "pct_ba" ~ sprintf("%.0f%%", Raw_Value),
        Raw_Var == "pct_pba" ~ sprintf("%.0f%%", Raw_Value),
        Raw_Var == "pct_lib" ~ sprintf("%.0f%%", Raw_Value),
        Raw_Var == "pct_con" ~ sprintf("%.0f%%", Raw_Value)
      )
    )
    
  long_se <- agg %>%
    select(!!sym(topic_col), starts_with("se_z_")) %>%
    pivot_longer(cols = -!!sym(topic_col), names_to = "SE_Var", values_to = "SE_Value") %>%
    mutate(Variable = str_replace(SE_Var, "se_", ""))
    
  plot_data <- long_raw %>%
    left_join(long_z %>% select(!!sym(topic_col), Variable, Z_Score), by = c(quo_name(topic_col), "Variable")) %>%
    left_join(long_se %>% select(!!sym(topic_col), Variable, SE_Value), by = c(quo_name(topic_col), "Variable")) %>%
    left_join(agg %>% select(!!sym(topic_col), n), by = quo_name(topic_col)) %>%
    mutate(
      is_sig = (abs(Z_Score) - (1.96 * SE_Value)) > 0,
      is_sig = ifelse(is.na(is_sig), FALSE, is_sig),
      Label = ifelse(is_sig, paste0(Label, "*"), Label)
    )
    
  # Clean up Variable names for plotting
  plot_data$Variable <- case_when(
    plot_data$Variable == "z_older" ~ "Age (% Over 45)",
    plot_data$Variable == "z_woman" ~ "Gender (% Woman)",
    plot_data$Variable == "z_ba" ~ "Education (% BA+)",
    plot_data$Variable == "z_pba" ~ "Parent Educ (% BA+)",
    plot_data$Variable == "z_lib" ~ "Politics (% Liberal)",
    plot_data$Variable == "z_con" ~ "Politics (% Cons)"
  )
  
  plot_data$Variable <- factor(plot_data$Variable, levels=c("Age (% Over 45)", "Gender (% Woman)", "Education (% BA+)", "Parent Educ (% BA+)", "Politics (% Liberal)", "Politics (% Cons)"))
  
  plot_data <- plot_data %>% filter(Variable != "Politics (% Cons)")
  
  plot_data$LabelWithN <- sprintf("%s\n(n=%d)", plot_data$Label, plot_data$n)
  
  p <- ggplot(plot_data, aes(x = Variable, y = !!sym(topic_col), fill = Raw_Value)) +
    geom_tile(color = "white") +
    geom_text(aes(label = LabelWithN), color = "black", size = 4.5, fontface = "bold") +
    scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 50, limits = c(0, 100), name="Percentage") +
    theme_minimal(base_size = 14) +
    labs(title = paste("Demographic Profile:", title_prefix), x = "", y = "") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
          axis.text.y = element_text(face = "bold"))
          
  ggsave(paste0("report/Plots/", file_prefix, "_demo_profile.png"), plot = p, width = 10, height = 6)
}

plot_profile_heatmap("Good_Topic", "Good Taste Cultural Models", "good_taste")
plot_profile_heatmap("Bad_Topic", "Bad Taste Cultural Models", "bad_taste")
print("Profile heatmaps generated successfully!")