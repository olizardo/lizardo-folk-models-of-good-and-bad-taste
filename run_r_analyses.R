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
  
  probs_valid <- probs %>% filter(!primary_topic %in% c("X.1", "-1"))
  
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

analyze_model("good_taste_def_model_min5/document_topic_probabilities.csv", "Good Taste Def")
analyze_model("bad_taste_def_model_min15/document_topic_probabilities.csv", "Bad Taste Def")
analyze_model("good_taste_example_model_min10/document_topic_probabilities.csv", "Good Taste Example")
analyze_model("bad_taste_example_model_min10/document_topic_probabilities.csv", "Bad Taste Example")

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

good_labels <- paste("Topic", 0:(length(levels(factor(analysis_df$GoodTopic_Id)))-1))
bad_labels <- paste("Topic", 0:(length(levels(factor(analysis_df$BadTopic_Id)))-1))

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

library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(jsonlite)
library(tidyr)
library(tidytext)
library(FactoMineR)
library(factoextra)
library(nnet)
library(car)

dir.create("report/Plots", showWarnings = FALSE)
dir.create("report/Tabs", showWarnings = FALSE)

# 1. Topic Frequencies for Examples
good_ex_info <- read_csv("good_taste_example_model_min10/topic_info.csv", show_col_types = FALSE)
bad_ex_info <- read_csv("bad_taste_example_model_min10/topic_info.csv", show_col_types = FALSE)

clean_keywords <- function(rep_str) {
  str_remove_all(rep_str, "\\[|\\]|'")
}
good_ex_info$Keywords <- sapply(good_ex_info$Representation, clean_keywords)
bad_ex_info$Keywords <- sapply(bad_ex_info$Representation, clean_keywords)

p_good_ex <- ggplot(good_ex_info %>% filter(Topic != -1), aes(x = reorder(Name, Count), y = Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  theme_minimal(base_size = 14) +
  labs(title = "Good Taste Examples: Topic Frequencies", x = "Topic Name", y = "Count")
ggsave("report/Plots/good_taste_example_topics.png", plot = p_good_ex, width = 8, height = 6)

p_bad_ex <- ggplot(bad_ex_info %>% filter(Topic != -1), aes(x = reorder(Name, Count), y = Count)) +
  geom_bar(stat = "identity", fill = "indianred") +
  coord_flip() +
  theme_minimal(base_size = 14) +
  labs(title = "Bad Taste Examples: Topic Frequencies", x = "Topic Name", y = "Count")
ggsave("report/Plots/bad_taste_example_topics.png", plot = p_bad_ex, width = 8, height = 6)


# 2. Keyword Importance for Examples
extract_keyword_weights <- function(json_path) {
  topics_data <- fromJSON(json_path)$topic_representations
  topics_data[["-1"]] <- NULL
  df_list <- lapply(names(topics_data), function(topic_id) {
    mat <- topics_data[[topic_id]]
    data.frame(Topic = paste("Topic", topic_id), Word = as.character(mat[,1]), Score = as.numeric(mat[,2]), stringsAsFactors = FALSE)
  })
  do.call(rbind, df_list)
}

good_ex_weights <- extract_keyword_weights("good_taste_example_model_min10/topics.json")
p_good_ex_keys <- good_ex_weights %>%
  group_by(Topic) %>% top_n(8, Score) %>% ungroup() %>%
  mutate(Word = reorder_within(Word, Score, Topic)) %>%
  ggplot(aes(x = Word, y = Score, fill = Topic)) +
  geom_col(show.legend = FALSE) + facet_wrap(~ Topic, scales = "free_y") +
  coord_flip() + scale_x_reordered() + theme_minimal() +
  labs(title = "Good Taste Examples: Keyword Importance", x = NULL, y = "c-TF-IDF Score")
ggsave("report/Plots/good_taste_example_keywords.png", plot = p_good_ex_keys, width = 10, height = 8)

bad_ex_weights <- extract_keyword_weights("bad_taste_example_model_min10/topics.json")
p_bad_ex_keys <- bad_ex_weights %>%
  group_by(Topic) %>% top_n(8, Score) %>% ungroup() %>%
  mutate(Word = reorder_within(Word, Score, Topic)) %>%
  ggplot(aes(x = Word, y = Score, fill = Topic)) +
  geom_col(show.legend = FALSE) + facet_wrap(~ Topic, scales = "free_y") +
  coord_flip() + scale_x_reordered() + theme_minimal() +
  labs(title = "Bad Taste Examples: Keyword Importance", x = NULL, y = "c-TF-IDF Score")
ggsave("report/Plots/bad_taste_example_keywords.png", plot = p_bad_ex_keys, width = 10, height = 8)


# 3. CA Plots for Examples
good_ex_probs <- read_csv("good_taste_example_model_min10/document_topic_probabilities.csv", show_col_types = FALSE)
bad_ex_probs <- read_csv("bad_taste_example_model_min10/document_topic_probabilities.csv", show_col_types = FALSE)

good_ex_probs_clean <- good_ex_probs %>% select(-original_index)
bad_ex_probs_clean <- bad_ex_probs %>% select(-original_index)

colnames(good_ex_probs_clean) <- paste("Topic", 0:(ncol(good_ex_probs_clean)-1))
colnames(bad_ex_probs_clean) <- paste("Topic", 0:(ncol(bad_ex_probs_clean)-1))

good_ex_ca_data <- good_ex_probs_clean + 1e-6
bad_ex_ca_data <- bad_ex_probs_clean + 1e-6

good_ex_assigned <- as.factor(paste("Topic", apply(good_ex_probs_clean, 1, which.max) - 1))
bad_ex_assigned <- as.factor(paste("Topic", apply(bad_ex_probs_clean, 1, which.max) - 1))

res.ca.good.ex <- CA(good_ex_ca_data, graph = FALSE)
p_ca_good_ex <- fviz_ca_biplot(res.ca.good.ex, geom.row = "point", col.row = good_ex_assigned, col.col = "black", 
                               title = "CA: Good Taste Example Schemas", palette = "jco", legend.title = "Assigned Topic")
ggsave("report/Plots/ca_good_taste_example.png", plot = p_ca_good_ex, width = 8, height = 6)

res.ca.bad.ex <- CA(bad_ex_ca_data, graph = FALSE)
p_ca_bad_ex <- fviz_ca_biplot(res.ca.bad.ex, geom.row = "point", col.row = bad_ex_assigned, col.col = "black", 
                              title = "CA: Bad Taste Example Schemas", palette = "jco", legend.title = "Assigned Topic")
ggsave("report/Plots/ca_bad_taste_example.png", plot = p_ca_bad_ex, width = 8, height = 6)


# 4. Wald Table for Demographics across Four Models
raw_survey <- read_csv("data/FolkTaste_CleanData.csv", show_col_types = FALSE)
if(grepl("What do you mean", raw_survey$GoodTaste_Def[1])) {
  raw_survey <- raw_survey[-1, ]
}
raw_survey <- raw_survey %>% filter(Taste_Possibility == "YES")
raw_survey$Age <- as.numeric(raw_survey$Age)
raw_survey$EducationLevel_Coded <- as.numeric(raw_survey$EducationLevel_Coded)
raw_survey$Political <- as.numeric(raw_survey$Political)

run_multinom <- function(prob_file, name) {
  probs <- read_csv(prob_file, show_col_types = FALSE)
  topic_cols <- setdiff(names(probs), "original_index")
  probs$primary_topic <- apply(probs[, topic_cols], 1, function(row) topic_cols[which.max(row)])
  
  probs_valid <- probs %>% filter(!primary_topic %in% c("-1"))
  raw_survey$python_index <- 0:(nrow(raw_survey)-1)
  
  merged_df <- merge(raw_survey, probs_valid, by.x="python_index", by.y="original_index")
  merged_df$primary_topic <- as.factor(merged_df$primary_topic)
  
  model_data <- merged_df %>% drop_na(Age, Gender, EducationLevel_Coded, Political) %>% filter(Gender %in% c("A man", "A woman"))
  
  if (length(unique(model_data$primary_topic)) > 1) {
    model_data$primary_topic <- relevel(model_data$primary_topic, ref=names(sort(table(model_data$primary_topic), decreasing=TRUE)[1]))
    m <- multinom(primary_topic ~ Age + Gender + EducationLevel_Coded + Political, data=model_data, trace=FALSE)
    w <- Anova(m, type="III")
    
    # Return as dataframe
    df <- as.data.frame(w)
    df$Predictor <- rownames(df)
    df$Model <- name
    return(df)
  }
  return(NULL)
}

res1 <- run_multinom("good_taste_def_model_min5/document_topic_probabilities.csv", "Good Taste Def")
res2 <- run_multinom("bad_taste_def_model_min15/document_topic_probabilities.csv", "Bad Taste Def")
res3 <- run_multinom("good_taste_example_model_min10/document_topic_probabilities.csv", "Good Taste Example")
res4 <- run_multinom("bad_taste_example_model_min10/document_topic_probabilities.csv", "Bad Taste Example")

all_res <- bind_rows(res1, res2, res3, res4)
all_res <- all_res %>% select(Model, Predictor, `LR Chisq`, Df, `Pr(>Chisq)`)

# Write to LaTeX
latex_str <- "\\begin{table}[htpb]\n\\centering\n\\resizebox{\\textwidth}{!}{\n\\begin{tabular}{llrrr}\n\\toprule\n"
latex_str <- paste0(latex_str, "Model & Predictor & Wald $\\chi^2$ & Df & $p$-value \\\\\n\\midrule\n")

for (i in 1:nrow(all_res)) {
  pval <- all_res$`Pr(>Chisq)`[i]
  pval_str <- if(pval < 0.001) "< 0.001" else sprintf("%.4f", pval)
  latex_str <- paste0(latex_str, sprintf("%s & %s & %.2f & %d & %s \\\\\n", 
                                         all_res$Model[i], gsub("_", "\\\\_", all_res$Predictor[i]), all_res$`LR Chisq`[i], all_res$Df[i], pval_str))
}

latex_str <- paste0(latex_str, "\\bottomrule\n\\end{tabular}\n}\n\\caption{Multinomial Logistic Regression Wald Tests for Topic Predictors}\n\\label{tab:wald_four_models}\n\\end{table}\n")

writeLines(latex_str, "report/Tabs/wald_four_models.tex")

library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(stringr)

# Load data and assigned topics
raw_survey <- read_csv("data/FolkTaste_CleanData.csv", show_col_types = FALSE)
if(grepl("What do you mean", raw_survey$GoodTaste_Def[1])) {
  raw_survey <- raw_survey[-1, ]
}
raw_survey <- raw_survey %>% filter(Taste_Possibility == "YES")

# Probabilities
good_def_probs <- read_csv("good_taste_def_model_min5/document_topic_probabilities.csv", show_col_types = FALSE)
bad_def_probs <- read_csv("bad_taste_def_model_min15/document_topic_probabilities.csv", show_col_types = FALSE)
good_ex_probs <- read_csv("good_taste_example_model_min10/document_topic_probabilities.csv", show_col_types = FALSE)
bad_ex_probs <- read_csv("bad_taste_example_model_min10/document_topic_probabilities.csv", show_col_types = FALSE)

get_primary <- function(probs, name) {
  topic_cols <- setdiff(names(probs), "original_index")
  probs$primary <- apply(probs[, topic_cols], 1, function(row) topic_cols[which.max(row)])
  res <- probs %>% select(original_index, primary)
  names(res)[2] <- name
  return(res)
}

gd <- get_primary(good_def_probs, "Good_Def")
bd <- get_primary(bad_def_probs, "Bad_Def")
ge <- get_primary(good_ex_probs, "Good_Ex")
be <- get_primary(bad_ex_probs, "Bad_Ex")

df <- gd %>% inner_join(bd, by="original_index") %>% inner_join(ge, by="original_index") %>% inner_join(be, by="original_index")

# Mapping
# dynamic mapping
df <- df %>%
  filter(Good_Def != "-1", Bad_Def != "-1", Good_Ex != "-1", Bad_Ex != "-1") %>%
  mutate(
    Good_Def = paste("Topic", Good_Def),
    Bad_Def = paste("Topic", Bad_Def),
    Good_Ex = paste("Topic", Good_Ex),
    Bad_Ex = paste("Topic", Bad_Ex)
  )

domains <- c("Good_Def", "Bad_Def", "Good_Ex", "Bad_Ex")
combos <- combn(domains, 2, simplify = FALSE)

dir.create("report/Plots", showWarnings = FALSE)

cat("Chi-square results:\n\n")

for (combo in combos) {
  v1 <- combo[1]
  v2 <- combo[2]
  
  tb <- table(df[[v1]], df[[v2]])
  if (nrow(tb) > 1 && ncol(tb) > 1) {
    res <- chisq.test(tb)
    cat(sprintf("%s vs %s: X-squared = %.2f, df = %d, p-value = %.4f\n", v1, v2, res$statistic, res$parameter, res$p.value))
    
    residuals_df <- as.data.frame(as.table(res$residuals))
    names(residuals_df) <- c("Var1", "Var2", "Residual")
    
    # Format labels to fit horizontally
    residuals_df$Var1 <- str_replace_all(residuals_df$Var1, "/", "/\n")
    residuals_df$Var1 <- str_replace_all(residuals_df$Var1, " ", "\n")
    
    p <- ggplot(residuals_df, aes(x = Var1, y = Var2, fill = Residual)) +
      geom_tile(color = "white") +
      geom_text(aes(label = round(Residual, 2)), color = "black", size = 4) +
      scale_fill_gradient2(low = "indianred", mid = "white", high = "steelblue", midpoint = 0) +
      scale_x_discrete(position = "top") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0)) +
      labs(
        title = sprintf("Pearson Residuals: %s vs %s", str_replace(v1, "_", " "), str_replace(v2, "_", " ")),
        x = str_replace(v1, "_", " "),
        y = str_replace(v2, "_", " "),
        fill = "Residual"
      )
    
    file_name <- sprintf("report/Plots/schema_corr_%s_%s.png", v1, v2)
    ggsave(file_name, plot = p, width = 8, height = 6)
  }
}

library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(stringr)

# 1. Load Data
raw_survey <- read_csv("data/FolkTaste_CleanData.csv", show_col_types = FALSE)
if(grepl("What do you mean", raw_survey$GoodTaste_Def[1])) {
  raw_survey <- raw_survey[-1, ]
}
raw_survey <- raw_survey %>% filter(Taste_Possibility == "YES")

# 2. Load Probs
good_ex_probs <- read_csv("good_taste_example_model_min10/document_topic_probabilities.csv", show_col_types = FALSE)
bad_ex_probs <- read_csv("bad_taste_example_model_min10/document_topic_probabilities.csv", show_col_types = FALSE)

topic_cols_g <- setdiff(names(good_ex_probs), "original_index")
good_ex_probs$Good_Ex_Id <- apply(good_ex_probs[, topic_cols_g], 1, function(row) topic_cols_g[which.max(row)])

topic_cols_b <- setdiff(names(bad_ex_probs), "original_index")
bad_ex_probs$Bad_Ex_Id <- apply(bad_ex_probs[, topic_cols_b], 1, function(row) topic_cols_b[which.max(row)])

df_g <- good_ex_probs %>% select(original_index, Good_Ex_Id)
df_b <- bad_ex_probs %>% select(original_index, Bad_Ex_Id)

raw_survey$python_index <- 0:(nrow(raw_survey)-1)
merged_df <- raw_survey %>% 
  left_join(df_g, by=c("python_index"="original_index")) %>%
  left_join(df_b, by=c("python_index"="original_index"))

# Labels

merged_df <- merged_df %>%
  mutate(
    Good_Ex = paste("Topic", Good_Ex_Id),
    Bad_Ex = paste("Topic", Bad_Ex_Id)
  ) %>%
  filter(!is.na(Good_Ex) & Good_Ex != "Topic -1" & !is.na(Bad_Ex) & Bad_Ex != "Topic -1")

distinction_cols <- grep("Domains_Distinction1_", names(merged_df), value = TRUE)

dir.create("report/Plots", showWarnings = FALSE)

# Function to create heatmap
create_heatmap <- function(data, group_col, title, filename) {
  dist_means <- data %>%
    select(all_of(group_col), all_of(distinction_cols)) %>%
    pivot_longer(cols = starts_with("Domains_Distinction1_"), names_to = "Domain", values_to = "Score") %>%
    mutate(Domain = str_remove(Domain, "Domains_Distinction1_"), Score = as.numeric(Score)) %>%
    filter(!is.na(Score)) %>%
    group_by(!!sym(group_col), Domain) %>%
    summarize(Mean_Score = mean(Score, na.rm = TRUE), .groups = 'drop')
  
  domain_order <- dist_means %>%
    group_by(Domain) %>%
    summarize(Overall = mean(Mean_Score)) %>%
    arrange(Overall) %>%
    pull(Domain)
  
  dist_means <- dist_means %>% mutate(Domain = factor(Domain, levels = domain_order))
  
  p <- ggplot(dist_means, aes(x = !!sym(group_col), y = Domain, fill = Mean_Score)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "steelblue", mid = "white", high = "indianred", midpoint = 1.5) +
    theme_minimal(base_size = 14) +
    theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
    labs(title = title, x = "Topic Schema", y = "Cultural Domain", fill = "Mean Score")
  
  ggsave(filename, plot = p, width = 10, height = 6)
  cat(paste0("Generated: ", filename, "\n"))
}

create_heatmap(merged_df, "Good_Ex", "Average Distinction Rating by Domain (Good Taste Examples)", "report/Plots/distinction_domains_good_ex.png")
create_heatmap(merged_df, "Bad_Ex", "Average Distinction Rating by Domain (Bad Taste Examples)", "report/Plots/distinction_domains_bad_ex.png")

library(dplyr)
library(tidyr)
library(nnet)
library(car)
library(effects)
library(ggplot2)
library(ggsci)

# Re-run the model for Good Taste Examples specifically
raw_survey <- read.csv("data/FolkTaste_CleanData.csv")
if(grepl("What do you mean", raw_survey$GoodTaste_Def[1])) raw_survey <- raw_survey[-1, ]
raw_survey <- raw_survey %>% filter(Taste_Possibility == "YES")

good_ex_probs <- read.csv("good_taste_example_model_min10/document_topic_probabilities.csv")
topic_cols <- setdiff(names(good_ex_probs), "original_index")
good_ex_probs$primary_topic <- apply(good_ex_probs[, topic_cols], 1, function(row) topic_cols[which.max(row)])
good_ex_probs <- good_ex_probs %>% filter(!primary_topic %in% "-1")

raw_survey$python_index <- 0:(nrow(raw_survey)-1)
merged_df <- merge(raw_survey, good_ex_probs, by.x="python_index", by.y="original_index")

# Mapping
merged_df$Topic <- as.factor(merged_df$primary_topic)

merged_df$Age <- as.numeric(merged_df$Age)
merged_df$EducationLevel_Coded <- as.numeric(merged_df$EducationLevel_Coded)
merged_df$Political <- as.numeric(merged_df$Political)

model_data <- merged_df %>% drop_na(Age, Gender, EducationLevel_Coded, Political) %>% filter(Gender %in% c("A man", "A woman"))
m_good_ex <- multinom(Topic ~ Age + Gender + EducationLevel_Coded + Political, data = model_data, Hess = TRUE, trace = FALSE)

# Generate effect plots for Age (significant predictor)
eff_age <- Effect("Age", m_good_ex)
plot_df <- as.data.frame(eff_age)

# (Visualization code would go here to plot `plot_df` probabilities by age across the 7 topics)

library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(stringr)
library(FactoMineR)
library(factoextra)

# 1. Load Data
raw_survey <- read_csv("data/FolkTaste_CleanData.csv", show_col_types = FALSE)
if(grepl("What do you mean", raw_survey$GoodTaste_Def[1])) {
  raw_survey <- raw_survey[-1, ]
}
raw_survey <- raw_survey %>% filter(Taste_Possibility == "YES")
raw_survey$python_index <- 0:(nrow(raw_survey)-1)

# 2. Impute and Run PCA
dist_cols <- grep("Domains_Distinction1_", names(raw_survey), value = TRUE)
dist_data <- raw_survey %>% select(all_of(dist_cols)) %>% mutate_all(as.numeric)

# Simple mean imputation to retain all valid rows
dist_data_imp <- as.data.frame(lapply(dist_data, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x)))

# Clean column names for the plot
names(dist_data_imp) <- str_remove(names(dist_data_imp), "Domains_Distinction1_")

res.pca <- PCA(dist_data_imp, scale.unit = TRUE, graph = FALSE)

dir.create("report/Plots", showWarnings = FALSE)
p_pca_var <- fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, title = "PCA of Cultural Domains Distinction Ratings")
ggsave("report/Plots/pca_domain_variables.png", plot = p_pca_var, width = 8, height = 6)

# Extract first 3 dimensions
raw_survey$Dim1 <- res.pca$ind$coord[,1]
raw_survey$Dim2 <- res.pca$ind$coord[,2]
raw_survey$Dim3 <- res.pca$ind$coord[,3]

# 3. Load Topics and Merge
get_primary <- function(path, name) {
  probs <- read_csv(path, show_col_types = FALSE)
  tc <- setdiff(names(probs), "original_index")
  probs[[name]] <- apply(probs[, tc], 1, function(row) tc[which.max(row)])
  probs %>% select(original_index, !!sym(name))
}

gd <- get_primary("good_taste_def_model_min5/document_topic_probabilities.csv", "Good_Def")
bd <- get_primary("bad_taste_def_model_min15/document_topic_probabilities.csv", "Bad_Def")
ge <- get_primary("good_taste_example_model_min10/document_topic_probabilities.csv", "Good_Ex")
be <- get_primary("bad_taste_example_model_min10/document_topic_probabilities.csv", "Bad_Ex")

df <- raw_survey %>%
  left_join(gd, by=c("python_index"="original_index")) %>%
  left_join(bd, by=c("python_index"="original_index")) %>%
  left_join(ge, by=c("python_index"="original_index")) %>%
  left_join(be, by=c("python_index"="original_index"))

df$Good_Def[df$Good_Def == "-1"] <- NA
df$Bad_Def[df$Bad_Def == "-1"] <- NA
df$Good_Ex[df$Good_Ex == "-1"] <- NA
df$Bad_Ex[df$Bad_Ex == "-1"] <- NA

run_anova_for_dim <- function(dim_col) {
  results <- list()
  for (model_col in c("Good_Def", "Bad_Def", "Good_Ex", "Bad_Ex")) {
    test_df <- df %>% select(!!sym(model_col), !!sym(dim_col)) %>% drop_na()
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

all_res <- bind_rows(res_dim1, res_dim2, res_dim3)

# Format to wide table for LaTeX
all_res$Format <- sprintf("%.2f (%.3f)", all_res$F_val, all_res$p_val)
all_res$Format <- ifelse(all_res$p_val < 0.001, paste0(all_res$Format, "$^{***}$"), 
                  ifelse(all_res$p_val < 0.01, paste0(all_res$Format, "$^{**}$"),
                  ifelse(all_res$p_val < 0.05, paste0(all_res$Format, "$^{*}$"), all_res$Format)))

wide_res <- all_res %>% select(Dimension, Model, Format) %>%
  pivot_wider(names_from = Model, values_from = Format)

dir.create("report/Tabs", showWarnings = FALSE)
latex_str <- "\\begin{table}[htpb]\n\\centering\n\\begin{tabular}{lcccc}\n\\toprule\n"
latex_str <- paste0(latex_str, "Dimension & Good Taste Def & Bad Taste Def & Good Taste Ex & Bad Taste Ex \\\\\n\\midrule\n")
for (i in 1:nrow(wide_res)) {
  latex_str <- paste0(latex_str, sprintf("%s & %s & %s & %s & %s \\\\\n", 
        wide_res$Dimension[i], wide_res$Good_Def[i], wide_res$Bad_Def[i], wide_res$Good_Ex[i], wide_res$Bad_Ex[i]))
}
latex_str <- paste0(latex_str, "\\bottomrule\n\\multicolumn{5}{l}{\\footnotesize $^{*}p<0.05; ^{**}p<0.01; ^{***}p<0.001$} \\\\\n")
latex_str <- paste0(latex_str, "\\end{tabular}\n\\caption{ANOVA Results: Mean Differences in PCA Dimensions of Distinction Ratings by Topic Schema.}\n\\label{tab:anova_pca}\n\\end{table}\n")

writeLines(latex_str, "report/Tabs/anova_pca.tex")

