library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(stringr)

# --- RE-GENERATE PEARSON RESIDUAL PLOTS ---
raw_survey <- read_csv("data/FolkTaste_CleanData.csv", show_col_types = FALSE)
if(grepl("What do you mean", raw_survey$GoodTaste_Def[1])) raw_survey <- raw_survey[-1, ]
raw_survey <- raw_survey %>% filter(Taste_Possibility == "YES")

gd_probs <- read_csv("good_taste_def_model_min10/document_topic_probabilities.csv", show_col_types = FALSE)
bd_probs <- read_csv("bad_taste_def_model_min5/document_topic_probabilities.csv", show_col_types = FALSE)

get_primary <- function(probs, name) {
  tc <- setdiff(names(probs), "original_index")
  probs$primary <- apply(probs[, tc], 1, function(row) tc[which.max(row)])
  res <- probs %>% select(original_index, primary)
  names(res)[2] <- name
  res
}

gd <- get_primary(gd_probs, "Good_Def")
bd <- get_primary(bd_probs, "Bad_Def")

df <- gd %>% inner_join(bd, by="original_index")

t_g_d <- c("-1"="Outlier", "0"="Similarity/Subjectivity", "1"="Aesthetics", "2"="Style/Quality")
t_b_d <- c("-1"="Outlier", "0"="Dislike/Subjective", "1"="Poor Choices", "2"="Tacky/Loud", "3"="Low-Brow Media", "4"="Immoral", "5"="Poor Manners")

df <- df %>%
  filter(Good_Def != "-1", Bad_Def != "-1") %>%
  mutate(
    Good_Def = t_g_d[as.character(Good_Def)],
    Bad_Def = t_b_d[as.character(Bad_Def)]
  )

combos <- list(c("Good_Def", "Bad_Def"))

for (c in combos) {
  v1 <- c[1]
  v2 <- c[2]
  tb <- table(df[[v1]], df[[v2]])
  if(nrow(tb)>1 && ncol(tb)>1) {
    res <- chisq.test(tb)
    r_df <- as.data.frame(as.table(res$residuals))
    names(r_df) <- c("Var1", "Var2", "Residual")
    
    # Format labels with newlines so they fit horizontally
    r_df$Var1 <- str_replace_all(r_df$Var1, "/", "/\n")
    r_df$Var1 <- str_replace_all(r_df$Var1, " ", "\n")
    
    p <- ggplot(r_df, aes(x = Var1, y = Var2, fill = Residual)) +
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
    ggsave(sprintf("report/Plots/schema_corr_%s_%s.png", v1, v2), plot=p, width=10, height=7)
  }
}

# --- RE-GENERATE HEATMAPS (DEFINITIONS ONLY) ---
df_g_d <- get_primary(gd_probs, "Good_Def")
df_b_d <- get_primary(bd_probs, "Bad_Def")

raw_survey$python_index <- 0:(nrow(raw_survey)-1)
m_df <- raw_survey %>%
  left_join(df_g_d, by=c("python_index"="original_index")) %>%
  left_join(df_b_d, by=c("python_index"="original_index")) %>%
  mutate(
    Good_Def = t_g_d[as.character(Good_Def)],
    Bad_Def = t_b_d[as.character(Bad_Def)]
  )

distinction_cols <- grep("Domains_Distinction1_", names(m_df), value = TRUE)

create_heatmap <- function(data, group_col, title, filename) {
  d_clean <- data %>% filter(!is.na(!!sym(group_col)) & !!sym(group_col) != "Outlier")
  
  dist_means <- d_clean %>%
    select(all_of(group_col), all_of(distinction_cols)) %>%
    pivot_longer(cols = starts_with("Domains_Distinction1_"), names_to = "Domain", values_to = "Score") %>%
    mutate(Domain = str_remove(Domain, "Domains_Distinction1_"), Score = as.numeric(Score)) %>%
    filter(!is.na(Score)) %>%
    group_by(!!sym(group_col), Domain) %>%
    summarize(Mean_Score = mean(Score, na.rm = TRUE), .groups = 'drop')
  
  domain_order <- dist_means %>% group_by(Domain) %>% summarize(Overall = mean(Mean_Score)) %>% arrange(Overall) %>% pull(Domain)
  dist_means <- dist_means %>% mutate(Domain = factor(Domain, levels = domain_order))
  
  # Add newlines to make sure they fit horizontally
  dist_means[[group_col]] <- str_replace_all(dist_means[[group_col]], "/", "/\n")
  dist_means[[group_col]] <- str_replace_all(dist_means[[group_col]], " ", "\n")
  
  p <- ggplot(dist_means, aes(x = !!sym(group_col), y = Domain, fill = Mean_Score)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "steelblue", mid = "white", high = "indianred", midpoint = 1.5) +
    scale_x_discrete(position = "top") +
    theme_minimal(base_size = 14) +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0)) +
    labs(title = title, x = "Topic Schema", y = "Cultural Domain", fill = "Mean Score")
  
  ggsave(filename, plot = p, width = 11, height = 7)
}

create_heatmap(m_df, "Good_Def", "Average Distinction Rating by Domain (Good Taste Definitions)", "report/Plots/distinction_domains.png")
create_heatmap(m_df, "Bad_Def", "Average Distinction Rating by Domain (Bad Taste Definitions)", "report/Plots/distinction_domains_bad.png")
