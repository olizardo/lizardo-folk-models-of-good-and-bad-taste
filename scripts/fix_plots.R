library(tidytext)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(stringr)
library(jsonlite)

config <- fromJSON("config.json")
good_model_dir <- config$good_model
bad_model_dir <- config$bad_model

dir.create("report/Plots", showWarnings = FALSE)

# --- TOPIC FREQUENCIES AND KEYWORDS ---
plot_topics_and_keywords <- function(model_dir, title_prefix, color, prefix) {
  # 1. Frequencies
  info <- read_csv(file.path(model_dir, "topic_info.csv"), show_col_types = FALSE) %>% filter(Topic != -1)
  
  # Format names
  info$NameClean <- str_replace_all(str_replace(info$Name, "^\\d+_", ""), "_", " ")
  info$NameClean <- str_to_title(info$NameClean)
  
  p_topics <- ggplot(info, aes(x = reorder(NameClean, Count), y = Count)) +
    geom_bar(stat = "identity", fill = color) +
    coord_flip() +
    theme_minimal(base_size = 14) +
    labs(title = paste(title_prefix, "(Definitions)"), x = "Cultural Model", y = "Count")
  ggsave(sprintf("report/Plots/%s_topics.png", prefix), plot = p_topics, width = 8, height = 6)
  
  # 2. Keywords
  clean_keywords <- function(rep_str) {
    words <- str_extract_all(rep_str, "'([^']+)'")[[1]]
    words <- str_replace_all(words, "'", "")
    return(words)
  }
  
  word_list <- list()
  for (i in 1:nrow(info)) {
    words <- clean_keywords(info$Representation[i])
    # Give them a dummy score decreasing from 10 to 1 for plotting order
    scores <- seq(length(words), 1, length.out = length(words))
    df_words <- data.frame(
      Topic = info$NameClean[i],
      Word = words,
      Score = scores
    )
    word_list[[i]] <- df_words
  }
  
  words_df <- bind_rows(word_list)
  
  p_keywords <- ggplot(words_df, aes(x = reorder_within(Word, Score, Topic), y = Score)) +
    geom_bar(stat = "identity", fill = color, show.legend = FALSE) +
    scale_x_reordered() +
    coord_flip() +
    facet_wrap(~ Topic, scales = "free_y") +
    theme_minimal(base_size = 12) +
    theme(
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    ) +
    labs(title = paste("Top Keywords per", title_prefix, "Cultural Model"), x = "Keyword", y = "Relative Prominence (NMF Weight)")
  
  ggsave(sprintf("report/Plots/%s_keywords.png", prefix), plot = p_keywords, width = 10, height = 6)
}
plot_topics_and_keywords(good_model_dir, "Good Taste", "steelblue", "good_taste")
plot_topics_and_keywords(bad_model_dir, "Bad Taste", "indianred", "bad_taste")

# --- RE-GENERATE CA PLOTS ---
library(FactoMineR)
library(factoextra)

plot_ca <- function(model_dir, title_prefix, prefix) {
  # Get names for plotting
  info <- read_csv(file.path(model_dir, "topic_info.csv"), show_col_types = FALSE)
  names_clean <- str_replace_all(str_replace(info$Name, "^\\d+_", ""), "_", " ")
  names_title <- str_to_title(names_clean)
  map <- setNames(names_title, as.character(info$Topic))
  
  probs <- read_csv(file.path(model_dir, "document_topic_probabilities.csv"), show_col_types = FALSE)
  probs_clean <- probs %>% select(-original_index)
  colnames(probs_clean) <- unname(map[as.character(0:(ncol(probs_clean)-1))])
  ca_data <- probs_clean + 1e-6
  assigned_raw <- as.character(apply(probs_clean, 1, which.max) - 1)
  assigned <- as.factor(unname(map[assigned_raw]))
  
  res.ca <- CA(ca_data, graph = FALSE)
  p_ca <- fviz_ca_biplot(res.ca, geom.row = "point", col.row = assigned, col.col = "black", 
                         title = paste("CA:", title_prefix, "Taste Cultural Models"), palette = "jco", legend.title = "Assigned Cultural Model")
  p_ca <- p_ca + theme(legend.position = "bottom", legend.direction="vertical")
  ggsave(sprintf("report/Plots/%s.png", prefix), plot = p_ca, width = 10, height = 7)
}
plot_ca(good_model_dir, "Good", "ca_good_taste")
plot_ca(bad_model_dir, "Bad", "ca_bad_taste")
raw_survey <- read_csv("data/FolkTaste_CleanData.csv", show_col_types = FALSE)
if(grepl("What do you mean", raw_survey$GoodTaste_Def[1])) raw_survey <- raw_survey[-1, ]
raw_survey <- raw_survey %>% filter(Taste_Possibility == "YES")

gd_probs <- read_csv(file.path(good_model_dir, "document_topic_probabilities.csv"), show_col_types = FALSE)
bd_probs <- read_csv(file.path(bad_model_dir, "document_topic_probabilities.csv"), show_col_types = FALSE)

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
      geom_text(aes(label = round(Residual, 2)), color = "black", size = 12, fontface = "bold") +
      scale_fill_gradient2(low = "indianred", mid = "white", high = "steelblue", midpoint = 0) +
      scale_x_discrete(position = "top") +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0, size = 12, face = "bold"),
            axis.text.y = element_text(size = 12, face = "bold")) +
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
    geom_text(aes(label = sprintf("%.2f", Mean_Score)), color = "black", size = 4) +
    scale_fill_gradient2(low = "steelblue", mid = "white", high = "indianred", midpoint = 1.5) +
    scale_x_discrete(position = "top") +
    theme_minimal(base_size = 14) +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0)) +
    labs(title = title, x = "Cultural Model", y = "Cultural Domain", fill = "Mean Score")
  
  ggsave(filename, plot = p, width = 11, height = 7)
}

create_heatmap(m_df, "Good_Def", "Average Distinction Rating by Domain (Good Taste Definitions)", "report/Plots/distinction_domains.png")
create_heatmap(m_df, "Bad_Def", "Average Distinction Rating by Domain (Bad Taste Definitions)", "report/Plots/distinction_domains_bad.png")
