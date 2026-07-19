library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(jsonlite)

config <- fromJSON("config.json")
good_model_dir <- config$good_model
bad_model_dir <- config$bad_model

# 1. Load Data
raw_survey <- read_csv("data/FolkTaste_CleanData.csv", show_col_types = FALSE)
if(grepl("What do you mean", raw_survey$GoodTaste_Def[1])) {
  raw_survey <- raw_survey[-1, ]
}
raw_survey <- raw_survey %>% filter(Taste_Possibility == "YES")
raw_survey$python_index <- 0:(nrow(raw_survey)-1)

# 2. Get Primary Topics
get_primary <- function(path, name) {
  probs <- read_csv(path, show_col_types = FALSE)
  tc <- setdiff(names(probs), "original_index")
  probs[[name]] <- apply(probs[, tc], 1, function(row) tc[which.max(row)])
  probs %>% select(original_index, !!sym(name))
}

gd <- get_primary(file.path(good_model_dir, "document_topic_probabilities.csv"), "Good_Def")
bd <- get_primary(file.path(bad_model_dir, "document_topic_probabilities.csv"), "Bad_Def")

df <- raw_survey %>%
  left_join(gd, by=c("python_index"="original_index")) %>%
  left_join(bd, by=c("python_index"="original_index"))

# Remove outliers for ANOVA
df$Good_Def[df$Good_Def == "-1"] <- NA
df$Bad_Def[df$Bad_Def == "-1"] <- NA

dist_cols <- grep("Domains_Distinction1_", names(df), value = TRUE)

run_anova_for_model <- function(topic_col) {
  results <- list()
  for (col in dist_cols) {
    domain_name <- str_remove(col, "Domains_Distinction1_")
    
    test_df <- df %>% select(!!sym(topic_col), !!sym(col)) %>% drop_na()
    test_df[[col]] <- as.numeric(test_df[[col]])
    
    if (length(unique(test_df[[topic_col]])) > 1) {
      formula <- as.formula(paste0("`", col, "` ~ ", topic_col))
      aov_res <- aov(formula, data = test_df)
      s <- summary(aov_res)[[1]]
      f_val <- s[["F value"]][1]
      p_val <- s[["Pr(>F)"]][1]
      results[[domain_name]] <- data.frame(Domain=domain_name, F_val=f_val, p_val=p_val)
    }
  }
  res_df <- bind_rows(results)
  res_df$p_adj <- p.adjust(res_df$p_val, method="BH")
  res_df
}

res_gd <- run_anova_for_model("Good_Def")
res_bd <- run_anova_for_model("Bad_Def")

# Format into a single table
combined <- res_gd %>% select(Domain)
format_cell <- function(f, p_adj) {
  sig <- ifelse(p_adj < 0.001, "***", ifelse(p_adj < 0.01, "**", ifelse(p_adj < 0.05, "*", "")))
  sprintf("%.2f%s (%.3f)", f, sig, p_adj)
}

combined$Good_Def <- format_cell(res_gd$F_val, res_gd$p_adj)
combined$Bad_Def <- format_cell(res_bd$F_val, res_bd$p_adj)

dir.create("report/Tabs", showWarnings = FALSE)

# Write to LaTeX
latex_str <- "\\begin{table}[htpb]\n\\centering\n\\resizebox{\\textwidth}{!}{\n\\begin{tabular}{lcc}\n\\toprule\n"
latex_str <- paste0(latex_str, "Domain & Good Taste Def & Bad Taste Def \\\\\n")
latex_str <- paste0(latex_str, "& $F$ ($p_{adj}$) & $F$ ($p_{adj}$) \\\\\n\\midrule\n")

for (i in 1:nrow(combined)) {
  # Add significance asterisks nicely formatted in math mode
  g_d <- str_replace_all(combined$Good_Def[i], "\\*\\*\\*", "$^{***}$")
  g_d <- str_replace_all(g_d, "(?<!\\$)\\*\\*(?!\\$)", "$^{**}$")
  g_d <- str_replace_all(g_d, "(?<!\\$)\\*(?!\\$)", "$^{*}$")
  
  b_d <- str_replace_all(combined$Bad_Def[i], "\\*\\*\\*", "$^{***}$")
  b_d <- str_replace_all(b_d, "(?<!\\$)\\*\\*(?!\\$)", "$^{**}$")
  b_d <- str_replace_all(b_d, "(?<!\\$)\\*(?!\\$)", "$^{*}$")
  
  latex_str <- paste0(latex_str, sprintf("%s & %s & %s \\\\\n", 
                                         str_replace_all(combined$Domain[i], "_", "\\\\_"), 
                                         g_d, b_d))
}

latex_str <- paste0(latex_str, "\\bottomrule\n\\multicolumn{3}{l}{\\footnotesize $^{*}p<0.05; ^{**}p<0.01; ^{***}p<0.001$ (Benjamini-Hochberg FDR corrected)} \\\\\n")
latex_str <- paste0(latex_str, "\\end{tabular}\n}\n\\caption{One-Way ANOVA Results: Mean Differences in Domain Distinction Ratings by Topic Schema. Values are $F$-statistics with FDR-adjusted $p$-values in parentheses.}\n\\label{tab:anova_distinction}\n\\end{table}\n")

writeLines(latex_str, "report/Tabs/anova_distinction.tex")
