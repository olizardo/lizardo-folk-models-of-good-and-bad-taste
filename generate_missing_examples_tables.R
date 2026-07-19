library(dplyr)
library(readr)
library(stringr)

dir.create("report/Tabs", showWarnings = FALSE)

# 1. Load Data
raw_survey <- read_csv("data/FolkTaste_CleanData.csv", show_col_types = FALSE)
if(grepl("What do you mean", raw_survey$GoodTaste_Def[1])) {
  raw_survey <- raw_survey[-1, ]
}
valid_data <- raw_survey %>% filter(Taste_Possibility == "YES")

# 2. Get Example Responses function
get_top_docs <- function(docs, prob_matrix, topic_col, n = 2) {
  if(!as.character(topic_col) %in% colnames(prob_matrix)) return("N/A")
  probs <- prob_matrix[[as.character(topic_col)]]
  top_idx <- order(probs, decreasing = TRUE)[1:n]
  # For latex, use bullet points instead of HTML
  paste(paste0("\\textbullet\\ ", "\\textit{`", str_replace_all(docs[top_idx], "[\\n\\r]", " "), "'}"), collapse = " \\newline ")
}

# 3. Good Taste Examples (Ex)
good_ex_docs <- valid_data$GoodTaste_Example[!is.na(valid_data$GoodTaste_Example)]
good_ex_probs <- read_csv("good_taste_example_model_min5/document_topic_probabilities.csv", show_col_types = FALSE)
good_ex_info <- read_csv("good_taste_example_model_min5/topic_info.csv", show_col_types = FALSE)

clean_keywords <- function(rep_str) str_remove_all(rep_str, "\\[|\\]|'")
good_ex_info$Keywords <- sapply(good_ex_info$Representation, clean_keywords)

good_ex_info$Example_Responses <- sapply(good_ex_info$Topic, function(t) {
  if(t == -1) return("Outliers")
  get_top_docs(good_ex_docs, good_ex_probs, t, n = 2)
})

good_ex_table <- good_ex_info %>% filter(Topic != -1) %>% select(Topic, Name, Example_Responses)

# 4. Bad Taste Examples (Ex)
bad_ex_docs <- valid_data$BadTaste_Example[!is.na(valid_data$BadTaste_Example)]
bad_ex_probs <- read_csv("bad_taste_example_model_min10/document_topic_probabilities.csv", show_col_types = FALSE)
bad_ex_info <- read_csv("bad_taste_example_model_min10/topic_info.csv", show_col_types = FALSE)

bad_ex_info$Keywords <- sapply(bad_ex_info$Representation, clean_keywords)
bad_ex_info$Example_Responses <- sapply(bad_ex_info$Topic, function(t) {
  if(t == -1) return("Outliers")
  get_top_docs(bad_ex_docs, bad_ex_probs, t, n = 2)
})

bad_ex_table <- bad_ex_info %>% filter(Topic != -1) %>% select(Topic, Name, Example_Responses)

# We need custom LaTeX writers to handle the multi-line text safely using p{} columns
write_latex_table <- function(df, filename, caption, label) {
  num_cols <- ncol(df)
  
  latex_str <- "\\begin{table}[htpb]\n\\centering\n\\resizebox{\\textwidth}{!}{\n"
  # Set specific widths for the columns
  latex_str <- paste0(latex_str, "\\begin{tabular}{llp{10cm}}\n\\toprule\n")
  latex_str <- paste0(latex_str, paste(str_replace_all(colnames(df), "_", "\\\\_"), collapse=" & "), " \\\\\n\\midrule\n")
  
  for(i in 1:nrow(df)) {
    # Escape latex special characters in the text
    escaped_text <- str_replace_all(df$Example_Responses[i], "&", "\\\\&")
    escaped_text <- str_replace_all(escaped_text, "%", "\\\\%")
    escaped_text <- str_replace_all(escaped_text, "\\$", "\\\\$")
    # Escape underscores in text
    escaped_text <- str_replace_all(escaped_text, "_", "\\\\_")
    
    latex_str <- paste0(latex_str, sprintf("%s & %s & %s \\\\\n", df$Topic[i], str_replace_all(df$Name[i], "_", "\\\\_"), escaped_text))
  }
  
  latex_str <- paste0(latex_str, "\\bottomrule\n\\end{tabular}\n}\n")
  latex_str <- paste0(latex_str, sprintf("\\caption{%s}\n\\label{%s}\n\\end{table}\n", caption, label))
  
  writeLines(latex_str, filename)
}

write_latex_table(good_ex_table, "report/Tabs/good_taste_ex_examples_table.tex", "Representative Documents for Good Taste Examples", "tab:good_ex_examples")
write_latex_table(bad_ex_table, "report/Tabs/bad_taste_ex_examples_table.tex", "Representative Documents for Bad Taste Examples", "tab:bad_ex_examples")

# Now re-generate the Definition ones using the safe LaTeX script
# (the old python convert script breaks on multiline HTML tables or doesn't set p{} widths, making them run off the page)
gd_docs <- valid_data$GoodTaste_Def[!is.na(valid_data$GoodTaste_Def)]
gd_probs <- read_csv("good_taste_def_model_min10/document_topic_probabilities.csv", show_col_types = FALSE)
gd_info <- read_csv("good_taste_def_model_min10/topic_info.csv", show_col_types = FALSE)
gd_info$Example_Responses <- sapply(gd_info$Topic, function(t) {
  if(t == -1) return("Outliers")
  get_top_docs(gd_docs, gd_probs, t, n = 2)
})
gd_table <- gd_info %>% filter(Topic != -1) %>% select(Topic, Name, Example_Responses)

bd_docs <- valid_data$BadTaste_Def[!is.na(valid_data$BadTaste_Def)]
bd_probs <- read_csv("bad_taste_def_model_min5/document_topic_probabilities.csv", show_col_types = FALSE)
bd_info <- read_csv("bad_taste_def_model_min5/topic_info.csv", show_col_types = FALSE)
bd_info$Example_Responses <- sapply(bd_info$Topic, function(t) {
  if(t == -1) return("Outliers")
  get_top_docs(bd_docs, bd_probs, t, n = 2)
})
bd_table <- bd_info %>% filter(Topic != -1) %>% select(Topic, Name, Example_Responses)

write_latex_table(gd_table, "report/Tabs/good_taste_def_examples_table.tex", "Representative Documents for Good Taste Definitions", "tab:good_def_examples")
write_latex_table(bd_table, "report/Tabs/bad_taste_def_examples_table.tex", "Representative Documents for Bad Taste Definitions", "tab:bad_def_examples")

