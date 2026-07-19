library(readr)
library(dplyr)
library(jsonlite)
conf <- fromJSON("config.json")
gm <- read_csv(file.path(conf$good_model, "topic_info.csv")) %>% filter(Topic != -1)
bm <- read_csv(file.path(conf$bad_model, "topic_info.csv")) %>% filter(Topic != -1)

make_tab <- function(df, out, cap, lbl) {
  df$Name <- sapply(strsplit(df$Name, "_"), function(x) paste(tools::toTitleCase(x[-1]), collapse=" "))
  df$Representation <- sapply(regmatches(df$Representation, gregexpr("'([^']+)'", df$Representation)), function(x) {
    x <- gsub("'", "", x)
    s <- paste(x, collapse=", ")
    if(nchar(s) > 60) paste0(substr(s, 1, 57), "...") else s
  })
  str <- "\\begin{table}[htpb]\n\\centering\n\\resizebox{\\textwidth}{!}{\n\\begin{tabular}{llrl}\n\\toprule\n"
  str <- paste0(str, "Topic & Count & Name & Representation \\\\\n\\midrule\n")
  for(i in 1:nrow(df)) {
    str <- paste0(str, sprintf("%s & %s & %s & %s \\\\\n", df$Topic[i], df$Count[i], df$Name[i], gsub("&", "\\\\&", df$Representation[i])))
  }
  str <- paste0(str, "\\bottomrule\n\\end{tabular}\n}\n")
  str <- paste0(str, "\\caption{", cap, "}\n\\label{tab:", lbl, "}\n\\end{table}\n")
  writeLines(str, out)
}

make_tab(gm, "report/Tabs/good_taste_def_summary_table.tex", "Good Taste Def Summary", "good_taste_def_summary")
make_tab(bm, "report/Tabs/bad_taste_def_summary_table.tex", "Bad Taste Def Summary", "bad_taste_def_summary")
