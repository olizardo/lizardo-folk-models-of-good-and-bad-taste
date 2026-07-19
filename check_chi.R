library(dplyr)
library(readr)
library(jsonlite)

config <- fromJSON("config.json")
gd_prob <- read_csv(file.path(config$good_model, "document_topic_probabilities.csv"), show_col_types = FALSE)
bd_prob <- read_csv(file.path(config$bad_model, "document_topic_probabilities.csv"), show_col_types = FALSE)

get_primary <- function(probs, name) {
  tc <- setdiff(names(probs), "original_index")
  probs[[name]] <- apply(probs[, tc], 1, function(row) tc[which.max(row)])
  probs %>% select(original_index, !!sym(name))
}

gd <- get_primary(gd_prob, "Good_Def")
bd <- get_primary(bd_prob, "Bad_Def")

df <- inner_join(gd, bd, by="original_index") %>%
  filter(Good_Def != "-1", Bad_Def != "-1")

tbl <- table(df$Good_Def, df$Bad_Def)
print(chisq.test(tbl))
