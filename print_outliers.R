library(dplyr)
library(readr)
get_outliers <- function(prefix, sizes) {
  for (s in sizes) {
    ti <- read_csv(paste0(prefix, "_model_min", s, "/topic_info.csv"), show_col_types = FALSE)
    out_count <- if(any(ti$Topic == -1)) ti$Count[ti$Topic == -1] else 0
    cat(prefix, "min:", s, "Topics:", nrow(ti)-1, "Outliers:", out_count, "\n")
  }
}
get_outliers("good_taste_def", c(5,10,15,20))
get_outliers("bad_taste_def", c(5,10,15,20))
