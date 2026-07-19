library(jsonlite)
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)

summary_data <- fromJSON("topic_robustness_summary_def_only.json")

get_outliers <- function(prefix, sizes) {
  outliers <- c()
  for (s in sizes) {
    ti <- read_csv(paste0(prefix, "_model_min", s, "/topic_info.csv"), show_col_types = FALSE)
    out_count <- if(any(ti$Topic == -1)) ti$Count[ti$Topic == -1] else 0
    outliers <- c(outliers, out_count)
  }
  return(outliers)
}

sizes <- c(5, 10, 15, 20)
good_outliers <- get_outliers("good_taste_def", sizes)
bad_outliers <- get_outliers("bad_taste_def", sizes)

df_good <- data.frame(
  MinTopicSize = sizes,
  NumTopics = summary_data$good_taste_def$num_topics,
  Outliers = good_outliers,
  Model = "Good Taste Definition"
)

df_bad <- data.frame(
  MinTopicSize = sizes,
  NumTopics = summary_data$bad_taste_def$num_topics,
  Outliers = bad_outliers,
  Model = "Bad Taste Definition"
)

df <- bind_rows(df_good, df_bad)

# We want a dual-axis plot or just plot them side by side
p <- ggplot(df, aes(x = MinTopicSize)) +
  geom_line(aes(y = NumTopics, color = "Number of Topics"), size=1) +
  geom_point(aes(y = NumTopics, color = "Number of Topics"), size=3) +
  geom_line(aes(y = Outliers / 10, color = "Outliers (scaled / 10)"), size=1, linetype="dashed") +
  geom_point(aes(y = Outliers / 10, color = "Outliers (scaled / 10)"), size=3) +
  facet_wrap(~Model) +
  scale_y_continuous(
    name = "Number of Topics",
    sec.axis = sec_axis(~ . * 10, name="Number of Outlier Documents")
  ) +
  labs(x = "Minimum Topic Size Parameter", title = "BERTopic Model Robustness", 
       subtitle = "Optimal models balance finding enough distinct topics while minimizing outliers") +
  theme_minimal() +
  scale_color_manual(name="", values=c("Number of Topics"="#00AFBB", "Outliers (scaled / 10)"="#FC4E07")) +
  theme(legend.position="bottom")

ggsave("report/Plots/robustness_check_def.png", plot = p, width=9, height=5)
