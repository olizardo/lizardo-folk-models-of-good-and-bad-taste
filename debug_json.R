library(jsonlite)
topics_data <- fromJSON("good_taste_model/topics.json")$topic_representations
print(class(topics_data[["0"]]))
print(dim(topics_data[["0"]]))
print(head(topics_data[["0"]]))
