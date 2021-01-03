movies <- read.csv(file="data/tmdb_5000_movies.csv", header=TRUE, sep=",")
keep <- c("budget", "genres", "production_countries", "revenue", "runtime", "vote_average", "title")
dataset <- movies[keep]
summary(dataset)
dataset <- dataset[dataset$budget != 0,]
dataset <- dataset[dataset$revenue != 0,]
dataset <- dataset[dataset$genres != "[]",]
dataset <- dataset[dataset$production_countries != "[]",]

parseJSON <- function(column) {
  result <- NULL
  for (i in 1:length(column)) {
    result[i] <- as.data.frame(fromJSON(column[i])[1])$name
  }
  result
}

dataset$genres <- parseJSON(dataset$genres)
dataset$production_countries <- parseJSON(dataset$production_countries)

write.csv(x = dataset, file="data/cleaned_tmdb_5000_movies.csv")
