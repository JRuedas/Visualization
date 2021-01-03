library("rjson")
movies <- read.csv(file="data/tmdb_5000_movies.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
keep <- c("budget", "genres", "production_countries", "revenue", "runtime", "vote_average", "title", "release_date")
dataset <- movies[keep]
summary(dataset)
dataset <- dataset[dataset$budget != 0,]
dataset <- dataset[dataset$revenue != 0,]
dataset <- dataset[dataset$genres != "[]",]
dataset <- dataset[dataset$production_countries != "[]",]
date <- strsplit(dataset$release_date, "-")
dataset$year <- unlist(lapply(date, `[[`, 1))
dataset$month <- unlist(lapply(date, `[[`, 2))
dataset$day <- unlist(lapply(date, `[[`, 3))
dataset$release_date <- NULL

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
