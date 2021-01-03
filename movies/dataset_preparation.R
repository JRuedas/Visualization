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
dataset$year <- sapply(date, `[[`, 1)
dataset$month <- sapply(date, `[[`, 2)
dataset$day <- sapply(date, `[[`, 3)
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

as_decade <- function(year) {
  year - (year %% 10)
}

dataset$decade <- sapply(as.numeric(dataset$year), as_decade)
dataset$decade <- factor(dataset$decade)

write.csv(x = dataset, file="data/cleaned_tmdb_5000_movies.csv")

dataset <- dataset[as.numeric(dataset$year) >= 1990 & as.numeric(dataset$year) <= 1999,]

dataset$revenue <- log10(dataset$revenue)

ggplot(data = dataset, mapping = aes(x = year, y = month, fill = revenue)) +
  geom_tile()

ggplot(data = dataset, aes(x = decade, y = month)) +
  geom_tile(aes(fill = revenue))
  