library("rjson")
library("ggplot2")
library("dplyr")

# Aux functions
parseJSON <- function(column) {
  result <- NULL
  for (i in 1:length(column)) {
    result[i] <- as.data.frame(fromJSON(column[i])[1])$name
  }
  result
}

as_decade <- function(year) {
  year - (year %% 10)
}

# Read CSV file
movies <- read.csv(file="data/tmdb_5000_movies.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)

# Removes useless columns
keep <- c("budget", "genres", "production_countries", "revenue", "runtime", "vote_average", "title", "release_date")
dataset <- movies[keep]

# Removes missing values
dataset <- dataset[dataset$budget != 0,]
dataset <- dataset[dataset$revenue != 0,]
dataset <- dataset[dataset$genres != "[]",]
dataset <- dataset[dataset$production_countries != "[]",]

# Parse date to new columns
dataset$release_date <- as.Date(dataset$release_date)
dataset$year <- format(dataset$release_date, format="%Y")
dataset$month <- format(dataset$release_date, format="%m")
dataset$day <- format(dataset$release_date, format="%d")

# Create earnings column
dataset$earnings <- dataset$revenue - dataset$budget

# Parse JSON columns
dataset$genres <- parseJSON(dataset$genres)
dataset$production_countries <- parseJSON(dataset$production_countries)

# Create decade column
dataset$decade <- factor(sapply(as.numeric(dataset$year), as_decade))

# Saves CSV
write.csv(x = dataset, file="data/cleaned_tmdb_5000_movies.csv")

dataset <- dataset[as.numeric(dataset$year) >= 1990 & as.numeric(dataset$year) <= 2019,]

# Aggregate earnings by year and month
earnings_by_year <- dataset %>%
                      group_by(month, year) %>%
                      summarise(total = sum(earnings))

# Aggregate earnings by decade and month
earnings_by_decade <- dataset %>%
                          group_by(year, decade) %>%
                          summarise(total = sum(earnings))

ggplot(data = earnings_by_year, mapping = aes(x = earnings_by_year$year, y = earnings_by_year$month, fill = earnings_by_year$total)) +
  geom_tile() +
  scale_fill_gradient(low="white", high="red") +
  labs(x = "Years", y = "Months", fill = "Earnings") +
  theme_light()

ggplot(data = earnings_by_decade, aes(x = earnings_by_decade$decade, y = earnings_by_decade$year)) +
  geom_tile(aes(fill = earnings_by_decade$total)) +
  scale_fill_gradient(low="white", high="red") +
  labs(x = "Decades", y = "Years", fill = "Earnings") +
  theme_light()
