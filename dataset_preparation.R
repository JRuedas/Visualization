## First specify the packages of interest
packages = c("rjson")

## Now load or install & load all
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

######################## DATA PREPARATION ######################## 

# Aux functions
get_name <- function(col) {
  lapply(fromJSON(col), function(x) x$name)
}

parseJSON <- function(column) {
  lapply(column, get_name) # Do not use column as name
}

as_decade <- function(year) {
  year - (year %% 10)
}

# Read CSV file
movies <- read.csv(file="movies/data/tmdb_5000_movies.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)

# Removes useless columns
keep <- c("budget", "genres", "keywords", "production_companies", "production_countries", "revenue", "vote_average", "title", "release_date")
dataset <- movies[keep]

# Removes missing values
dataset <- dataset[!duplicated(dataset$title), ]
dataset <- dataset[dataset$budget != 0,]
dataset <- dataset[dataset$revenue != 0,]
dataset <- dataset[dataset$genres != "[]",]
dataset <- dataset[dataset$production_countries != "[]",]
dataset <- dataset[dataset$keywords != "[]",]
dataset <- dataset[dataset$production_companies != "[]",]

# Parse date to new columns
dataset$release_date <- as.Date(dataset$release_date)
dataset$year <- format(dataset$release_date, format="%Y")
dataset$month <- format(dataset$release_date, format="%m")
dataset$day <- format(dataset$release_date, format="%d")

# Remove films prior to 1970
dataset <- dataset[dataset$year >= 1970,]

# Create earnings column
dataset$earnings <- dataset$revenue - dataset$budget

# Parse JSON columns
dataset$genres <- parseJSON(dataset$genres)
dataset$keywords <- parseJSON(dataset$keywords)
dataset$production_companies <- parseJSON(dataset$production_companies)
dataset$production_countries <- parseJSON(dataset$production_countries)

# Flatten the lists into a compact values to write into CSV
dataset$genres <- vapply(dataset$genres, paste, collapse = ", ", character(1L))
dataset$keywords <- vapply(dataset$keywords, paste, collapse = ", ", character(1L))
dataset$production_companies <- vapply(dataset$production_companies, paste, collapse = ", ", character(1L))
dataset$production_countries <- vapply(dataset$production_countries, paste, collapse = ", ", character(1L))

# Create decade column
dataset$decade <- factor(sapply(as.numeric(dataset$year), as_decade))

# Saves CSV
write.csv(x = dataset, file="movies/data/cleaned_tmdb_5000_movies.csv")

###################### END DATA PREPARATION ######################
