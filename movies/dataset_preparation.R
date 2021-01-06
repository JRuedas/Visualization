## First specify the packages of interest
packages = c("rjson", "ggplot2", "dplyr", "wordcloud")

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
  y <- sapply(fromJSON(col), function(x) x$name)
  y[1]
}

parseJSON <- function(column) {
  sapply(column, get_name, USE.NAMES = FALSE) # Do not use column as name
}

as_decade <- function(year) {
  year - (year %% 10)
}

# Read CSV file
movies <- read.csv(file="data/tmdb_5000_movies.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)

# Removes useless columns
keep <- c("budget", "genres", "keywords", "production_companies", "production_countries", "revenue", "runtime", "vote_average", "title", "release_date")
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

# Create earnings column
dataset$earnings <- dataset$revenue - dataset$budget

# Parse JSON columns
dataset$genres <- parseJSON(dataset$genres)
dataset$keywords <- parseJSON(dataset$keywords)
dataset$production_companies <- parseJSON(dataset$production_companies)
dataset$production_countries <- parseJSON(dataset$production_countries)

# Create decade column
dataset$decade <- factor(sapply(as.numeric(dataset$year), as_decade))

# Saves CSV
write.csv(x = dataset, file="data/cleaned_tmdb_5000_movies.csv")

###################### END DATA PREPARATION ######################

################# FIRST QUESTION #################

# Show from 1970-1979 to 2010-2019 -> 5 decades

# Aggregate earnings by year and month from selected decade
earnings_by_year <- dataset %>%
                      filter(decade == 2010) %>%
                      group_by(month, year) %>%
                      summarise(total = mean(earnings)) %>%
                      arrange(year, month)

ggplot(earnings_by_year, aes(earnings_by_year$year, as.factor(earnings_by_year$month))) +
  geom_tile(aes(fill = earnings_by_year$total), colour="white") +
  scale_fill_gradient(low="light green", high="dark green") +
  labs(x = "Years", y = "Months", fill = "Earnings") +
  theme_light()

# Note: negative profits are shown as 0 earnings

# Several conclusions:
# 1) Most beneficial in summer, maybe because people have more time to go to cinema
# 2) Moreover, earnings are increasing over time

############### END FIRST QUESTION ###############

############### SECOND QUESTION ###############

a <- c('Paramount Pictures','Marvel Studios','DC Comics','Warner Bros.','Walt Disney Pictures') 
rating_by_year <- dataset %>%
  filter(production_companies %in% a) %>%
  group_by(year, production_companies) %>%
  summarise(total = mean(vote_average))

ggplot(rating_by_year, aes(rating_by_year$year, rating_by_year$total,group=unlist(rating_by_year$production_companies),color=unlist(rating_by_year$production_companies))) +
  geom_line() +
  labs(x = "Year", y = "Average") +
  scale_color_discrete(name="Companies") +
  scale_x_discrete(breaks = seq(min(as_decade(as.numeric(rating_by_year$year))),
                                max(as_decade(as.numeric(rating_by_year$year))),
                                10)) +
  theme_light()

############# END SECOND QUESTION #############

############### THIRD QUESTION ###############

b <- c('United States of America')

genres_by_year <- dataset %>%
  filter(production_countries %in% b) %>%
  group_by(production_countries,genres) %>%
  count(genres)

set.seed(1234)
wordcloud(words = genres_by_year$genres, freq = genres_by_year$n, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

############# END THIRD QUESTION #############