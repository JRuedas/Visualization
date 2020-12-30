movies <- read.csv(file="data/IMDb_movies.csv", header=TRUE, sep=",")
movies <- na.omit(movies)

budget_splitted <- strsplit(movies$budget, "\\s+")
currency <- unlist(sapply(budget_splitted, head, 1))
unique(currency)
budget <- as.numeric(unlist(sapply(budget_splitted, tail, 1)))

convert <- function(currency_column, value_column){
  result <- NULL
  for (i in 1:length(currency_column)){
    result[i] <- switch(currency_column[i],
           "DEM" <- value_column[i] * 10 )
  }
  result
}
