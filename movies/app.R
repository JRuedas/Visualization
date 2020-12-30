# Authors
# José Domínguez Pérez
# Ismael Muñoz Aztout
# Jonatan Ruedas Mora

# Libraries

## If a package is installed, it will be loaded. If any
## are not, the missing package(s) will be installed
## from CRAN and then loaded.

## First specify the packages of interest
packages = c("shiny")

movies <- read.csv(file="data/IMDb_movies.csv", header=TRUE, sep=",")

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

# User interface ----
ui <- fluidPage(
  titlePanel("Big data: IMDB Movies project"),
  
  sidebarLayout(
    sidebarPanel(
      
      
      br(),
      br(),
    ),
    
    mainPanel()
  )
)

# Server logic
server <- function(input, output) {

}

# Run the app
shinyApp(ui, server)
