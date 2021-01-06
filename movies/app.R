# Authors
# José Domínguez Pérez
# Ismael Muñoz Aztout
# Jonatan Ruedas Mora

# Libraries

## If a package is installed, it will be loaded. If any
## are not, the missing package(s) will be installed
## from CRAN and then loaded.

## First specify the packages of interest
packages = c("shiny", "ggplot2", "dplyr", "wordcloud")

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

# Load data
dataset <- read.csv(file="data/cleaned_tmdb_5000_movies.csv", header=TRUE, sep=",")

companies <- sort(unique(dataset$production_companies))
companies <- c("Choose a company", companies)

countries <- sort(unique(dataset$production_countries))

# Aux Functions
as_decade <- function(year) {
  year - (year %% 10)
}

# User interface ----
ui <- fluidPage(
  titlePanel("Big data: TMDB Movies project"),
  
  sidebarLayout(
    sidebarPanel(
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      selectInput("selected_decade", 
                  label = "Choose a decade to display",
                  choices = seq(1970, 2010, 10),
                  selected = 1970),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      hr(style = "border-top: 1px solid black;"),
      br(),
      br(),
      selectInput("selected_company1", 
                  label = "Company 1",
                  choices = companies,
                  selected = 'Paramount Pictures'),
      selectInput("selected_company2", 
                  label = "Company 2",
                  choices = companies,
                  selected = 'Walt Disney Pictures'),
      selectInput("selected_company3", 
                  label = "Company 3",
                  choices = companies,
                  selected = 'Warner Bros.'),
      selectInput("selected_company4", 
                  label = "Company 4",
                  choices = companies,
                  selected = companies[1]),
      selectInput("selected_company5", 
                  label = "Company 5",
                  choices = companies,
                  selected = companies[1]),
      hr(style = "border-top: 1px solid black;"),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      selectInput("selected_country", 
                  label = "Choose a country to display",
                  choices = countries,
                  selected = "United States of America"),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br()
    ),
    
    mainPanel(
      h4("First Question", align = "center"),
      plotOutput("heat_map"),
      hr(style = "border-top: 1px solid black;"),
      h4("Second Question", align = "center"),
      plotOutput("line_char"),
      hr(style = "border-top: 1px solid black;"),
      plotOutput("wordcloud"),
      br(),
      br(),
      br()
    )
  )
)

# Server logic
server <- function(input, output) {
  
  ################# FIRST QUESTION #################
  
  output$heat_map <- renderPlot({
    selected_decade <- input$selected_decade
    
    # Aggregate earnings by year and month from selected decade
    earnings_by_year <- dataset %>%
      filter(decade == selected_decade) %>%
      group_by(month, year) %>%
      summarise(total = mean(earnings)) %>%
      arrange(year, month)
    
    ggplot(earnings_by_year, aes(earnings_by_year$year, as.factor(earnings_by_year$month))) +
      geom_tile(aes(fill = earnings_by_year$total), colour="white") +
      scale_fill_gradient(low="light green", high="dark green") +
      labs(x = "Years", y = "Months", fill = "Earnings") +
      theme_light()
  })
  
  # Note: negative profits are shown as 0 earnings
  
  # Several conclusions:
  # 1) Most beneficial in summer, maybe because people have more time to go to cinema
  # 2) Moreover, earnings are increasing over time
  
  ############### END FIRST QUESTION ###############
  
  ############### SECOND QUESTION ###############
  
  output$line_char <- renderPlot({
    
    selected_companies = c(
      input$selected_company1,
      input$selected_company2,
      input$selected_company3,
      input$selected_company4,
      input$selected_company5
    )
    
    rating_by_year <- dataset %>%
      filter(production_companies %in% selected_companies) %>%
      group_by(year, production_companies) %>%
      summarise(total = sum(vote_average))
    
    ggplot(rating_by_year, aes(rating_by_year$year, rating_by_year$total,
                               group=unlist(rating_by_year$production_companies),
                               color=unlist(rating_by_year$production_companies))) +
      geom_line() +
      labs(x = "Year", y = "Average") +
      scale_color_discrete(name="Companies") +
      theme_light()
  })
  
  ############# END SECOND QUESTION #############
  
  ############### THIRD QUESTION ###############
  
  output$wordcloud <- renderPlot({
    
    genres_by_year <- dataset %>%
      filter(production_countries %in% input$selected_country) %>%
      group_by(production_countries,genres) %>%
      count(genres)
    
    set.seed(1234)
    wordcloud(words = genres_by_year$genres, freq = genres_by_year$n, min.freq = 1,
              max.words=200, random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Dark2"))
  })
  ############# END THIRD QUESTION #############
}

# Run the app
shinyApp(ui, server)
