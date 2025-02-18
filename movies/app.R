# Authors
# José Domínguez Pérez
# Ismael Muñoz Aztout
# Jonatan Ruedas Mora

# Libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(wordcloud)

# Load data
dataset <- read.csv(file="data/cleaned_tmdb_5000_movies.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)

################# FIRST QUESTION #################

# Aux Function
as_decade <- function(year) {
  year - (year %% 10)
}

############### END FIRST QUESTION ###############

################# SECOND QUESTION #################

# Get max and min year
max_year <- max(dataset$year)
min_year <- min(dataset$year)

# Unflatten the production companies
ds_companies <- dataset %>%
  unnest(production_companies = strsplit(production_companies, ",")) 

# Trim whitespaces
ds_companies$production_companies <- lapply(ds_companies$production_companies, trimws)

# Select companies and the year they realized a film (without repetitions)
df_companies_dist_years <- ds_companies %>%
  distinct(production_companies, year)

# Count number of years a company has realized a film
df_count <- df_companies_dist_years %>%
  group_by(production_companies) %>%
  summarise(count = n())

# Select companies with less than 5 different years
df_delete <- df_count %>%
  filter(count < 5)

# Delete companies with less than 5 different years
# This is the dataset used for the second question
ds_filter_companies <- ds_companies %>%
  filter(!production_companies %in% df_delete$production_companies)

companies <- sort(unlist(unique(ds_filter_companies$production_companies)))
choose_a_company = "Choose a company"
companies <- c(choose_a_company, companies)

############### END SECOND QUESTION ###############

################# THIRD QUESTION #################

# Unflatten the production countries
ds_countries <- dataset %>%
  unnest(production_countries = strsplit(production_countries, ","))

# Trim production countries whitespaces
ds_countries$production_countries <- lapply(ds_countries$production_countries, trimws)
  
# Unflatten the genres
ds_countries_genres <- ds_countries %>%
  unnest(genres = strsplit(genres, ","))

# Trim genres whitespaces
ds_countries_genres$genres <- lapply(ds_countries_genres$genres, trimws)

# Unflatten the keywords
ds_countries_keywords <- ds_countries %>%
  unnest(keywords = strsplit(keywords, ","))

# Trim keywords whitespaces
ds_countries_keywords$keywords <- lapply(ds_countries_keywords$keywords, trimws)

ds_countries_keywords <- ds_countries_keywords %>%
  filter(nchar(keywords) <= 7)

countries <- sort(unlist(unique(ds_countries$production_countries)))

############### END THIRD QUESTION ###############

# User interface ----
ui <- fluidPage(
  titlePanel(
    h1("Big data: TMDB Movies project", align = "center")
  ),
  
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
                  choices = seq(min(dataset$year), max(dataset$year), 10),
                  selected = 2010),
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
                  selected = choose_a_company),
      selectInput("selected_company5", 
                  label = "Company 5",
                  choices = companies,
                  selected = choose_a_company),
      selectInput("date_range_start", 
                  label = "Select start year", 
                  choices = min_year:max_year,
                  selected = min_year
      ),
      selectInput("date_range_end", 
                  label = "Select end year", 
                  choices = min_year:max_year,
                  selected = max_year
      ),
      br(),
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
      radioButtons("sorted_radio",
                   label = "Sort by frequency",
                   choices = list("Unsorted" = 1, 
                                  "Ascendent" = 2,
                                  "Descendent" = 3),
                   selected = 1),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      sliderInput("num_words",
                  "Number of words:",
                  min = 10, max = 50,
                  value = 30),
      br(),
      br(),
      br()
    ),
    
    mainPanel(
      h4("First Question", align = "center"),
      plotOutput("heat_map"),
      hr(style = "border-top: 1px solid black;"),
      h4("Second Question", align = "center"),
      plotOutput("line_chart"),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      hr(style = "border-top: 1px solid black;"),
      h4("Third Question", align = "center"),
      plotOutput("lollipop_chart"),
      plotOutput("wordcloud"),
      br(),
      br(),
      br()
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  ################# FIRST QUESTION #################
  
  output$heat_map <- renderPlot({
    selected_decade <- input$selected_decade
    
    # Aggregate earnings by year and month from selected decade
    earnings_by_year <- dataset %>%
      filter(decade == selected_decade) %>%
      group_by(month, year) %>%
      summarise(total = mean(earnings)) %>%
      arrange(year, month)
    
    ggplot(earnings_by_year, aes(as.factor(earnings_by_year$year), as.factor(earnings_by_year$month))) +
      geom_tile(aes(fill = earnings_by_year$total), colour="white") +
      scale_fill_gradient(low="light green", high="dark green") +
      labs(x = "Years", y = "Months", fill = "Earnings") +
      theme_light() +
      theme(
        panel.grid = element_blank()
      )
  })
  
  # Note: negative profits are shown as 0 earnings
  
  # Several conclusions:
  # 1) Most beneficial in summer, maybe because people have more time to go to cinema
  # 2) Moreover, earnings are decreasing over time (for every film, not the total sum)
  
  ############### END FIRST QUESTION ###############
  
  ############### SECOND QUESTION ###############
  
  output$line_chart <- renderPlot({
    
    selected_companies = c(
      input$selected_company1,
      input$selected_company2,
      input$selected_company3,
      input$selected_company4,
      input$selected_company5
    )
    
    # Associate number of company with color
    colors <- setNames(c("red", "blue", "green", "brown", "orange"), selected_companies)
    
    ds_filter_year <- ds_filter_companies[as.numeric(ds_filter_companies$year) >= input$date_range_start &
                                                      as.numeric(ds_filter_companies$year) <= input$date_range_end,]
    
    rating_by_year <- ds_filter_year %>%
      filter(production_companies %in% selected_companies) %>%
      group_by(year, production_companies) %>%
      summarise(total = mean(vote_average))
    
    ggplot(rating_by_year, aes(rating_by_year$year, rating_by_year$total,
                               color=factor(unlist(rating_by_year$production_companies)))) +
      geom_line() +
      labs(x = "Years", y = "Rating average") +
      scale_color_manual(name="Companies", values=colors) +
      theme_light()
  })
  
  # Adjust the selection input of the year range to avoid select the same year or an inverse range.
  observe({
    
    min_year_selected <- input$date_range_start
    max_year_selected <- input$date_range_end
    
    updateSelectInput(session, "date_range_start",
                      choices = min_year:(as.numeric(max_year_selected)-1),
                      selected = min_year_selected
    )
    
    updateSelectInput(session, "date_range_end",
                      choices = (as.numeric(min_year_selected)+1):max_year,
                      selected = max_year_selected
    )
  })
  
  ############# END SECOND QUESTION #############
  
  ############### THIRD QUESTION ###############
  
  output$lollipop_chart <- renderPlot({
    
    genres_by_country <- ds_countries_genres %>%
      filter(production_countries %in% input$selected_country) %>%
      group_by(production_countries,genres) %>%
      count(genres)
    
    genres_by_country <- switch(input$sorted_radio,
                                "1" = genres_by_country,
                                "2" = genres_by_country %>% 
                                  arrange(n),
                                "3" = genres_by_country %>% 
                                  arrange(desc(n))
    )
    
    genres_by_country$genres <- factor(genres_by_country$genres, levels = genres_by_country$genres)
    
    ggplot(genres_by_country, aes(x=genres_by_country$genres, y=genres_by_country$n)) +
      geom_segment(aes(x=genres_by_country$genres ,xend=genres_by_country$genres, y=0, yend=genres_by_country$n), color="grey") +
      geom_point(size=4, color="dark green") +
      coord_flip() +
      labs(x = "Genre", y = "Frequency") +
      theme_light() +
      theme(
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank()
      )
  })
  
  output$wordcloud <- renderPlot({
    
    keywords_by_country <- ds_countries_keywords %>%
      filter(production_countries %in% input$selected_country) %>%
      group_by(production_countries,keywords) %>%
      count(keywords)
    
    set.seed(1234)
    wordcloud(words = keywords_by_country$keywords, freq = keywords_by_country$n, min.freq = 5,
              max.words=input$num_words, random.order=FALSE, rot.per=0.55, 
              colors=brewer.pal(8, "Dark2"))
  })
  
  
  ############# END THIRD QUESTION #############
}

# Run the app
shinyApp(ui, server)