#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(stringr)
library(shiny)

get_movie_data <- function() {
  # Import data ----------------------------------------------------------------
  movies_file <- "./data/IMDB_5000_Movies_Dataset.csv"
  movies <- read_csv(movies_file)
  
  # Get US movies since 1990 ---------------------------------------------------
  us_movies_since_1990 <- movies %>%
    filter(country == "USA", !is.na(title_year)) %>%
    filter(title_year >= 1990) %>%
    select(movie_title,
           title_year,
           genres,
           imdb_score,
           budget,
           gross
    ) %>%
    mutate(movie_title = str_trim(movie_title),
           budget = budget / 1000000,
           gross = gross / 1000000) %>%
    arrange(title_year)
  
  # Clean up the data ----------------------------------------------------------
  # Calculate the annual mean budge and gross rate
  us_movies_since_1990_financial_stats <- us_movies_since_1990 %>%
    select(movie_title,
           title_year,
           budget,
           gross) %>%
    filter(!is.na(budget)) %>%
    mutate(gross_rate = gross / budget) %>%
    group_by(title_year) %>%
    summarise(budget_ave = mean(budget, na.rm = TRUE),
              gross_rate_ave = sum(gross, na.rm = TRUE) / sum(budget, na.rm = TRUE))
  
  # Fill out the missing budget and gross values in the data frame
  us_movies_since_1990 <- inner_join(us_movies_since_1990, 
                                     us_movies_since_1990_financial_stats, 
                                     by = "title_year") %>%
    mutate(budget = ifelse(is.na(budget), budget_ave, budget),
           gross = ifelse(is.na(gross), budget * gross_rate_ave, gross),
           gross_rate = gross / budget) %>%
    select(-c(budget_ave, gross_rate_ave))
  
  # Create a new data frame that spreads the genres ----------------------------
  genres <- us_movies_since_1990$genres %>% str_split(fixed("|"))
  us_movies_since_1990_spread <- 
    us_movies_since_1990 %>%
    select(-genres) %>%
    .[rep(1:3300, sapply(genres, length)),] %>%
    mutate(genre = reduce(genres, c),
           genre_frac = rep(1 / sapply(genres, length), sapply(genres, length)))
  
  # Return data ----------------------------------------------------------------
  us_movies_since_1990_spread
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  movies <- get_movie_data()
  
  # Get selected movies from input ---------------------------------------------
  selected_movies <- reactive({
    movies %>% 
      filter(title_year >= input$sliderYear[1], 
             title_year <= input$sliderYear[2],
             budget >= input$sliderBudget[1],
             budget <= input$sliderBudget[2])
  })

  output$apPlot <- renderPlot({
    selected_movies() %>%
      group_by(title_year, genre, genre_frac) %>%
      summarise(n = sum(genre_frac)) %>%
      ggplot(aes(x = title_year, y = n)) +
        geom_bar(aes(fill = genre), stat = "identity") +
        labs(title = "Movie Production by Year",
             x = "Year",
             y = "Movie Production") +
        scale_fill_discrete(name = NULL) +
        theme(plot.title = element_text(hjust = .5))
  })
  
  output$agpPlot <- renderPlot({
    selected_movies() %>%
      group_by(title_year, genre) %>%
      summarise(n = sum(genre_frac)) %>%
      ggplot(aes(x = title_year, y = n)) +
        geom_bar(aes(fill = genre), stat = "identity") +
        facet_wrap(~genre) +
        labs(title = "Annual Movie Production by Genre",
             x = "Year",
             y = "Movie Production") +
        theme(plot.title = element_text(hjust = .5),
              legend.position = "none")
  })
  
  output$prPlot <- renderPlot({
    selected_movies() %>%
      group_by(genre) %>%
      summarise(profit_rate = sum(gross_rate > 1) / n()) %>%
      ggplot(aes(x = genre, y = profit_rate)) +
        geom_point(aes(color = genre), size = 2.5) +
        geom_hline(yintercept = 0.5, color = "blue") +
        labs(title = "Profitibality Ratio by Budget Range",
             x = "Genre",
             y = "Profitability Ratio") +
        theme(plot.title = element_text(hjust = .5),
              axis.text.x = element_text(angle = -30, hjust = 0),
              legend.position = "none")
  })
  
  output$imdbPlot <- renderPlot({
    selected_movies() %>%
      ggplot(aes(x = as.factor(genre), y = imdb_score)) +
        geom_boxplot(aes(color = genre)) +
        labs(title = "IMDB Score Distribution by Genre",
             x = "Genre",
             y = "IMDB Score") +
        theme(plot.title = element_text(hjust = .5),
              axis.text.x = element_text(angle = -30, hjust = 0),
              legend.position = "none")
  })
})
