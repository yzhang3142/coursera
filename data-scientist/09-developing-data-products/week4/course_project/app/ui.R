#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(navbarPage("Explore IMDB 5000 Movies",
                   tabPanel("Home",
                            sidebarLayout(
                              sidebarPanel(
                                sliderInput("sliderYear", "Choose Release Year Range", 
                                            1990, 2016, value = c(1920, 2016)),
                                sliderInput("sliderBudget", "Choose the Budget Range (Millions)",
                                            0, 300, value = c(0, 300)),
                                p(tags$strong("Note: "), 
                                  "Due to the large amount of computation, the plots may be slow to render.")
                              ),
                              mainPanel(
                                tabsetPanel(
                                  tabPanel("Annual Production", plotOutput("apPlot")),
                                  tabPanel("Annual Production by Genre", plotOutput("agpPlot")),
                                  tabPanel("Profitbility Ratio", plotOutput("prPlot")),
                                  tabPanel("IMDB Score", plotOutput("imdbPlot"))
                                )
                              )
                            )
                   ),
                   tabPanel("About",
                            fluidRow(
                              column(1),
                              column(10,
                                     div(p("In this project, I am going to use the \"IMDB 5000 Movies\" 
                                           dataset to explore the changes of the US movie market including 
                                           annual production, movie type, and revenue. In addtion, I am 
                                           going to focus on studying the profitibality ratio and IMDB scores 
                                           to show the best types of movies to invest based the budget.")
                                         ),
                                     div(h3("INPUT"), 
                                         tags$ul(
                                           tags$li(
                                             tags$strong("Release Year Slider"), 
                                             ": choose the year range for movie release."
                                           ),
                                           tags$li(
                                             tags$strong("Budget Slider"), 
                                             ": choose the budget range for movie production."
                                           )
                                         )
                                     ),
                                     div(h3("OUTPUT"), 
                                         tags$ul(
                                           tags$li(
                                             tags$strong("Annual Production"), 
                                             ": a bar plot shows the total movie production per year."
                                             ),
                                           tags$li(
                                             tags$strong("Annual Genre Production"), 
                                             ": a facet bar plot shows the total movie production per year by genre."
                                           ),
                                           tags$li(
                                             tags$strong("Annual Production"), 
                                             ": a scatter plot shows the profitbility ratio of movies by genre."
                                           ),
                                           tags$li(
                                             tags$strong("Annual Production"), 
                                             ": a box plot shows the distribution of IMDB scores of movies by genre."
                                           )
                                       )
                                     )
                              ),
                              column(1)
                            ))
                   )
)
