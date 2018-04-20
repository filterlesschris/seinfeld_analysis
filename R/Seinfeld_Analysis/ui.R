# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Load packages
library(shiny)
library(tidyverse)
library(ggplot2)
library(plotly)

# Read in csv file
clean_scripts <- read.csv("GitHub/seinfeld_analysis/seinfeld_analysis/outputData/cleaned_scripts.csv")
merged_scripts <- read.csv("GitHub/seinfeld_analysis/seinfeld_analysis/outputData/merged_scripts.csv")
seinfeld_df <- read.csv("GitHub/seinfeld_analysis/seinfeld_analysis/outputData/seinfeld.csv")
episode_info <- read.csv("GitHub/seinfeld_analysis/seinfeld_analysis/rawData/episode_info.csv")

# Create a list that holds the episode names for use in the drop down episode selector
episode_list <- unique(episode_info$SEID)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Seinfeld Episode Analysis"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("Episode",
                  "Select Episode:",
                  choices = paste(episode_list,episode_info$Title, sep=" - "),
                  selected = episode_list[1])
    ),
    
    # Show a plot of the generated distribution
    
    #mainPanel(
    #  plotOutput("distPlot"), plotOutput("distTest")
    #)
    
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #output then some variable name. And that is passed above to the ui
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x <- clean_scripts %>% filter(SEID == substr(input$Episode,1,6))
    
    ggplot(data = x, aes(X, CompSent)) + 
      geom_line() + geom_smooth(method = "lm")
  })
  #output$testPlot <-
}

# Run the application 
shinyApp(ui = ui, server = server)

