library(shiny)
library(tidyr)
library(dplyr)
library(plotly)
packageVersion('plotly')
library(quantmod)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  # Read in raw data files
  seinfeld_data <- read.csv("../../outputData/seinfeld.csv")
  scripts <- read.csv("../../outputData/cleaned_scripts.csv")
  summary_scripts <- read.csv("../../outputData/merged_scripts.csv")
  
  # Create first line plot
  output$episodeLine <- renderPlotly({
    
    add_lines(y = scripts$CompSent, name = "Compound Sentiment") %>%
      layout(
        title = "Compound Sentiment",
        xaxis = list(
          rangeselector = list(
            buttons = list(
              list(
                count = 3,
                label = "3 mo",
                step = "month",
                stepmode = "backward"),
              list(
                count = 6,
                label = "6 mo",
                step = "month",
                stepmode = "backward"),
              list(
                count = 1,
                label = "1 yr",
                step = "year",
                stepmode = "backward"),
              list(
                count = 1,
                label = "YTD",
                step = "year",
                stepmode = "todate"),
              list(step = "all"))),
          
          rangeslider = list(type = "date")),
        
        yaxis = list(title = "Price"))
    
  })
  
})
