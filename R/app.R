# Load packages
library(shiny)
library(tidyverse)
library(ggplot2)
library(plotly)
packageVersion('plotly')
library(shinydashboard)
library(dplyr)

require(data.table)

# Read in csv file
clean_scripts <- read.csv("~/GitHub/seinfeld_analysis/seinfeld_analysis/outputData/cleaned_scripts.csv")
merged_scripts <- read.csv("~/GitHub/seinfeld_analysis/seinfeld_analysis/outputData/merged_scripts.csv")
seinfeld_df <- read.csv("~/GitHub/seinfeld_analysis/seinfeld_analysis/outputData/seinfeld.csv")
episode_info <- read.csv("~/GitHub/seinfeld_analysis/seinfeld_analysis/rawData/episode_info.csv")
# copy for chart 2
scripts <- clean_scripts
# Create a list that holds the episode names for use in the drop down episode selector
episode_list <- unique(seinfeld_df$SEID)



# Shiny Dashboard

ui <- dashboardPage(
  skin = "black"
  ,
  dashboardHeader(
    tags$li(class = "dropdown", tags$img(align="right",src="george.png",height="50px")),
    title = "Seinfeld Episode Analysis", 
    titleWidth = 350
  ) # end of dasboardHeader
  ,
  dashboardSidebar(
    width = 350, 
    sidebarMenu(
      menuItem(text="Dashboard",
               tabName = "dashboard", 
               icon = icon("dashboard"),
               selected = TRUE
      ), # end of menuItem for machine learning tab
      menuItem(text="Machine Learning", 
               tabName = "machinelearn", 
               icon = icon("cogs")
      ), # end of menuItem for machine learning tab
      menuItem(text="GitHub Repo", 
               icon = icon("github"), 
               href = "https://github.com/filterlesschris/seinfeld_analysis"
      ) # end of menuTtem for link to github
    ) # end of sidebar menu
  ) # end of dashboardSidebar
  ,
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                box(
                  width = 12,
                  selectInput("Episode",
                              "Select Episode:",
                              choices = paste(episode_list,episode_info$Title, sep=" - "),
                              selected = episode_list[1]),
                  height = 100
                )# end of box
              )# end of fluidRow
              ,
              # start of fluidRow 2
              fluidRow(
                # plot chart 1
                box(width = 12, plotlyOutput("distPlot", height = 400))
              ) # end of fluid row
              ,
              # start of fluid row to output chart 2
              fluidRow(
                box(width = 6,plotlyOutput("appearPlot", height = 300)),
                box(width = 6,plotlyOutput("avgSentPlot", height = 300))
              ) # end of fluid row
      ),
      
      tabItem(tabName = "machinelearn",
              h2("Machine Learning")
      )
    )
    ,
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    )# end of tags$head - This points to the css file
  ) # end of dashboardBody
) # end of dashboardPage

server <- function(input, output) {
  #begin output chart 1
  #output plotas variable name. And that is passed above to the ui
  output$distPlot <- renderPlotly({
    # generate bins based on input$bins from ui.R
    x <- clean_scripts %>% filter(SEID == substr(input$Episode,1,6))
    
    g <- ggplot(data = x, aes(X, CompSent)) + 
      geom_line(color='black', size=0.5, alpha=0.2) + geom_smooth(se=FALSE, color="black", span = 0.5, method = "loess") + 
      labs(x="Position in Script", y="Overall Sentiment") + 
      ggtitle("Overall Sentinment")
    ggplotly(g)
  }) # end of output chart 1

  # begin output chart 2
  output$appearPlot <- renderPlotly({
    # generate bins based on input$bins from ui.R
    episode <- clean_scripts %>% filter(SEID == substr(input$Episode,1,6))
    # code for chart 2
    
    #episode <- subset(scripts, SEID == "S09E18")
    episode <- episode[-c(1,2)]
    
    episode["index"] = seq(length=nrow(episode))
    
    # Initialize empty arrays
    j_count <- NULL
    j_temp <- 0
    j_index <- NULL
    
    g_count <- NULL
    g_temp <- 0
    g_index <- NULL
    
    k_count <- NULL
    k_temp <- 0
    k_index <- NULL
    
    e_count <- NULL
    e_temp <- 0
    e_index <- NULL
    
    s_count <- NULL
    s_temp <- 0
    s_index <- NULL
    
    o_count <- NULL
    o_temp <- 0
    o_index <- NULL
    
    
    for(i in 1:dim(episode)[1]){
      
      if(as.character(episode$Character[i]) == "JERRY") {
        j_temp <- j_temp + 1
        j_count <- c(j_count, j_temp)
        j_index <- c(j_index, i)
      } 
      
      else if (as.character(episode$Character[i]) == "GEORGE") {
        g_temp <- g_temp + 1
        g_count <- c(g_count, g_temp)
        g_index <- c(g_index, i)
      }
      
      else if (as.character(episode$Character[i]) == "KRAMER") {
        k_temp <- k_temp + 1
        k_count <- c(k_count, k_temp)
        k_index <- c(k_index, i)
      }
      
      else if (as.character(episode$Character[i]) == "ELAINE") {
        e_temp <- e_temp + 1
        e_count <- c(e_count, e_temp)
        e_index <- c(e_index, i)
      }
      
      else if (as.character(episode$Character[i]) == "SECONDARY") {
        s_temp <- s_temp + 1
        s_count <- c(s_count, s_temp)
        s_index <- c(s_index, i)
      }
      
      else if (as.character(episode$Character[i]) == "OTHER") {
        o_temp <- o_temp + 1
        o_count <- c(o_count, o_temp)
        o_index <- c(o_index, i)
      }
      
    }
    
    ep_index <- c(j_index, g_index, k_index, e_index, s_index, o_index)
    ep_count <- c(j_count, g_count, k_count, e_count, s_count, o_count)
    
    ep_matrix <- as.matrix(cbind(ep_index, ep_count), ncol = 2)
    
    ep_table <- data.table(ep_matrix, key = "ep_index")
    ep_table
    
    episode$CharLineCount <- ep_table$ep_count
    
    g <- ggplot(episode, aes(x = index, y = CharLineCount)) + 
      geom_line(aes(color = Character)) +
      labs(x="Position in Script", y="Count of Lines") + 
      ggtitle("Character Line Count")
    ggplotly(g)
  })
  #end output chart 2
  
  #start of chart 3 output plot
  output$avgSentPlot <- renderPlotly({
    # Chart 3 - Aggregated average of character sentiment throughout episode
    
    # generate bins based on input$bins from ui.R
    episode <- clean_scripts %>% filter(SEID == substr(input$Episode,1,6))
    
    # code for chart 2
    
    #episode <- subset(scripts, SEID == "S09E18")
    episode <- episode[-c(1,2)]
    
    episode["index"] = seq(length=nrow(episode))
    
    # Initialize empty arrays
    j_sent <- NULL
    j_temp2 <- NULL
    j_index2 <- NULL
    
    g_sent <- NULL
    g_temp2 <- NULL
    g_index2 <- NULL
    
    k_sent <- NULL
    k_temp2 <- NULL
    k_index2 <- NULL
    
    e_sent <- NULL
    e_temp2 <- NULL
    e_index2 <- NULL
    
    s_sent <- NULL
    s_temp2 <- NULL
    s_index2 <- NULL
    
    o_sent <- NULL
    o_temp2 <- NULL
    o_index2 <- NULL
    
    
    for(i in 1:dim(episode)[1]){
      
      ##### JERRY ########################################
      if(as.character(episode$Character[i]) == "JERRY") {
        j_index2 <- c(j_index2, i)
        j_temp2 <- episode$CompSent[i]
        
        if (is.null(j_sent)) {
          j_sent <- j_temp2
        }
        
        j_sent <- c(j_sent, mean(c(j_sent, j_temp2)))
      } 
      
      ##### GEORGE ########################################
      if(as.character(episode$Character[i]) == "GEORGE") {
        g_index2 <- c(g_index2, i)
        g_temp2 <- episode$CompSent[i]
        
        if (is.null(g_sent)) {
          g_sent <- g_temp2
        }
        
        g_sent <- c(g_sent, mean(c(g_sent, g_temp2)))
      } 
      
      ##### KRAMER ########################################
      if(as.character(episode$Character[i]) == "KRAMER") {
        k_index2 <- c(k_index2, i)
        k_temp2 <- episode$CompSent[i]
        
        if (is.null(k_sent)) {
          k_sent <- k_temp2
        }
        
        k_sent <- c(k_sent, mean(c(k_sent, k_temp2)))
      } 
      
      ##### ELAINE ########################################
      if(as.character(episode$Character[i]) == "ELAINE") {
        e_index2 <- c(e_index2, i)
        e_temp2 <- episode$CompSent[i]
        
        if (is.null(e_sent)) {
          e_sent <- e_temp2
        }
        
        e_sent <- c(e_sent, mean(c(e_sent, e_temp2)))
      } 
      
      ##### SECONDARY ######################################
      if(as.character(episode$Character[i]) == "SECONDARY") {
        s_index2 <- c(s_index2, i)
        s_temp2 <- episode$CompSent[i]
        
        if (is.null(s_sent)) {
          s_sent <- s_temp2
        }
        
        s_sent <- c(s_sent, mean(c(s_sent, s_temp2)))
      } 
      
      ##### OTHER ######################################
      if(as.character(episode$Character[i]) == "OTHER") {
        o_index2 <- c(o_index2, i)
        o_temp2 <- episode$CompSent[i]
        
        if (is.null(o_sent)) {
          o_sent <- o_temp2
        }
        
        o_sent <- c(o_sent, mean(c(o_sent, o_temp2)))
      } 
      
    }
    
    ep_index2 <- c(j_index2, g_index2, k_index2, e_index2, s_index2, o_index2)
    ep_sent <- c(j_sent[-1], g_sent[-1], k_sent[-1], e_sent[-1], s_sent[-1], o_sent[-1])
    
    ep_matrix2 <- as.matrix(cbind(ep_index2, ep_sent), ncol = 2)
    
    ep_table2 <- data.table(ep_matrix2, key = "ep_index2")
    
    episode$CharAvgSentiment <- ep_table2$ep_sent
    
    g <- ggplot(episode, aes(x = index, y = CharAvgSentiment)) + 
      geom_line(aes(color = Character)) +
      labs(x="Position in Script", y="Rolling AVG Sentiment") +
      ggtitle("Character Sentiment (AVG)")
    ggplotly(g)
  }) # end of output chart 3
}

shinyApp(ui, server)
