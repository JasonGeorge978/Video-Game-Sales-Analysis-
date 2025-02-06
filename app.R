library(shiny)
library(bslib)
library(ggplot2)
library(tidyverse)
library(shinythemes)
library(plotly)
library(shinycssloaders)
library(colourpicker)


data <- read.csv("Total_Top_500.csv")
data$Franchise <- str_to_title(data$Franchise)

ui <- fluidPage(
  theme = shinytheme("superhero"),
  titlePanel("Top 500 Video Game Sales Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      h3("How to use:"),
      
      p("Select the variable(s) you want to analyze.
      Then the app will make a graph based on the chosen variable(s).
      Choosing a franchise will isolate a single franchise to analyze, and lastly the chosen color will change the color of your graph."),
      
      selectInput("v1","Select a Variable:", choices = c("","Rank","Platform","Genre","North American Sales"= "NA_Sales","European Sales"="EU_Sales","Japan Sales"="JP_Sales","Other Sales"= "Other_Sales","Global Sales"="Global_Sales"),selected = "Rank"),
      br(),
      
      selectInput("v2","Select a Second Variable:", choices = c("None")),
      br(),
      
      selectInput("franchise","Select a Franchise:", choices = c("All",unique(data$Franchise))),
      br(),
      
      colourInput("color","Select Graph Color:","tomato"),
      
      checkboxGroupInput("dstats","Choose Descriptive Statistic:", 
                         choices = list("Show Mean" = 1,"Show Median"= 2,"Show Standard Deviation" = 3, "Show Correlation" = 4,"Show Max"= 5),selected = NULL)
    ),
    
    mainPanel(
      h3("Outcome Graph",align='center'),
  
      plotlyOutput("graph") %>% withSpinner(color = "#268bd2"),
      
      hr(),
      
      fluidRow(
        column(6, 
               verbatimTextOutput("mean"),
               br(),
               verbatimTextOutput("median"),
               br(),
               verbatimTextOutput("sd")
        ),
        column(6, 
               verbatimTextOutput("correlation"),
               br(),
               verbatimTextOutput("max")
        )
      )
    )
  )
)
server <- function(input, output,session) {
  franchise_data <- reactive({
    if (input$franchise == "All"){
      data
    }
    else {
      data[data$Franchise == input$franchise,]
    }
  })
  
  observeEvent(input$v1, {
    if (input$v1 %in% c("Platform","Genre")) {
      updateSelectInput(session, "v2",
                        choices = c("None", "Rank","North American Sales"= "NA_Sales","European Sales"="EU_Sales","Japan Sales"="JP_Sales","Other Sales"= "Other_Sales","Global Sales"="Global_Sales")
      ) 
    } else {
      updateSelectInput(session, "v2",
                        choices = c("None","Rank","Platform","Genre","North American Sales"= "NA_Sales","European Sales"="EU_Sales","Japan Sales"="JP_Sales","Other Sales"= "Other_Sales","Global Sales"="Global_Sales")
      ) 
    }
  })
  
  output$graph <- renderPlotly({
    data <- franchise_data()
    
    if(input$v1 != "" && input$v2== "None") {
      if(input$v1 %in% c("Platform","Genre")){
        plot <- ggplot(data) + geom_bar(aes_string(x=input$v1),fill=input$color,color="black") + labs(title= paste("Counts of",input$v1),x= input$v1, y="Count") + theme_classic()
      }
      else{
        plot <- ggplot(data) + geom_histogram(aes_string(x=input$v1),fill=input$color,color="black") + labs(title= paste("Frequency of",input$v1),x= input$v1, y="Frequency")+ theme_classic()
      }
    }
    
    else if(input$v1 != "" && input$v2 != "None"){
      if(input$v1 %in% c("Platform", "Genre") || input$v2 %in% c("Platform", "Genre")){
        plot <- ggplot(data) + geom_col(aes_string(x=input$v1, y= input$v2),fill= input$color) + labs(title= paste(input$v2,"based on",input$v1),x= input$v1, y=input$v2) + theme_classic()
      }
      else {
        plot <- ggplot(data) + geom_point(aes_string(x=input$v1, y=input$v2),color= input$color) + labs(title= paste(input$v2,"based on",input$v1),x= input$v1, y=input$v2) + theme_classic()
      }
    }
    ggplotly(plot)
  })
  output$mean <- renderPrint({
    req(1 %in% input$dstats)
    data <- franchise_data()
    
    if (input$v2 == "None") {
      if (input$v1 %in% c("Platform", "Genre")) {
        mean_value <- round(mean(table(data[[input$v1]])),2)
        cat(paste("Mean of", input$v1, ":", mean_value, "\n"))
      } else{
        mean_value <- round(mean(data[[input$v1]]), 2)
        cat(paste("Mean of", input$v1, ":", mean_value, "\n"))
      }
    }
    else{
      cat("Mean: Not available when multiple variables are chosen.\n")
    }
  })
  
  output$median <- renderPrint({
    req(2 %in% input$dstats)
    data <- franchise_data()
    
    if (input$v2 == "None") {
      if (input$v1 %in% c("Platform", "Genre")) {
        median_value <- round(median(table(data[[input$v1]])),2)
        cat(paste("Median of", input$v1, ":", median_value, "\n"))
      } else{
        median_value <- round(median(data[[input$v1]]), 2)
        cat(paste("Median of", input$v1, ":", median_value, "\n"))
      }
    }
    else{
      cat("Median: Not available when multiple variables are chosen.\n")
    }
  })
  
  output$sd <- renderPrint({
    req(3 %in% input$dstats)
    data <- franchise_data()
    
    if (input$v2 == "None") {
      if (input$v1 %in% c("Platform", "Genre")) {
        sd_value <- round(sd(table(data[[input$v1]])),2)
        cat(paste("Standard Deviation of", input$v1, ":", sd_value, "\n"))
      } else{
        sd_value <- round(sd(data[[input$v1]]), 2)
        cat(paste("Standard Deviation of", input$v1, ":", sd_value, "\n"))
      }
    }
    else{
      cat("Standard Deviation: Not available when multiple variables are chosen.\n")
    }
  })
  
  output$correlation <- renderPrint({
    req(4 %in% input$dstats)
    data <- franchise_data()
    
    if (4 %in% input$dstats && input$v2 != "None") {
      if (input$v1 %in% c("Platform", "Genre") || input$v2 %in% c("Platform", "Genre")) {
        cat("Correlation: Not available when a variable is categorical.\n")
      } else {
        cor_value <- round(cor(data[[input$v1]], data[[input$v2]], use = "complete.obs"), 2)
        cat(paste("Correlation between", input$v1, "and", input$v2, ":", cor_value, "\n"))
      }
    } else if (4 %in% input$dstats) {
      cat("Correlation requires two numeric variables.\n")
    }
  })
  
  output$max <- renderPrint({
    req(5 %in% input$dstats)
    data <- franchise_data()
    
    if (input$v2 == "None") {
      if (input$v1 %in% c("Platform", "Genre")) {
        max_value <- round(max(table(data[[input$v1]])),2)
        names <- names(which.max(table(data[[input$v1]])))
        cat(paste("Max of", input$v1, ":", max_value, "at",names,"\n"))
      } else{
        max_value <- round(max(data[[input$v1]]), 2)
        cat(paste("Max of", input$v1, ":", max_value, "\n"))
      }
    }
    else{
      cat("Max: Not available when multiple variables are chosen.\n")
    }
  })
}


shinyApp(ui = ui, server = server)