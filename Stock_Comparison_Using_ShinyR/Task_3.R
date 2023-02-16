
library(shiny)
  library(readr)
  library(dplyr)
  library("ggplot2")
  library(ggrepel)
  library(scales)
  library(gridExtra)
  library(lubridate)
  #setwd("C:/Users/Kiran/Desktop/La Trobe/Semester 3/Visual Analytics/Assignment 2")
  big_stock <- read.csv("big_stock_data.csv")
  big_stock$date<- as.Date(parse_date_time(big_stock$date, "dmy"))


  ui <- fluidPage(
    titlePanel("stock price change over time"),

    sidebarPanel(
                 checkboxGroupInput("company", label = "Please select a Company",
                                    choices = list("Apple" = 'Apple',
                                                 "Google" = 'Google',
                                                   "Microsoft" = 'Microsoft',
                                                   "Facebook" = 'Facebook',
                                                   "Amazon" =  'Amazon',
                                                   "Alibaba" = 'Alibaba',
                                                   "Intel"  =  'Intel',
                                                   "SAP" ='SAP'),selected = "Apple"),

                 helpText("select company to see changes over time"),
                 selectInput("plottype", "Select what you want to plot", c("Prices" = 'close_price', "Share Volume" = 'volume')),

    ),
    mainPanel('Graph of company comparisons over price or volume',
              plotOutput('stockPlot')

    )
  )

  #server function
  server <- function(input, output) {

    output$stockPlot <- renderPlot({
      if(input$plottype == "Prices"){
      big_stock <- big_stock %>% filter(company %in% input$company)
      ggplot(data = big_stock) +
        geom_line(
          aes_string(x='date', y= input$plottype, group = "company", col = "company")) +
        theme_bw() }
      else{
        big_stock <- big_stock %>% filter(company %in% input$company)
        ggplot(data = big_stock) +
          geom_col(
            aes_string(x='date', y=input$plottype, fill="company")) +
          theme_bw()
      }
    }
    )
  }

  shinyApp(ui, server)
