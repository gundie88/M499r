library(shiny)
library(shinydashboard)
library(rhandsontable)
library(readxl)
library(shiny)
library(DT)
library(mosaic)
library(scales)
library(lubridate)
library(plotly)
library(tidyverse)


if (interactive()){
  ui <- dashboardPage(
    dashboardHeader(title = "Point Cloud"),
    dashboardSidebar(),
    dashboardBody(
      tabsetPanel(type = "tabs",
                  tabPanel("Plot",
                           plotlyOutput("plot1", height = "500px")),
                  tabPanel("Data")
                  )
      )
    )

  server <- function(input, output){
    
    df <- reactive({
      
    })
    
    output$plot1 <- renderPlotly({
      
    })
    
    
  }
}

shinyApp(ui=ui, server = server)
