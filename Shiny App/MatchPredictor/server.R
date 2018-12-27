library(shiny)
library(tidyverse)
library(data.table)
fightMetricsEvent <- readRDS("data/fightMetricsEvent.rds")
filtfightsOdds <- readRDS("data/filtfightsOdds.rds")

fightMetricsEvent <- fightMetricsEvent %>%
  select(Link1, Result, Link2, Date, Method, Method_d, R, Time, Referee, Org, Event, r1b, r2b, r1a, r2a, Age1, Age2)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  datasetInput <- reactive({
    switch(input$data,
           "filtfightsOdds" = filtfightsOdds,
           "fightMetricsEvent" = fightMetricsEvent)
  })

  
  output$table <- renderDataTable({
    datasetInput()
  })
  
})
