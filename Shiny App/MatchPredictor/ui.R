library(shiny)
library(tidyverse)
library(DT)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  fluidRow(
    column(3,
           # Application title
           titlePanel("MMA Match Predictor"),
           # Sidebar with a slider input for number of bins 
           selectInput("data", h4("select data"),
                       choices=c("All Fights" = "fightMetricsEvent",
                                 "Fights with Odds" = "filtfightsOdds"))
           
    ),
    column(9,
           # Show a plot of the generated distribution
           dataTableOutput("table")
           
    )
  )
  )
)
