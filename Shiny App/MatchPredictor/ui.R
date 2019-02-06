navbarPage(
  title = "MMA Predictor",
  tabPanel("Matchups", 
    fluidPage(
      h2(htmlOutput("event", container = span)),
      h3(htmlOutput("date", container = span)),
      splitLayout(
        numericInput(inputId = "match_num",
                     label = "Match to view:",
                     value = 1),
        textInput(inputId = "filter_text",
                  label = "Write in Custom Filter",
                  width = "100%"),
        verbatimTextOutput("returns"),
        cellWidths = c("10%", "40%", "50%")
        ),
      splitLayout(
        verticalLayout(
          h3(htmlOutput("name1")),
          htmlOutput("rating1"),
          htmlOutput("age1"),
          htmlOutput("record1"),
          htmlOutput("odds1"),
          div(dataTableOutput("pastFights1"), style = "font-size:80%")
          ),
        verticalLayout(
          h3(htmlOutput("name2")),
          htmlOutput("rating2"),
          htmlOutput("age2"),
          htmlOutput("record2"),
          htmlOutput("odds2"),
          div(dataTableOutput("pastFights2"), style = "font-size:80%")
          ),
        cellArgs = list(style = "overflow-x: hidden; padding: 6px; border: 1px solid silver;")
        )
      )
    ),
  tabPanel("Analyze Filtered Fights", dataTableOutput("dataset_filt"))
  )