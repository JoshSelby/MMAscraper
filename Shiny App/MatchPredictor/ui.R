fluidPage(
  
  # App title ----
  titlePanel("Reactivity"),
  
  # Sidebar layout with input and output definitions ----
  fluidRow(
    
    # Sidebar panel for inputs ----
    column(2,
      numericInput(inputId = "match_num",
                   label = "Match to view:",
                   value = 1)
      )
    ),
    
   
      h2(textOutput("event", container = span)),
      h3(textOutput("date", container = span)),
      
      
      splitLayout(
        verticalLayout(
          h2(textOutput("name1")),
          h3(textOutput("rating1")),
          h3(textOutput("age1")),
          h3(textOutput("record1")),
          h3(textOutput("odds1")),
          div(tableOutput("pastFights1"), style = "font-size:80%")
          
          ),
        verticalLayout(
          h2(textOutput("name2")),
          h3(textOutput("rating2")),
          h3(textOutput("age2")),
          h3(textOutput("record2")),
          h3(textOutput("odds2")),
          div(tableOutput("pastFights2"), style = "font-size:80%")
        ),
        cellArgs = list(style = "overflow-x: hidden; padding: 6px; border: 1px solid silver;")
      )
      
    )
  