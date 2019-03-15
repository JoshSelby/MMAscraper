navbarPage(
  title = "MMA Predictor",
  tabPanel("Matchups", 
    fluidPage(
      useShinyjs(),
# Header ----
      splitLayout(
        verticalLayout(
          h2(htmlOutput("event", container = span)),
          h3(htmlOutput("date", container = span)),
          splitLayout(
            # Inputs ----
            selectInput("match_num", 
                        label = "Choose a match to display",
                        choices = futureFights %>% 
                          select(rownum, Name1, Name2, Date, Event) %>% 
                          mutate(Date = as.character(Date),
                                 Event = gsub("(.*):(.*)", "\\1", Event)) %>%
                          split(futureFights$rownum) %>% 
                          lapply(function(x)
                            x %>% 
                              head(1) %>% 
                              as.character %>% 
                              paste(collapse = "$") %>% 
                              gsub(pattern = "(.*)\\$(.*)\\$(.*)\\$(.*)\\$(.*)",
                                   replacement = "\\1: \\5 - \\2 vs. \\3 (\\4)")
                          ) %>%  
                          unlist() %>%
                          unname()
                        ,
                        width = "100%"),
            verticalLayout(
              checkboxInput("swapFighter", "Check to swap fighter", value = FALSE),
              checkboxInput("oddsCheck", "Check to analyze on odds", value = FALSE),
              checkboxInput("ageCheck", "Check to analyze on age", value = TRUE),
              checkboxInput("ratCheck", "Check to analyze on rating", value = TRUE)
            ),
            textInput(inputId = "filter_text",
                      label = "Write in Custom Filter",
                      width = "100%"),
            verbatimTextOutput("returns"),
            cellWidths = c("35%", "15%", "25%", "24%")
          )
        ),
        plotlyOutput("eloGraph", height = "200px"),
        cellWidths = c("70%", "30%")
      ),
      tags$head(
        tags$style(HTML("
          .shiny-split-layout > div {
              overflow: visible;
          }
          h2 { margin-top: 0;}
          h3 { margin-top: 0;}
          .checkbox { margin-top: 0px; margin-bottom: 0px;}
          div.form-group { margin-bottom: 0px; }
        "))),
# Records of Fighters ----
          splitLayout(
            splitLayout(
              verticalLayout(
                h3(htmlOutput("name1")),
                htmlOutput("rating1"),
                htmlOutput("age1"),
                htmlOutput("record1"),
                htmlOutput("odds1")
              ),
              column(9, offset=3,
                     div(style = "padding: 0px 0px; margin:0%; zoom:0.9",
                         dataTableOutput("recordTable1")),
                     div(style = "padding: 0px 0px; margin-top:-21px; zoom:0.9",
                         dataTableOutput("drawNCTable1"))
                     )
            ),
            splitLayout(
              verticalLayout(
                h3(htmlOutput("name2")),
                htmlOutput("rating2"),
                htmlOutput("age2"),
                htmlOutput("record2"),
                htmlOutput("odds2")
              ),
              column(9, offset=3,
                     div(style = "padding:0px 0px; margin:0%; zoom:0.9",
                         dataTableOutput("recordTable2")),
                     div(style = "padding:0px 0px; margin-top:-21px; zoom:0.9",
                         dataTableOutput("drawNCTable2"))
                     )
              ),
            cellArgs = list(style = "overflow-x:hidden; padding:6px; border:1px solid silver;")
            ),
# Past fights ----
      splitLayout(
        div(dataTableOutput("pastFights1"), style = "zoom:0.7; padding:6px"),
        div(dataTableOutput("pastFights2"), style = "zoom:0.7; padding:6px"),
        cellArgs = list(style = "overflow-x: hidden; padding: 2px; border: 1px solid silver; margin-top:-1px")
      )
    )
  ),
# Tab Panel ----
  tabPanel("Analyze Filtered Fights", div(dataTableOutput("dataset_filt"), style = "font-size:70%")),
  tabPanel("Compare Fighters",  {
    splitLayout(
      textInput.typeahead(id = "fighter1Search", placeholder = "PLACEHOLDERTEXT",
                          local = topFighters, valueKey = "Link1", tokens = c(1:5000),
                          template = HTML("{{Fighter1}}, {{r1a}}")
                )
    )
  }
  )
)
