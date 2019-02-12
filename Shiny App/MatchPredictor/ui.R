navbarPage(
  title = "MMA Predictor",
  tabPanel("Matchups", 
    fluidPage(
      h2(htmlOutput("event", container = span)),
      h3(htmlOutput("date", container = span)),
      splitLayout(
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
        textInput(inputId = "filter_text",
                  label = "Write in Custom Filter",
                  value = "TRUE",
                  width = "100%"),
        verbatimTextOutput("returns"),
        cellWidths = c("30%", "30%", "40%")
        ), 
      tags$head(tags$style(HTML(".shiny-split-layout > div {
            overflow: visible;
          }
        "))),
      splitLayout(
        verticalLayout(
          splitLayout(
            verticalLayout(
              h3(htmlOutput("name1")),
              htmlOutput("rating1"),
              htmlOutput("age1"),
              htmlOutput("record1"),
              htmlOutput("odds1")
            ),
            dataTableOutput("recordTable1")
          ),
          div(dataTableOutput("pastFights1"), style = "font-size:70%")
          ),
        verticalLayout(
          splitLayout(
            verticalLayout(
              h3(htmlOutput("name2")),
              htmlOutput("rating2"),
              htmlOutput("age2"),
              htmlOutput("record2"),
              htmlOutput("odds2")
            ),
            dataTableOutput("recordTable2")
          ),
          div(dataTableOutput("pastFights2"), style = "font-size:70%")
          ),
        cellArgs = list(style = "overflow-x: hidden; padding: 6px; border: 1px solid silver;")
        )
      )
    ),
  tabPanel("Analyze Filtered Fights", div(dataTableOutput("dataset_filt"), style = "font-size:70%"))
  )
