library(shiny)
library(shinysky)
library(data.table)

profvis({
  fightersTable <- readRDS("~/GitHub/MMAscraper/scripts/9-various-tables/data/fightersTable.rds")
  fightMetricsEvent <- readRDS(file = "~/GitHub/MMAscraper/scripts/5-metrics/data/fightMetricsEvent.rds")
  
  shinyUI(basicPage(headerPanel("ShinySky Examples"),  br(),
                    tabsetPanel(selected = "Typeahead",
                                tabPanel("Typeahead",
                                         h4("Typeahead Text Input "),
                                         div(class="row-fluid ", 
                                             div(class="well container-fluid", 
                                                 div(class="container span3",
                                                     helpText("Type Fighter's name"),
                                                     textInput.typeahead(
                                                       id="thti",
                                                       placeholder="Type Fighter's Name",
                                                       local=fightersTable,
                                                       valueKey = "Name",
                                                       tokens=1:nrow(fightersTable),
                                                       template <- HTML("Name: <em>{{Name}}</em> Birthday: <em>{{BDchar}}</em> Rating: <em>{{rating}}</em>")
                                                       
                                                     ),
                                                     actionButton("update_typeahead_btn","Update Typeahead", styleclass= "primary")
                                                 ),
                                                 div(class="container span9"
                                                     ,shinyalert("shinyalert3")
                                                 ))
                                         ))
                                
                    ))
  )
})
