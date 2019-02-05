library(shiny)
library(tidyverse)

futureFights <- readRDS("~/GitHub/MMAscraper/Shiny App/MatchPredictor/data/futureFights.RDS")
fightMetricsEventOdds <- readRDS("~/GitHub/MMAscraper/Shiny App/MatchPredictor/data/fightMetricsEventOdds.rds") %>%
  mutate(Date = as.character(Date),
         r1b = as.integer(r1b),
         r2b = as.integer(r2b),
         odds = as.integer(odds)) %>%
  arrange(desc(match_id))

function(input, output) {
  
  dataset1 <- reactive({
    futureFights %>%
    mutate_at(c("Date", "BD1", "BD2"), as.character) %>%
    filter(rownum == input$match_num) %>% head(1)
  })
  
  dataset2 <- reactive({
    futureFights %>%
      mutate_at(c("Date", "BD1", "BD2"), as.character) %>%
      filter(rownum == input$match_num) %>% tail(1)
  })
  
  dataset1_past <- reactive({
    fightMetricsEventOdds %>% 
      filter(Link1 == dataset1() %>% pull(Link1)) %>% 
      select(Result, Fighter2, Date, Method, Method_d, Org, Date, r1b, r2b, odds) %>%
      rename(Opponent = Fighter2)
  })
  
  dataset2_past <- reactive({
    fightMetricsEventOdds %>% 
      filter(Link1 == dataset1() %>% pull(Link2)) %>% 
      select(Result, Fighter2, Date, Method, Method_d, Org, Date, r1b, r2b, odds) %>%
      rename(Opponent = Fighter2)
  })
  
  output$name1 <- renderText({
    dataset1() %>% pull(Name1)
    })
  
  output$name2 <- renderText({
    dataset2() %>% pull(Name1)
    })
  
  output$rating1 <- renderText({
    paste0("Rating: ", dataset1() %>% pull(r1b))
    })
  
  output$rating2 <- renderText({
    paste0("Rating: ", dataset2() %>% pull(r1b))
    })
  
  output$age1 <- renderText({
    paste0("Age: ", dataset1() %>% pull(Age1) %>% floor, " yrs ", 
           dataset1() %>% pull(Age1) %>% -as.integer(.) %>% '*'(365) %>% as.integer, " days")
  })
  
  output$age2 <- renderText({
    paste0("Age: ", dataset2() %>% pull(Age1) %>% floor, " yrs ", 
           dataset2() %>% pull(Age1) %>% -as.integer(.) %>% '*'(365) %>% as.integer, " days")
  })
  
  output$record1 <- renderText({
    paste0("Record: ", dataset1() %>% select(wins1, loss1, draw1) %>% as.integer %>% paste(collapse="-"),
             " (", dataset1() %>% pull(nc1), ")")
  })
  
  output$record2 <- renderText({
    paste0("Record: ", dataset2() %>% select(wins1, loss1, draw1) %>% as.integer %>% paste(collapse="-"),
           " (", dataset2() %>% pull(nc1), ")")
  })
  
  output$odds1 <- renderText({
    paste0("Odds: ", dataset1() %>% pull(odds))
  })
  
  output$odds2 <- renderText({
    paste0("Odds: ", dataset2() %>% pull(odds))
  })
  
  output$event <- renderText({
    paste(dataset1() %>% pull(Event))
  })
  
  output$date <- renderText({
    paste(dataset1() %>% pull(Date))
  })
  
  output$pastFights1 <- renderTable({
    dataset1_past()
  })
  
  output$pastFights2 <- renderTable({
    dataset2_past()
  })
}