library(shiny)
library(tidyverse)
library(DT)

# read datasets
futureFights <- readRDS("~/GitHub/MMAscraper/Shiny App/MatchPredictor/data/futureFights.RDS")
fightMetricsEventOdds <- readRDS("~/GitHub/MMAscraper/Shiny App/MatchPredictor/data/fightMetricsEventOdds.rds") %>%
  mutate(Date = as.character(Date),
         r1b = as.integer(r1b),
         r2b = as.integer(r2b),
         odds = as.integer(odds)) %>%
  arrange(desc(match_id))
filtfightsOdds <- readRDS("~/GitHub/MMAscraper/Shiny App/MatchPredictor/data/filtfightsOdds.rds")

# extra functions
with_plus <- function(x, ...) {
  if (x > 0) {
    sprintf(fmt = "+%s", format(x, ...))
  } else {
    x
  }
}

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
      select(Result, Fighter2, Date, Method, Method_d, R, Org, Date, r1b, r2b, odds) %>%
      rename(Opponent = Fighter2) %>%
      datatable(options = list(pageLength = 25)) %>%
      formatStyle("Result", 
                  backgroundColor = styleEqual(c("win","draw","loss"),
                                               c("lawngreen","silver","lightpink")))
  })
  
  dataset2_past <- reactive({
    fightMetricsEventOdds %>% 
      filter(Link1 == dataset1() %>% pull(Link2)) %>% 
      select(Result, Fighter2, Date, Method, Method_d, R, Org, Date, r1b, r2b, odds) %>%
      rename(Opponent = Fighter2) %>%
      datatable(options = list(pageLength = 25)) %>%
      formatStyle("Result", 
                  backgroundColor = styleEqual(c("win","draw","loss"),
                                               c("lawngreen","silver","lightpink")))
  })
  
  output$name1 <- renderUI({
    dataset1() %>% pull(Name1)
    })
  
  output$name2 <- renderUI({
    dataset2() %>% pull(Name1)
    })
  
  output$rating1 <- renderUI({
    paste0("<b>Rating: </b>", dataset1() %>% pull(r1b), 
           " (", dataset1() %>% pull(ratIncrease1) %>% with_plus, ")") %>% 
      HTML
    })
  
  output$rating2 <- renderUI({
    paste0("<b>Rating: </b>", dataset2() %>% pull(r1b), 
           " (", dataset2() %>% pull(ratIncrease1) %>% with_plus, ")") %>% 
      HTML
    })
  
  output$age1 <- renderUI({
    paste0("<b>Age: </b>",dataset1() %>% pull(Age1) %>% floor, " yrs ", 
           dataset1() %>% pull(Age1) %>% -as.integer(.) %>% '*'(365) %>% as.integer, " days") %>%
      HTML
  })
  
  output$age2 <- renderUI({
    paste0("<b>Age: </b>",dataset2() %>% pull(Age1) %>% floor, " yrs ", 
           dataset2() %>% pull(Age1) %>% -as.integer(.) %>% '*'(365) %>% as.integer, " days") %>%
      HTML
  })
  
  output$record1 <- renderUI({
    paste0("<b>Record: </b>", dataset1() %>% select(wins1, loss1, draw1) %>% as.integer %>% paste(collapse="-"),
             " (", dataset1() %>% pull(nc1), ")") %>%
      HTML
  })
  
  output$record2 <- renderUI({
    paste0("<b>Record: </b>", dataset2() %>% select(wins1, loss1, draw1) %>% as.integer %>% paste(collapse="-"),
           " (", dataset2() %>% pull(nc1), ")") %>%
      HTML
  })
  
  output$odds1 <- renderUI({
    paste0("<b>Odds: </b>", dataset1() %>% pull(odds)) %>% HTML
  })
  
  output$odds2 <- renderUI({
    paste0("<b>Odds: </b>", dataset2() %>% pull(odds)) %>% HTML
  })
  
  output$event <- renderUI({
    paste(dataset1() %>% pull(Event))
  })
  
  output$date <- renderUI({
    paste(dataset1() %>% pull(Date))
  })
  
  output$pastFights1 <- renderDataTable({
    dataset1_past()
  })
  
  output$pastFights2 <- renderDataTable({
    dataset2_past()
  })
  
  analyseData <-  reactive({
    filtfightsOdds %>%
      filter(eval(parse(text=input$filter_text))) %>%
      select(Link1, Link2, Result, Method, Date, Event, odds, r1b, r2b, Age1, Age2, highestWin1_5, highestWin2_5) %>% 
      mutate(bet = 10,
             Age1 = round(Age1, 2),
             Age2 = round(Age2, 2),
             winnings = ifelse(Result %in% c("NC", "draw"), 0, ifelse(odds>0,ifelse(Result=="win", odds*bet/100, -bet),
                                                                      ifelse(Result=="win", -100/odds * bet, -bet))) %>%
               round(2)
      ) %>%
      arrange(desc(Date))
    })
  
  output$dataset_filt <- renderDataTable({
    analyseData()
  })
  
  output$returns <- renderPrint({
    analyseData() %>%
    summarise(avgWin = sum(winnings)/n(),
              wins = sum(Result=="win"),
              count = n(),
              winPer = paste0(round(wins/count * 100,2),"%"),
              bet = mean(bet),
              ROI = paste0(round(sum(winnings)/(bet*n())*100, 2), "%"))
  })
  
}