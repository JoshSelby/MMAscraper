source('~/GitHub/MMAscraper/Shiny App/MatchPredictor/scriptsForApp.R', echo=TRUE)

function(input, output, session) {
  
  dataset1 <- reactive({
    futureFights %>%
    mutate_at(c("Date", "BD1", "BD2"), as.character) %>%
    filter(rownum == input$match_num %>% 
             gsub("(.*):(.*)", "\\1",.)) %>% head(1)
  })
  
  dataset2 <- reactive({
    futureFights %>%
      mutate_at(c("Date", "BD1", "BD2"), as.character) %>%
      filter(rownum == input$match_num %>% 
               gsub("(.*):(.*)", "\\1",.)) %>% tail(1)
  })
  
  dataset1_past <- reactive({
    fightMetricsEventOdds %>% 
      filter(Link1 == dataset1() %>% pull(Link1)) %>% 
      select(Result, score, Fighter2, Date, Method, Method_d, R, Org, Date, Age1, Age2, r1b, r2b, odds) %>%
      mutate(Org = substr(Org, 1, 13),
             Fighter2 = substr(Fighter2, 1, 20), 
             Method_d = substr(Method_d, 1, 16),
             Age1 = round(Age1, 1),
             Age2 = round(Age2, 1),
             score = round(score, 2)) %>%
      dplyr::rename(Opponent = Fighter2)
  })
  
  dataset2_past <- reactive({
    fightMetricsEventOdds %>% 
      filter(Link1 == dataset1() %>% pull(Link2)) %>% 
      select(Result, score, Fighter2, Date, Method, Method_d, R, Org, Date, Age1, Age2, r1b, r2b, odds) %>%
      mutate(Org = substr(Org, 1, 13),
             Fighter2 = substr(Fighter2, 1, 20), 
             Method_d = substr(Method_d, 1, 16),
             Age1 = round(Age1, 1),
             Age2 = round(Age2, 1),
             score = round(score, 2)) %>%
      dplyr::rename(Opponent = Fighter2)
  })
  
  observeEvent({dataset1()
    input$swapFighter
    input$oddsCheck
    input$ageCheck
    input$ratCheck}, {
    if(input$swapFighter == FALSE) {
      data <- dataset1()
    } else { 
      data <- dataset2()
    }
    Age1 <- data %>% pull(Age1)
    Age2 <- data %>% pull(Age2)
    r1b <- data %>% pull(r1b)
    r2b <- data %>% pull(r2b)
    winper <- line_to_per(data %>% pull(odds))
    
    if(input$oddsCheck == TRUE) {
      oddsCheck <- paste0("between(odds, ",per_to_line(winper+0.05) %>% round(0),", ",
                          per_to_line(winper-0.05) %>% round(0),")")
    } else {
      oddsCheck <- ""
    }
    if(input$ageCheck == TRUE) {
      ageCheck <- paste0("between(Age1-Age2, ",round(Age1-Age2-1.5, 1),", ", round(Age1-Age2+1.5, 1),")")
    } else {
      ageCheck <- ""
    }
    if(input$ratCheck == TRUE) {
      ratCheck <- paste0("between(r1b-r2b, ",round(r1b-r2b-50, 0),", ", round(r1b-r2b+50, 0),")")
    } else {
      ratCheck <- ""
    }
    
    filtersToUse <- c(oddsCheck, ageCheck, ratCheck)[c(oddsCheck, ageCheck, ratCheck)!=""]
    
    updateTextInput(session, "filter_text", value = paste(filtersToUse, collapse = " & "))
  })
  

  
  output$name1 <- renderUI({
    dataset1() %>% pull(Name1)
    })
  
  output$name2 <- renderUI({
    dataset2() %>% pull(Name1)
    })
  
  output$rating1 <- renderUI({
    paste0("<b>Rating: </b>", dataset1() %>% pull(r1b), 
           " (", dataset1() %>% pull(ratIncrease1) %>% with_plus, ")",
           " (", dataset1() %>% pull(ratIncrease1_3) %>% with_plus, ")") %>% 
      HTML
    })
  
  output$rating2 <- renderUI({
    paste0("<b>Rating: </b>", dataset2() %>% pull(r1b), 
           " (", dataset2() %>% pull(ratIncrease1) %>% with_plus, ")",
           " (", dataset2() %>% pull(ratIncrease1_3) %>% with_plus, ")") %>% 
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
    odds1 <- dataset1() %>% pull(odds)
    
    paste0("<b>Odds: </b>", odds1, " (", odds1 %>% 
             line_to_per() %>% round(3) %>% `*`(100), "%), Return on $10: $",
           odds1 %>% odds_to_return() %>% round(2)) %>% HTML
  })
  
  output$odds2 <- renderUI({
    odds2 <- dataset2() %>% pull(odds)
    
    paste0("<b>Odds: </b>", odds2, " (", odds2 %>% 
             line_to_per() %>% round(3)%>% `*`(100), "%), Return on $10: $",
           odds2 %>% odds_to_return() %>% round(2)) %>% HTML
  })
  
  output$event <- renderUI({
    paste(dataset1() %>% pull(Event))
  })
  
  output$date <- renderUI({
    paste(dataset1() %>% pull(Date))
  })
  
  output$pastFights1 <- renderDataTable({
    dataset1_past() %>%
      datatable(options = list(pageLength = 25, dom='tp', scrollX=TRUE)) %>%
      formatStyle("Result", 
                  backgroundColor = styleEqual(c("win","draw","loss"),
                                               c("lawngreen","silver","lightpink"))) %>%
      formatStyle("odds", color = styleInterval(0, c("green", "red")))
  })
  
  output$pastFights2 <- renderDataTable({
    dataset2_past() %>%
      datatable(options = list(pageLength = 25, dom='tp', scrollX=TRUE)) %>%
      formatStyle("Result", 
                  backgroundColor = styleEqual(c("win","draw","loss"),
                                               c("lawngreen","silver","lightpink"))) %>%
      formatStyle("odds", color = styleInterval(0, c("green", "red")))
  })
  
  analyseData <-  reactive({
    filtfightsOdds %>%
      filter(eval(parse(text=input$filter_text))) %>%
      select(Link1, Link2, Result2, Method, Date, Event, odds, r1b, r2b, Age1, Age2, highestWin1_5, highestWin2_5, ratIncrease1, ratIncrease2) %>% 
      filter(Result2 %in% c("win", "loss")) %>%
      mutate(bet = 10,
             Age1 = round(Age1, 2),
             Age2 = round(Age2, 2),
             winnings = if_else(Result2 %in% c("NC", "draw"), 0, if_else(odds>0,if_else(Result2=="win", odds*bet/100, -bet),
                                                                      if_else(Result2=="win", -100/odds * bet, -bet))) %>%
               round(2)
      ) %>%
      arrange(desc(Date))
    })
  
  output$dataset_filt <- renderDataTable({
    analyseData()},
    options = list(pageLength = 15)
  )
  
  output$returns <- renderPrint({
    analyseData() %>%
    summarise(avgWin = sum(winnings)/n(),
              wins = sum(Result2=="win"),
              count = n(),
              winPer = ifelse(count==0, NA, paste0(round(wins/count * 100,2),"%")),
              ROI = paste0(round(sum(winnings)/(mean(bet)*n())*100, 2), "%"))
  })
  
  output$recordTable1 <- renderDataTable({
    
    dataset1_past() %>% 
      mutate(Method = if_else(grepl("KO", Method),"TKO/KO",Method)) %>% 
      group_by(Method, Result) %>% 
      summarise(n=n()) %>% 
      spread(Result, n, fill = 0) %>%
      full_join(tibble(Method = c("TKO/KO", "Submission", "Decision"), 
                       win = 0, 
                       loss = 0,
                       draw = 0)) %>%
      group_by(Method) %>%
      summarise(win = sum(win, na.rm = TRUE),
                loss = sum(loss, na.rm = TRUE)) %>%
      arrange(match(Method, c("TKO/KO", "Submission", "Decision"))) %>%
      filter(Method %in% c("TKO/KO", "Submission", "Decision")) %>%
      datatable(options = list(dom = 't', ordering = F, columnDefs = list(list(className = 'dt-center', targets = 1:2)),
        initComplete = JS(
        "function(settings, json) {", 
        "$(this.api().table().header()).find('th').css({'padding': '2px'});", 
        "}")), rownames = F) %>%
      formatStyle("win", backgroundColor = "lawngreen") %>%
      formatStyle("loss", backgroundColor = "lightpink") 
  })
  
  output$drawNCTable1 <- renderDataTable({
    dataset1_past() %>% 
      mutate(Method = if_else(grepl("KO", Method),"TKO/KO",Method)) %>% 
      filter(!(Result %in% c("win", "loss"))) %>%
      group_by(Result) %>% 
      summarise(n=n()) %>%
      datatable(options = list(dom = 't', ordering = F, columnDefs = list(list(className = 'dt-center', targets = 1:1))), 
                colnames = NULL,
                rownames = F) %>%
      formatStyle("Result", target = "row", backgroundColor = "lightblue")
  })
  
  observeEvent(dataset1_past(), {
    if (any(c("draw", "NC") %in% pull(dataset1_past(), Result)))
      showElement("drawNCTable1") else hideElement("drawNCTable1")
  })
  
  output$recordTable2 <- renderDataTable({
    dataset2_past() %>% 
      mutate(Method = if_else(grepl("KO", Method),"TKO/KO",Method)) %>% 
      group_by(Method, Result) %>% 
      summarise(n=n()) %>% 
      spread(Result, n, fill = 0) %>%
      full_join(tibble(Method = c("TKO/KO", "Submission", "Decision"), 
                       win = 0, 
                       loss = 0,
                       draw = 0)) %>%
      group_by(Method) %>%
      summarise(win = sum(win, na.rm = TRUE),
                loss = sum(loss, na.rm = TRUE)) %>%
      arrange(match(Method, c("TKO/KO", "Submission", "Decision"))) %>%
      filter(Method %in% c("TKO/KO", "Submission", "Decision")) %>%
      datatable(options = list(dom = 't', ordering = F, columnDefs = list(list(className = 'dt-center', targets = 1:2)),
                               initComplete = JS(
                                 "function(settings, json) {", 
                                 "$(this.api().table().header()).find('th').css({'padding': '2px'});", 
                                 "}")), rownames = F) %>%
      formatStyle("win", backgroundColor = "lawngreen") %>%
      formatStyle("loss", backgroundColor = "lightpink")
  })
  
  output$drawNCTable2 <- renderDataTable({
    dataset2_past() %>% 
      mutate(Method = if_else(grepl("KO", Method),"TKO/KO",Method)) %>% 
      filter(!(Result %in% c("win", "loss"))) %>%
      group_by(Result) %>% 
      summarise(n=n()) %>%
      datatable(options = list(dom = 't', ordering = F, columnDefs = list(list(className = 'dt-center', targets = 1:1))), 
                colnames = NULL,
                rownames = F) %>%
      formatStyle("Result", target = "row", backgroundColor = "lightblue")
  })
  
  observeEvent(dataset2_past(), {
    if (any(c("draw", "NC") %in% pull(dataset2_past(), Result)))
      showElement("drawNCTable2") else hideElement("drawNCTable2")
  })
  
  output$eloGraph <- renderPlotly({
    fighter1 <- dataset1() %>% pull(Name1)
    fighter2 <- dataset2() %>% pull(Name1)
    g <- graphFighters(c(fighter1, fighter2), opponent = FALSE)
    ggplotly(g) %>%
      layout(legend = list(x=0, y=1, font=list(size = 8), tracegroupgap = 7),
             xaxis = list(title=""),
             yaxis = list(title=""),
             margin = list(l=0,r=0,b=0,t=0, pad = 0)) %>%
      plotly::config(displayModeBar = F)
    
      
  })
  
  
}
