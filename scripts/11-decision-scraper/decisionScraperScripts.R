library(tidyverse)
library(lubridate)
library(rvest)

# Scrapes all fights from a given Event
eventsScraper <- function(year) {
  yearPage <- read_html(paste0("http://www.mmadecisions.com/decisions-by-event/", year,"/"))
  
  events <- yearPage %>% 
    html_nodes(".list a") %>% 
    html_attr("href") %>% 
    gsub(";.*", "", .)
  
  return(events)
}

# Scrapes all events for a single fighter
fightsScraper <- function(eventLink) {
  eventPage <- read_html(paste0("http://www.mmadecisions.com/", eventLink))
  
  fights <- eventPage %>% 
    html_nodes(".list2 a") %>% 
    html_attr("href") %>%
    gsub(";.*", "", .)
  
  return(fights)
}

winAttr <- function(score1, score2, rounds) {
  fighter1Attr <- (rounds+score1-score2)/(rounds*2)
  return(fighter1Attr)
}

decisionScraper <- function(fightLink) {
  fightPage <- read_html(paste0("http://www.mmadecisions.com/", fightLink))
  
  date <- fightPage %>% 
    html_nodes(":nth-child(5) .decision-top2") %>% 
    html_text() %>% 
    gsub("(.*)(\\d{4}-\\d{2}-\\d{2})(.*)", "\\2", .) %>%
    as.Date()
  
  eventLink <- fightPage %>% 
    html_nodes(".decision-top2 a") %>% 
    html_attr("href")
  
  fighter1 <- fightPage %>%
    html_nodes(".decision-top a") %>%
    html_text() %>%
    trimws() %>%
    gsub("\\s", " ", .)
  
  fighter2 <- fightPage %>%
    html_nodes(".decision-bottom a") %>%
    html_text() %>%
    trimws() %>%
    gsub("\\s", " ", .)
  
  fighter1Link <- fightPage %>%
    html_nodes(".decision-top a") %>%
    html_attr("href")
  
  fighter2Link <- fightPage %>%
    html_nodes(".decision-bottom a") %>%
    html_attr("href")
  
  mediaDecisions <- fightPage %>%
    html_nodes(".decision .external") %>%
    html_text()
  
  judgeDecisions <- fightPage %>%
    html_nodes("td td td .bottom-cell") %>%
    html_text() %>% 
    matrix(3,2,byrow=T) %>%
    apply(1, paste, collapse = "-")
  
  decisions <- c(judgeDecisions, mediaDecisions)
  
  rounds <- fightPage %>% 
    html_nodes("td td td:nth-child(2) .list:nth-child(1)") %>% 
    html_text() %>%
    as.numeric() %>%
    tail(1)
  
  fighter1Scores <- decisions %>% 
    gsub("-.*", "", .) %>%
    as.numeric()
  fighter2Scores <- decisions %>% 
    gsub(".*-", "", .) %>%
    as.numeric()
  
  fighter1winAttr <- winAttr(mean(fighter1Scores), mean(fighter2Scores), rounds)
  return(tibble(date, fighter1, fighter2, f1Score = mean(fighter1Scores), f2Score = mean(fighter2Scores), rounds, fighter1winAttr, media = length(mediaDecisions), judge = length(judgeDecisions), fighter1Link, fighter2Link, eventLink))
    
}
