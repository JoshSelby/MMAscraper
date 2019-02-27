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
  fighter1Attr <- (rounds-(score1-score2))/rounds
  return(fighter1Attr)
}

decisionScraper <- function(fightLink) {
  fightPage <- read_html(paste0("http://www.mmadecisions.com/", fightLink))
  
  fighter1 <- fightPage %>%
    html_nodes(".decision-top a") %>%
    html_text() %>%
    trimws()
  
  fighter2 <- fightPage %>%
    html_nodes(".decision-bottom a") %>%
    html_text() %>%
    trimws()
  
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
    html_nodes("td td td:nth-child(1) .decision:nth-child(5) :nth-child(1)") %>% 
    html_text() %>%
    as.numeric()
  
  fighter1Scores <- decisions %>% 
    gsub("-.*", "", .) %>%
    as.numeric()
  fighter2Scores <- decisions %>% 
    gsub(".*-", "", .) %>%
    as.numeric()
  
  fighter1winAttr <- winAttr(mean(fighter1Scores), mean(fighter2Scores), rounds)
  
  return(fighter1winAttr)
    
}
