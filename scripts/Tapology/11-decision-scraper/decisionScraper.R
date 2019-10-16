library(tidyverse)
library(lubridate)
library(rvest)
library(pbapply)
library(RSQLite)


mydb <- dbConnect(RSQLite::SQLite(), "MMA-db.sqlite")

source('./scripts/Tapology/11-decision-scraper/decisionScraperScripts.R', echo=TRUE)

if(dbExistsTable(mydb, "MMA_decisions")) {
  decisions <- dbReadTable(mydb, "MMA_decisions")

  fightsMMAdec <- decisions$fightLink
  
  if(is.null(decisions)) rm(decisions)
  if(is.null(fightsMMAdec)) rm(fightsMMAdec)
  
}


years <- c(1995:year(today()))

events <- pblapply(years, eventsScraper) %>% unlist
if (exists("decisions")) {
  events <- setdiff(events, decisions$eventLink %>% unique)
}


fightsMMAdec <- pblapply(events, fightsScraper) %>% unlist

if(fightsMMAdec %>% length > 0) {
  decisions <- tibble()
  while(length(fightsMMAdec) > 0) {
    parScrapDecisions(fightsMMAdec, 300)
    decisions <- rbind(decisions, decisions2)
    fightsMMAdec <- setdiff(fightsMMAdec, decisions2$fightLink)
  }
  
  decisions <- decisions %>% 
    mutate(date = as.character(date))
  
  dbWriteTable(mydb, "MMA_decisions", decisions, append = T)
}



rm(list=ls())
