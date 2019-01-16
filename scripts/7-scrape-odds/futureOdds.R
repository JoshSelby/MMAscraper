library(tidyverse)
library(data.table)
library(rvest)
library(lubridate)

source('./scripts/7-scrape-odds/oddsScraperScripts.R', echo=TRUE)

# Scrape front page of bestfightodds 
mainPage <- tryCatch({
  read_html("https://www.bestfightodds.com")
  },
  error=function(cond) {
    message(cond)
    no_errors = FALSE
    return(eventPage)
  },
  warning=function(cond) {
    message(cond)
    no_errors = FALSE
    return(eventPage)
    }
  )

# Get links for future Events
futureEvents <- mainPage %>% 
  html_nodes(".table-header a") %>% 
  html_attr("href")

futureOdds <- tibble()
for (i in 1:length(futureEvents)) {
  futureOdds <- rbind(futureOdds, eventOddsScraper(futureEvents[i], future = "y"))
  print(i)
}

rm(futureEvents, mainPage, i, bind_odds, eventOddsScraper, oddsScraper)

saveRDS(futureOdds, "./scripts/7-scrape-odds/data/futureOdds.RDS")
