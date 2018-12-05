library(tidyverse)
library(data.table)
library(rvest)
library(lubridate)

source('./scrape-odds-7/oddsScraperScripts.R', echo=TRUE)

pastOdds <- readRDS("./scrape-odds-7/data/pastOdds.RDS")
futureOdds <- readRDS("./scrape-odds-7/data/futureOdds.RDS")

archivePage <- tryCatch({
  read_html("https://www.bestfightodds.com/archive")
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

eventLinks <- archivePage %>% 
  html_nodes("#page-content a") %>%
  html_attr("href")

eventDates <- archivePage %>% 
  html_nodes(".content-list-date") %>% 
  html_text() %>% 
  gsub("([[:alpha:]]* )(\\d*)(\\D{2})(.*)", "\\1\\2\\4", .) %>%
  as.Date("%b %d %Y")

for (i in which(eventLinks %in% pastOdds$eventLink == FALSE)) {
  eventOddsScraper(eventLinks[i])
}
