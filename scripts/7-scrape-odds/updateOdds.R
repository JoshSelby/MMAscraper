library(tidyverse)
library(data.table)
library(rvest)
library(lubridate)

source('./scripts/7-scrape-odds/oddsScraperScripts.R', echo=TRUE)

pastOdds <- readRDS("./scripts/7-scrape-odds/data/pastOdds.RDS")

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


updateOdds <- tibble()

for (i in which(eventLinks %in% pastOdds$eventLink == FALSE)) {
  updateOdds <- rbind(updateOdds, eventOddsScraper(eventLinks[i]))
  print(i)
}

pastOdds <- rbind(updateOdds, pastOdds)

rm(archivePage, updateOdds, eventDates, eventLinks, i, bind_odds, eventOddsScraper, oddsScraper)
saveRDS(pastOdds, "./scripts/7-scrape-odds/data/pastOdds.RDS")
