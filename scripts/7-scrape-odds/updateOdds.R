library(tidyverse)
library(data.table)
library(rvest)
library(lubridate)

source('./scripts/7-scrape-odds/oddsScraperScripts.R', echo=TRUE)

pastOdds <- readRDS("./scripts/7-scrape-odds/data/pastOdds.RDS")

archivePage <- read_html("https://www.bestfightodds.com/archive")

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

if (length(colnames(updateOdds)) > 12) {
  updateOdds <- updateOdds %>% 
    rename(WilliamH = colnames(updateOdds)[13])
}


pastOdds <- rbind(updateOdds, pastOdds)

saveRDS(pastOdds, "./scripts/7-scrape-odds/data/pastOdds.RDS")

rm(list=ls())
