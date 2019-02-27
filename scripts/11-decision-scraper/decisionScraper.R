library(tidyverse)
library(lubridate)
library(rvest)

source('./scripts/11-decision-scraper/decisionScraperScripts.R', echo=TRUE)

years <- c(1995:year(today()))


events <- lapply(years, eventsScraper) %>% unlist

fights <- lapply(events, fightsScraper) %>% unlist


