library(tidyverse)
library(lubridate)
library(rvest)
library(pbapply)

source('./scripts/11-decision-scraper/decisionScraperScripts.R', echo=TRUE)

years <- c(1995:year(today()))


events <- pblapply(years, eventsScraper) %>% unlist

fights <- pblapply(events, fightsScraper) %>% unlist

decisions <- pblapply(fights, decisionScraper) %>% rbind_list()

saveRDS(fights, "./scripts/11-decision-scraper/data/fightsMMADec.RDS")
saveRDS(decisions, "./scripts/11-decision-scraper/data/decisions.RDS")


