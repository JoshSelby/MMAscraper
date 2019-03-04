library(tidyverse)
library(lubridate)
library(rvest)
library(pbapply)

source('./scripts/11-decision-scraper/decisionScraperScripts.R', echo=TRUE)
decisions <- if ("decisions.RDS" %in% list.files("./scripts/11-decision-scraper/data")) {
  readRDS("./scripts/11-decision-scraper/data/decisions.RDS")
  }
fightsMMAdec <- if ("fightsMMAdec.RDS" %in% list.files("./scripts/11-decision-scraper/data")) {
  readRDS("./scripts/11-decision-scraper/data/fightsMMAdec.RDS")
}

if(is.null(decisions)) rm(decisions)
if(is.null(fightsMMAdec)) rm(fightsMMAdec)

years <- c(1995:year(today()))

events <- pblapply(years, eventsScraper) %>% unlist
if (exists("decisions")) {
  events <- setdiff(events, decisions$eventLink %>% unique)
}


fightsMMAdec <- pblapply(events, fightsScraper) %>% unlist

decisions <- pblapply(fightsMMAdec, decisionScraper) %>% 
  bind_rows() %>%
  rbind(if(exists("decisions")) decisions) %>%
  unique

fightsMMAdec <- decisions$eventLink

saveRDS(fightsMMAdec, "./scripts/11-decision-scraper/data/fightsMMAdec.RDS")
saveRDS(decisions, "./scripts/11-decision-scraper/data/decisions.RDS")

rm(list=ls())
