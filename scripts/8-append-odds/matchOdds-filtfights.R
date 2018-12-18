library(data.table)
library(tidyverse)
library(stringr)
library(fuzzyjoin)

pastOdds <- readRDS("./scrape-odds-7/data/pastOdds.RDS")
futureOdds <- readRDS("./scrape-odds-7/data/futureOdds.RDS")
filtfights <- readRDS(file = "./metrics-5/filtfights.rds")

# Events with duplicate links
dupedEvents <- pastOdds %>% 
  group_by(eventName, eventLink, Date) %>% 
  summarise(num = n()) %>% 
  group_by(eventName, Date) %>% 
  summarise(num = n()) %>% 
  filter(num > 1) %>% 
  pull(eventName)

# Find the link that is incorrect
wrongEvents <- c()
for (i in 1:length(dupedEvents)) {
  wrongEvents[i] <- sapply(pastOdds %>% 
                             filter(eventName == dupedEvents[i]) %>% 
                             pull(eventLink) %>% 
                             unique(), function(x) stringdist(dupedEvents[i], x)) %>%
    which.max() %>%
    names()
}

# delete
pastOdds <- pastOdds %>%
  filter(!(eventLink %in% wrongEvents))

rm(wrongEvents, dupedEvents)

# Fix a few fighter's names, create stripped version of name
pastOdds <- pastOdds %>% 
  mutate(fighter = ifelse(fighter == "Jacob-Volkman-1246", "Jacob-Volkmann-3471", fighter),
         opponent = ifelse(opponent == "Jacob-Volkman-1246", "Jacob-Volkmann-3471", opponent),
         fighter = ifelse(fighter == "Luigi-Vandramini-8636", "Luigi-Vendramini-8636", fighter),
         opponent = ifelse(opponent == "Luigi-Vandramini-8636", "Luigi-Vendramini-8636", opponent),
         fighter = ifelse(fighter == "Elizeu-Zaleski-5594", "Elizeu-Zaleski-Dos-Santos-6667", fighter),
         opponent = ifelse(opponent == "Elizeu-Zaleski-5594", "Elizeu-Zaleski-Dos-Santos-6667", opponent),
         fighterLink1s = gsub("(.*)(-\\d*$)", "\\1", fighter) %>% gsub("-", "", .) %>% tolower(),
         fighterLink2s = gsub("(.*)(-\\d*$)", "\\1", opponent) %>% gsub("-", "", .) %>% tolower())

filtfights <- filtfights %>%
  mutate(fighter1Name = gsub("(.*)(-\\d*$)", "\\1", Link1) %>% gsub("-", "", .) %>% tolower(),
         fighter2Name = gsub("(.*)(-\\d*$)", "\\1", Link2) %>% gsub("-", "", .) %>% tolower())

# Begin Matching
filtfightsOdds <- tibble()
for (i in 0:2) {
  pastOdds <- pastOdds %>%
    mutate(Date2 = Date - i + 1)
  
  filtfightsOdds <- inner_join(filtfights, pastOdds, by = c("fighter1Name" = "fighterLink1s", "fighter2Name" = "fighterLink2s",
                                          "Date" = "Date2")) %>%
    select(colnames(filtfights), '5Dimes') %>%
    rbind(filtfightsOdds)
}

filtfights2 <- anti_join(filtfights, filtfightsOdds, by = "match_id")

pastOdds <- pastOdds %>%
  rowwise() %>%
  mutate(fighter1regex = strsplit(fighterLink1s, "") %>% unlist %>% paste(collapse = ".*") %>% paste0(., ".*"),
         fighter2regex = strsplit(fighterLink2s, "") %>% unlist %>% paste(collapse = ".*") %>% paste0(., ".*"))

pastOdds <- pastOdds %>%
  mutate(Date2 = Date)

filtfightsOdds <- regex_inner_join(filtfights2, pastOdds, by = c("fighter1Name" = "fighter1regex", "fighter2Name" = "fighter2regex")) %>%
  filter(Date >= Date2 - 1 & Date <= Date2 + 1) %>%
  select(colnames(filtfights), '5Dimes') %>%
  rbind(filtfightsOdds)


filtfights2 <- anti_join(filtfights, filtfightsOdds, by = "match_id")


filtfightsOdds <- inner_join(filtfightsOdds, filtfightsOdds, by=c("match_id" = "match_id", "Link1" = "Link2"), suffix = c("", ".y")) %>%
  select(colnames(filtfightsOdds), `5Dimes.y`)

saveRDS(filtfightsOdds, file = "./append-odds-8/data/filtfightsOdds.rds")

