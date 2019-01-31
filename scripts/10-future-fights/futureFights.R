library(tidyverse)
library(stringr)
library(stringdist)

fightersTable <- readRDS("./scripts/9-various-tables/data/fightersTable.rds")
fightMetricsEvent <- readRDS("./scripts/5-metrics/data/fightMetricsEvent.rds")
filtfightOdds <- readRDS("./scripts/8-append-odds/data/filtfightOdds.RDS")
Sherdog_to_BFO <- readRDS("./scripts/8-append-odds/data/Sherdog_to_BFO.RDS")
futureOdds <- readRDS("./scripts/7-scrape-odds/data/futureOdds.RDS")


futureFights <- futureOdds %>% 
  left_join(Sherdog_to_BFO, by=c("fighter" = "BFO")) %>%
  left_join(Sherdog_to_BFO, by=c("opponent" = "BFO")) 

availFighters <- fightersTable %>% 
  pull(Link)

noSherdog <- futureFights %>% 
  select(fighter, Sherdog.x) %>% 
  filter(is.na(Sherdog.x)) %>% 
  pull(fighter)

minitbl <- tibble()  
for (i in 1:length(noSherdog)) {
  minitbl <- tibble(Sherdog = availFighters, BFO = noSherdog[i]) %>%
    mutate(availStrip = Sherdog %>% gsub("(.*)(-\\d*$)", "\\1", .) %>% gsub("-", "", .) %>% tolower(),
           noSherstrip = BFO %>% gsub("(.*)(-\\d*$)", "\\1", .) %>% gsub("-", "", .) %>% tolower(),
           dist = stringdist(availStrip, noSherstrip, method = 'qgram')) %>% 
    arrange(dist) %>%
    filter(dist == 0) %>%
    select(Sherdog, BFO) %>%
    rbind(minitbl)
}
# Not a perfect match
noSherdog %>% setdiff(minitbl$BFO)

# Manually enter
Sherdog_to_BFO <- minitbl %>% 
  rbind(c("Marcos-Rosa-Mariano-182785", "Marcos-Rosa-8870"),
        c("Raulian-Paiva-Frazao-167575", "Raulian-Paiva-8553"),
        c("Geraldo-de-Freitas-Jr-107229", "Geraldo-De-Freitas-8898"),
        c("Carlos-Felipe-185021", "Felipe-Colares-8897")) %>%
  full_join(Sherdog_to_BFO) %>%
  unique()

rm(minitbl, availFighters, i, noSherdog)


futureFights <- futureOdds %>% 
  left_join(Sherdog_to_BFO, by=c("fighter" = "BFO")) %>%
  left_join(Sherdog_to_BFO, by=c("opponent" = "BFO")) %>%
  rename(Link1 = Sherdog.x, Link2 = Sherdog.y) %>%
  left_join(fightersTable, by = c("Link1" = "Link")) %>%
  left_join(fightersTable, by = c("Link2" = "Link")) %>%
  mutate(record1 = paste(wins.x, loss.x, draw.x, NC.x, sep="-"),
         record2 = paste(wins.y, loss.y, draw.y, NC.y, sep="-")) %>%
  select(Link1, Link2, eventName, Date, rating.x, rating.y, record1, record2, `5Dimes`) %>%
  rename(r1b = rating.x, r2b = rating.y, odds = `5Dimes`)
