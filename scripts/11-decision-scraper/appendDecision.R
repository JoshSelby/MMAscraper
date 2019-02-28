library(tidyverse)
library(stringr)
library(stringdist)

decisions <- readRDS("./scripts/11-decision-scraper/data/decisions.RDS")

fights <- readRDS("./scripts/2-clean-data/data/fights_clean.rds")

Sherdog_to_MMAdec <- tibble(Sherdog = NA %>% as.character, 
                         MMAdec = decisions %>% pull(fighter1Link) %>% c(decisions %>% pull(fighter2Link)) %>% unique)

fights2 <- fights %>% 
  select(Fighter1, Result, Fighter2, Method, R, Date, Link1, Link2) %>%
  filter(Method == "Decision")


# 1st Round Join on both fighter names
fights2Dec <- fights2 %>% 
  inner_join(decisions, by=c("Date" = "date", "Fighter1" = "fighter1", "Fighter2" = "fighter2"))

decisions <- decisions %>% 
  anti_join(fights2Dec, by=c("date" = "Date", "fighter1" = "Fighter1", "fighter2" = "Fighter2"))

fights2 <- fights2 %>% 
  anti_join(fights2Dec)


Sherdog_to_MMAdec <- left_join(Sherdog_to_MMAdec,
                               fights2Dec %>%
                                 select(Link1, fighter1Link) %>%
                                 rename(Sherdog = Link1, MMAdec = fighter1Link) %>%
                                 unique(),
                               by = "MMAdec") %>%
  mutate(Sherdog = coalesce(Sherdog.x, Sherdog.y)) %>%
  select(Sherdog, MMAdec)

# 2nd Round Join on stringdist logic + 1 fighter name
fights2Dec <- inner_join(fights2, decisions, by=c("Date" = "date", "Fighter1" = "fighter1")) %>%
  select(colnames(fights2Dec)) %>%
  full_join(
    inner_join(fights2, decisions, by=c("Date" = "date", "Fighter2" = "fighter2")) %>%
                 select(colnames(fights2Dec))
    ) %>%
  rbind(fights2Dec)









filtfightsOdds <- fights2Dec %>% 
  filter(stringdist(fighter2Name, fighterLink2s, method = 'qgram') < 8 |
           stringdist(fighter1Name, fighterLink1s, method = 'qgram') < 8) %>%
  rename(BFO1 = fighter, BFO2 = opponent) %>%
  select(-fighterLink1s, -fighterLink2s) %>%
  rbind(filtfightsOdds)

