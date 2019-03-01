library(tidyverse)
library(stringr)
library(stringdist)

decisions <- readRDS("./scripts/11-decision-scraper/data/decisions.RDS") %>%
  filter(rounds != 12) %>%
  mutate(rownum = row_number(),
         fighter1 = if_else(fighter1 == "Michael Byrnes Jr.", "Mike Byrnes", fighter1),
         fighter1 = if_else(fighter1 == "Zach Micklewright", "Zachary Micklewright", fighter1),
         fighter1 = if_else(fighter1 == "Thomas Diagne", "Ousmane Thomas Diagne", fighter1),
         fighter1 = if_else(fighter1 == "Michel Oleksiejczuk", "Michal Oleksiejczuk", fighter1),
         fighter2 = if_else(fighter2 == "Khalil Rountree Jr.", "Khalil Rountree", fighter2))

fights <- readRDS("./scripts/3-records/data/fights_records.rds")

Sherdog_to_MMAdec <- tibble(Sherdog = NA %>% as.character, 
                         MMAdec = decisions %>% pull(fighter1Link) %>% c(decisions %>% pull(fighter2Link)) %>% unique)

fights2 <- fights %>% 
  select(match_id, Fighter1, Result, Fighter2, Method, R, Date, Link1, Link2) %>%
  filter(Method %in% c("Decision", "Draw") | Result == "NC")

fights2Dec <- tibble()
# 1st Round Join on both fighter names
for (i in 0:2) {
  decisions <- decisions %>%
  mutate(date2 = date - i + 1)

fights2Dec <- fights2 %>% 
  inner_join(decisions, by=c("Date" = "date2", "Fighter1" = "fighter1", "Fighter2" = "fighter2")) %>%
  rbind(fights2Dec)
}

Sherdog_to_MMAdec <- left_join(Sherdog_to_MMAdec,
                               fights2Dec %>%
                                 select(Link1, fighter1Link) %>%
                                 rename(Sherdog = Link1, MMAdec = fighter1Link) %>%
                                 unique(),
                               by = "MMAdec") %>%
  mutate(Sherdog = coalesce(Sherdog.x, Sherdog.y)) %>%
  select(Sherdog, MMAdec)

decisions <- decisions %>% 
  anti_join(fights2Dec, by="rownum")

fights2 <- fights2 %>% 
  anti_join(fights2Dec, by="match_id")

# 2nd Round Join on 1 fighter name + date
for (i in 0:2) {
  decisions <- decisions %>%
    mutate(date2 = date - i + 1)

  fights2Dec <- inner_join(fights2, decisions, by=c("Date" = "date2", "Fighter1" = "fighter1")) %>%
    select(colnames(fights2Dec)) %>%
    full_join(
      inner_join(fights2, decisions, by=c("Date" = "date2", "Fighter2" = "fighter2")) %>%
                   select(colnames(fights2Dec))
      ) %>%
    rbind(fights2Dec)
}


Sherdog_to_MMAdec <- left_join(Sherdog_to_MMAdec,
                               fights2Dec %>%
                                 select(Link1, fighter1Link) %>% 
                                 full_join(fights2Dec %>% 
                                             select(Link2, fighter2Link), by = c("Link1" = "Link2", "fighter1Link" = "fighter2Link")) %>% 
                                 unique %>%
                                 rename(Sherdog = Link1, MMAdec = fighter1Link),
                               by = "MMAdec") %>%
  mutate(Sherdog = coalesce(Sherdog.x, Sherdog.y)) %>%
  select(Sherdog, MMAdec)


decisions <- decisions %>% 
  anti_join(fights2Dec, by = "rownum")
  

fights2 <- fights2 %>% 
  anti_join(fights2Dec, by = "match_id")


# 3rd Round flip fighters in cases of ties/NC
for (i in 0:2) {
  decisions <- decisions %>%
    mutate(date2 = date - i + 1)
  
  fights2Dec <- fights2 %>% 
    inner_join(decisions, by=c("Date" = "date2", "Fighter1" = "fighter2", "Fighter2" = "fighter1")) %>%
    rename(Fighter1 = Fighter2, Fighter2 = Fighter1, Link1 = Link2, Link2 = Link1) %>%
    select(colnames(fights2Dec)) %>%
    rbind(fights2Dec)
}

Sherdog_to_MMAdec <- left_join(Sherdog_to_MMAdec,
                               fights2Dec %>%
                                 select(Link1, fighter1Link) %>%
                                 rename(Sherdog = Link1, MMAdec = fighter1Link) %>%
                                 unique(),
                               by = "MMAdec") %>%
  mutate(Sherdog = coalesce(Sherdog.x, Sherdog.y)) %>%
  select(Sherdog, MMAdec) %>%
  unique()

decisions <- decisions %>% 
  anti_join(fights2Dec, by="rownum")

fights2 <- fights2 %>% 
  anti_join(fights2Dec, by="match_id")


# 4th Round, Use Sherdog_to_MMAdec table
fights2Dec <- decisions %>% 
  left_join(Sherdog_to_MMAdec, by = c("fighter1Link" = "MMAdec")) %>%
  rename("Sherdog1" = "Sherdog") %>%
  inner_join(decisions %>% 
               left_join(Sherdog_to_MMAdec, by = c("fighter2Link" = "MMAdec")) %>%
               rename("Sherdog2" = "Sherdog")) %>% inner_join(fights2, by = c("Sherdog1" = "Link1", "Sherdog2" = "Link2", "date" = "Date")) %>% 
  inner_join(fights2) %>% 
  select(colnames(fights2Dec)) %>%
  rbind(fights2Dec)


Sherdog_to_MMAdec <- left_join(Sherdog_to_MMAdec,
                               fights2Dec %>%
                                 select(Link1, fighter1Link) %>%
                                 rename(Sherdog = Link1, MMAdec = fighter1Link) %>%
                                 unique(),
                               by = "MMAdec") %>%
  mutate(Sherdog = coalesce(Sherdog.x, Sherdog.y)) %>%
  select(Sherdog, MMAdec) %>%
  unique()

decisions <- decisions %>% 
  anti_join(fights2Dec, by="rownum")

fights2 <- fights2 %>% 
  anti_join(fights2Dec, by="match_id")


# 5th Round, widen the daterange interval since some events have wrong dates
for (i in 0:30) {
  decisions <- decisions %>%
    mutate(date2 = date - i + 15)
  
  fights2Dec <- fights2 %>% 
    inner_join(decisions, by=c("Date" = "date2", "Fighter1" = "fighter1", "Fighter2" = "fighter2")) %>%
    rbind(fights2Dec)
}

Sherdog_to_MMAdec <- left_join(Sherdog_to_MMAdec,
                               fights2Dec %>%
                                 select(Link1, fighter1Link) %>%
                                 rename(Sherdog = Link1, MMAdec = fighter1Link) %>%
                                 unique(),
                               by = "MMAdec") %>%
  mutate(Sherdog = coalesce(Sherdog.x, Sherdog.y)) %>%
  select(Sherdog, MMAdec) %>%
  unique()

decisions <- decisions %>% 
  anti_join(fights2Dec, by="rownum")

fights2 <- fights2 %>% 
  anti_join(fights2Dec, by="match_id")



# 6th Round, Use Sherdog_to_MMAdec table and just 1 name
for (i in 0:30) {
  decisions <- decisions %>%
    mutate(date2 = date - i + 15)
  
  tempTable <- decisions %>% 
    left_join(Sherdog_to_MMAdec, by = c("fighter1Link" = "MMAdec")) %>%
    rename("Sherdog1" = "Sherdog") %>%
    inner_join(decisions %>% 
                 left_join(Sherdog_to_MMAdec, by = c("fighter2Link" = "MMAdec")) %>%
                 rename("Sherdog2" = "Sherdog"))
  
  fights2Dec <- tempTable %>% 
    inner_join(fights2, by = c("Sherdog1" = "Link1", "date" = "Date")) %>%
    full_join(tempTable %>% 
                 inner_join(fights2, by = c("Sherdog2" = "Link2", "date" = "Date"))) %>%
    mutate(Link1 = coalesce(Sherdog1, Link1),
           Link2 = coalesce(Sherdog2, Link2)) %>% 
    select(colnames(tempTable), Link1, Link2) %>%
    inner_join(fights2, by = c("Link1", "Link2", "date2" = "Date")) %>%
    rename(Date = date2) %>%
    select(colnames(fights2Dec)) %>%
    rbind(fights2Dec)
}

Sherdog_to_MMAdec <- left_join(Sherdog_to_MMAdec,
                               fights2Dec %>%
                                 select(Link1, fighter1Link) %>%
                                 rename(Sherdog = Link1, MMAdec = fighter1Link) %>%
                                 unique(),
                               by = "MMAdec") %>%
  mutate(Sherdog = coalesce(Sherdog.x, Sherdog.y)) %>%
  select(Sherdog, MMAdec) %>%
  unique()




decisions <- decisions %>% 
  anti_join(fights2Dec, by="rownum")

fights2 <- fights2 %>% 
  anti_join(fights2Dec, by="match_id")





fights <- fights %>% left_join(fights2Dec) %>% select(colnames(fights), fighter1winAttr)

saveRDS(fights, "./scripts/11-decision-scraper/data/fights.RDS")

rm(list=ls())
