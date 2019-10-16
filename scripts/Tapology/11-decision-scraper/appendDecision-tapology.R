if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)
if (!require("stringr")) install.packages("stringr")
library(stringr)
if (!require("stringi")) install.packages("stringi")
library(stringi)
if (!require("stringdist")) install.packages("stringdist")
library(stringdist)
if (!require("RSQLite")) install.packages("RSQLite")
library(RSQLite)


mydb <- dbConnect(RSQLite::SQLite(), "MMA-db.sqlite")


decisions <- dbReadTable(mydb, "MMA_decisions") %>%
  as.tibble() %>%
  filter(rounds != 12) %>%
  mutate(rownum = row_number(),
         date = as.Date(date),
         fighter1 = if_else(fighter1 == "Michael Byrnes Jr.", "Mike Byrnes", fighter1),
         fighter1 = if_else(fighter1 == "Zach Micklewright", "Zachary Micklewright", fighter1),
         fighter1 = if_else(fighter1 == "Thomas Diagne", "Ousmane Thomas Diagne", fighter1),
         fighter1 = if_else(fighter1 == "Michel Oleksiejczuk", "Michal Oleksiejczuk", fighter1),
         fighter2 = if_else(fighter2 == "Khalil Rountree Jr.", "Khalil Rountree", fighter2),
         fighter1 = if_else(fighter1 == "Rich Walsh", "Richard Walsh", fighter1),
         fighter1 = if_else(fighter1 == "J.J. Aldrich", "JJ Aldrich", fighter1),
         fighter1 = if_else(fighter1 == "R'Mandel Cameron", "Rmandel Cameron", fighter1),
         fighter2 = if_else(fighter2 == "O.J. Dominguez", "OJ Dominguez", fighter2),
         fighter1 = if_else(fighter1 == "Zachary Micklewright", "Zach Micklewright", fighter1),
         fighter1 = if_else(fighter1 == "Mike Byrnes", "Michael Byrnes", fighter1),
         fighter2 = if_else(fighter2 == "Joe Yager", "Joey Yager", fighter2),
         fighter2 = if_else(fighter2 == "Maegan Goodwin", "Maegen Goodwin", fighter2),
         fighter2 = if_else(fighter2 == "Emmanuel Walo", "Manny Walo", fighter2),
         fighter2 = if_else(fighter2 == "Larue Burley", "LaRue Burley", fighter2)
         ) %>%
  rename(dateMMA_dec = date) %>%
  select(-result)

if(!exists("fightsMMA_clean")) {
  fightsMMA_clean <- dbReadTable(mydb, "fightsMMA_clean") %>%
    as.tibble()
}


Tap_to_MMAdec <- tibble(Tap = NA_real_, 
                        MMAdec = decisions %>% 
                          pull(fighter1Link) %>% 
                          c(decisions %>%
                              pull(fighter2Link)) %>% 
                          unique)

fights_dec <- fightsMMA_clean %>% 
  select(fight_id, short_name1, result, short_name2, method, round, date, fighter1id, fighter2id) %>%
  filter(method %in% c("Decision", "Draw") | result == "No Contest") %>%
  mutate(date = as.Date(date),
         short_name1 = stri_trans_general(str = short_name1, id = "Latin-ASCII"),
         short_name2 = stri_trans_general(str = short_name2, id = "Latin-ASCII"))

fights2Dec <- tibble()


# 1st Round Join on both fighter names
for (i in 0:2) {
  decisions <- decisions %>%
    mutate(date2 = dateMMA_dec - i + 1)
  
  fights2Dec <- fights_dec %>% 
    inner_join(decisions, by=c("date" = "date2", "short_name1" = "fighter1", "short_name2" = "fighter2")) %>%
    rbind(fights2Dec)
}

Tap_to_MMAdec <- Tap_to_MMAdec %>%
  left_join(fights2Dec %>%
              select(fighter1id, fighter1Link) %>%
              rename(Tap = fighter1id, MMAdec = fighter1Link) %>%
              unique(),
            by = "MMAdec") %>%
  mutate(Tap = coalesce(Tap.x, Tap.y)) %>%
  select(Tap, MMAdec)

decisions <- decisions %>% 
  anti_join(fights2Dec, by="rownum")

fights_dec <- fights_dec %>% 
  anti_join(fights2Dec, by="fight_id")

# 2nd Round Join on 1 fighter name + date
for (i in 0:2) {
  decisions <- decisions %>%
    mutate(date2 = dateMMA_dec - i + 1)
  
  fights2Dec <- inner_join(fights_dec, decisions, by=c("date" = "date2", "short_name1" = "fighter1")) %>%
    select(colnames(fights2Dec)) %>%
    full_join(
      inner_join(fights_dec, decisions, by=c("date" = "date2", "short_name2" = "fighter2")) %>%
        select(colnames(fights2Dec))
    ) %>%
    rbind(fights2Dec)
}


Tap_to_MMAdec <- Tap_to_MMAdec %>%
  left_join(fights2Dec %>%
              select(fighter1id, fighter1Link) %>% 
              full_join(fights2Dec %>% 
                          select(fighter2id, fighter2Link),
                        by = c("fighter1id" = "fighter2id", "fighter1Link" = "fighter2Link")) %>% 
              unique() %>%
              rename(Tap = fighter1id, MMAdec = fighter1Link),
            by = "MMAdec") %>%
  mutate(Tap = coalesce(Tap.x, Tap.y)) %>%
  select(Tap, MMAdec)

decisions <- decisions %>% 
  anti_join(fights2Dec, by = "rownum")

fights_dec <- fights_dec %>% 
  anti_join(fights2Dec, by = "fight_id")


# 3rd Round flip fighters in cases of ties/NC
for (i in 0:2) {
  decisions <- decisions %>%
    mutate(date2 = dateMMA_dec - i + 1)
  
  fights2Dec <- fights_dec %>% 
    inner_join(decisions, by=c("date" = "date2", "short_name1" = "fighter2", "short_name2" = "fighter1")) %>%
    rename(short_name1 = short_name2, short_name2 = short_name1, fighter1id = fighter2id, fighter2id = fighter1id) %>%
    select(colnames(fights2Dec)) %>%
    rbind(fights2Dec)
}

Tap_to_MMAdec <- left_join(Tap_to_MMAdec,
                               fights2Dec %>%
                                 select(fighter1id, fighter1Link) %>%
                                 rename(Tap = fighter1id, MMAdec = fighter1Link) %>%
                                 unique(),
                               by = "MMAdec") %>%
  mutate(Tap = coalesce(Tap.x, Tap.y)) %>%
  select(Tap, MMAdec) %>%
  unique()

decisions <- decisions %>% 
  anti_join(fights2Dec, by="rownum")

fights_dec <- fights_dec %>% 
  anti_join(fights2Dec, by="fight_id")


# 4th Round, Use Tap_to_MMAdec table
fights2Dec <- decisions %>% 
  left_join(Tap_to_MMAdec, by = c("fighter1Link" = "MMAdec")) %>%
  rename("Tap1" = "Tap") %>%
  inner_join(decisions %>% 
               left_join(Tap_to_MMAdec, by = c("fighter2Link" = "MMAdec")) %>%
               rename("Tap2" = "Tap")) %>% 
  inner_join(fights_dec, by = c("Tap1" = "fighter1id", "Tap2" = "fighter2id", "dateMMA_dec" = "date")) %>% 
  inner_join(fights_dec) %>% 
  select(colnames(fights2Dec)) %>%
  rbind(fights2Dec)


Tap_to_MMAdec <- left_join(Tap_to_MMAdec,
                               fights2Dec %>%
                                 select(fighter1id, fighter1Link) %>%
                                 rename(Tap = fighter1id, MMAdec = fighter1Link) %>%
                                 unique(),
                               by = "MMAdec") %>%
  mutate(Tap = coalesce(Tap.x, Tap.y)) %>%
  select(Tap, MMAdec) %>%
  unique()

decisions <- decisions %>% 
  anti_join(fights2Dec, by="rownum")

fights_dec <- fights_dec %>% 
  anti_join(fights2Dec, by="fight_id")


# 5th Round, widen the daterange interval since some events have wrong dates
for (i in 0:30) {
  decisions <- decisions %>%
    mutate(date2 = dateMMA_dec - i + 15)
  
  fights2Dec <- fights_dec %>% 
    inner_join(decisions, by=c("date" = "date2", "short_name1" = "fighter1", "short_name2" = "fighter2")) %>%
    rbind(fights2Dec)
}

Tap_to_MMAdec <- left_join(Tap_to_MMAdec,
                               fights2Dec %>%
                                 select(fighter1id, fighter1Link) %>%
                                 rename(Tap = fighter1id, MMAdec = fighter1Link) %>%
                                 unique(),
                               by = "MMAdec") %>%
  mutate(Tap = coalesce(Tap.x, Tap.y)) %>%
  select(Tap, MMAdec) %>%
  unique()

decisions <- decisions %>% 
  anti_join(fights2Dec, by="rownum")

fights_dec <- fights_dec %>% 
  anti_join(fights2Dec, by="fight_id")



# 6th Round, Use Tap_to_MMAdec table and just 1 name
for (i in 0:32) {
  decisions <- decisions %>%
    mutate(date2 = dateMMA_dec - i + 16)
  
  tempTable <- decisions %>% 
    left_join(Tap_to_MMAdec, by = c("fighter1Link" = "MMAdec")) %>%
    rename("Tap1" = "Tap") %>%
    inner_join(decisions %>% 
                 left_join(Tap_to_MMAdec, by = c("fighter2Link" = "MMAdec")) %>%
                 rename("Tap2" = "Tap"))
  
  fights2Dec <- tempTable %>% 
    inner_join(fights_dec, by = c("Tap1" = "fighter1id", "dateMMA_dec" = "date")) %>%
    full_join(tempTable %>% 
                inner_join(fights_dec, by = c("Tap2" = "fighter2id", "dateMMA_dec" = "date"))) %>%
    mutate(fighter1id = coalesce(Tap1, fighter1id),
           fighter2id = coalesce(Tap2, fighter2id)) %>% 
    select(colnames(tempTable), fighter1id, fighter2id) %>%
    inner_join(fights_dec, by = c("fighter1id", "fighter2id", "date2" = "date")) %>%
    rename(date = date2) %>%
    select(colnames(fights2Dec)) %>%
    rbind(fights2Dec)
}
rm(tempTable)

Tap_to_MMAdec <- left_join(Tap_to_MMAdec,
                               fights2Dec %>%
                                 select(fighter1id, fighter1Link) %>%
                                 rename(Tap = fighter1id, MMAdec = fighter1Link) %>%
                                 unique(),
                               by = "MMAdec") %>%
  mutate(Tap = coalesce(Tap.x, Tap.y)) %>%
  select(Tap, MMAdec) %>%
  unique()




decisions <- decisions %>% 
  anti_join(fights2Dec, by="rownum")

fights_dec <- fights_dec %>% 
  anti_join(fights2Dec, by="fight_id")



fightsMMA_clean <- fightsMMA_clean %>% 
  left_join(fights2Dec %>%
              select(fight_id, fighter1id, fighter2id, fighter1winAttr),
            by = c("fight_id", "fighter1id", "fighter2id")) %>% 
  left_join(fights2Dec %>%
              select(fight_id, fighter1id, fighter2id, fighter1winAttr),
            by = c("fight_id", "fighter1id" = "fighter2id", "fighter2id" = "fighter1id")) %>%
  mutate(fighter1winAttr = coalesce(fighter1winAttr.x, 1 - fighter1winAttr.y)) %>%
  select(colnames(fightsMMA_clean), fighter1winAttr)

warning(paste0(nrow(decisions), " fights not matched"))

fightsMMA_NC <- dbReadTable(mydb, "fightsMMA_NC") %>%
  as.tibble()



fightsMMA_clean_records <- fightsMMA_clean %>%
  left_join(fightsMMA_NC %>% 
              select(fight_id, fighter1id, fighter2id, resultA), 
            by = c("fight_id", "fighter1id", "fighter2id")) %>%
  left_join(fightsMMA_NC %>% 
              select(fight_id, fighter1id, fighter2id, resultB),
            by = c("fight_id", "fighter1id" = "fighter2id", "fighter2id" = "fighter1id")) %>%
  mutate(result_fix = NA_real_,
         result_fix = if_else(result == "Win", coalesce(fighter1winAttr, 1), result_fix),
         result_fix = if_else(result == "Loss", coalesce(fighter1winAttr, 0), result_fix),
         result_fix = if_else(result == "Draw", coalesce(fighter1winAttr, 0.5), result_fix),
         result_fix = if_else(result == "No Contest", coalesce(fighter1winAttr, resultA, resultB), result_fix),
         result_fix = if_else(method == "Disqualification" & !is.na(method), NA_real_, result_fix)
         ) %>%
  select(-fighter1winAttr, -resultA, -resultB) %>%
  distinct()


dbWriteTable(mydb, "fightsMMA_clean", fightsMMA_clean_records, overwrite = T)

rm(list=ls()[! ls() %in% c("fightsMMA_clean_records")])
gc()
