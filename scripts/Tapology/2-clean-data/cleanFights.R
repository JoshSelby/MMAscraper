if (!require("dtplyr")) install.packages("dtplyr")
library(dtplyr)
if (!require("data.table")) install.packages("data.table")
library(data.table)
if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse, warn.conflicts = FALSE)
if (!require("RSQLite")) install.packages("RSQLite")
library(RSQLite)
if (!require("rvest")) install.packages("rvest")
library(rvest)


mydb <- dbConnect(RSQLite::SQLite(), "MMA-db.sqlite")


if(!exists("fights")) {
  fights <- dbReadTable(mydb, "fights") %>%
    lazy_dt()
}

if(!exists("fighterAttributes")) {
  fighterAttributes <- dbReadTable(mydb, "fighterAttributes") %>%
    lazy_dt()
}




{
  fights <- fights %>%
    filter(!(bout_url %in%
               c("https://www.tapology.com/fightcenter/bouts/362915-bars-abdulakh-gusniev-vs-vladimir-borozdin",
                 "https://www.tapology.com/fightcenter/bouts/367974-patriotic-victory-cup-2016-dokha-gutaev-vs-andrey-kovin"))
    ) %>%
    mutate(result_description = case_when(
      fighter_url == "148109-john-trano" & 
        opponent_fighter_name == "Judah Ciervo" ~ "Win",
      fighter_url == "63879-vicky-boparari" & 
        opponent_fighter_name == "Keegan Mcauly"~ "Win",
      fighter_url == "25250-justin-francisco" & 
        opponent_fighter_name == "Matt Murphy" ~ "Win",
      fighter_url == "65634-shamil-magomedov" & 
        opponent_fighter_name == "Evgeniy Ryazanov" ~ "Win",
      fighter_url == "109611-gary-edwards" & 
        opponent_fighter_name == "Ben Katzmark" ~ "Win",
      fighter_url == "43468-adrian-bartree" & 
        opponent_fighter_name == "Tony Helou" ~ "Win",
      fighter_url == "78226-javier-reyes-tyson" & 
        opponent_fighter_name == "Ignacio Bahamondes" ~ "Loss",
      TRUE ~ result_description
    ))  %>%
    lazy_dt()
  }

fightsMMA <- fights %>%
  filter(sport == "MMA" & status == "confirmed" & !is.na(result_description)) %>%
  rename(fighter1id = fighter.data.id, 
         fighter2id = opponent_fighter.data.id, 
         result = result_description,
         date = event_date, 
         method = finishing_method, 
         method_d = finishing_move, 
         event = event_name, 
         event_name = event_subname, 
         org = promotion_short_name_or_name, 
         fight_num = number_at_event,
         weight_class = weight_class,
         weight_class_lb = scheduled_weight,
         round_structure = round_structure, 
         round = final_round,
         time = final_round_time, 
         time_total = total_fight_time, 
         rematch = rematch_series,
         odds1 = betting_odds) %>%
  mutate(fighter1id = as.numeric(fighter1id), 
         fighter2id = as.numeric(fighter2id), 
         date = as.Date(date), 
         round = as.integer(round),
         event = ifelse(is.na(event_name), event, paste0(event, ": ", event_name)),
         odds1 = ifelse(odds1 == 0, NA, odds1)) %>%
  select(fighter1id, result, fighter2id, date, method, method_d, event, org, fight_num, rematch, weight_class, weight_class_lb, billing, referee, round_structure,
         round, time, time_total, bout_url, amateur, odds1) %>% 
  mutate(minId = pmin(fighter1id, fighter2id), maxId = pmax(fighter1id, fighter2id)) %>%
  lazy_dt()


fightsGroup <- fightsMMA %>%
  group_by(minId, maxId, date, event, fight_num, rematch, bout_url) %>% 
  summarise(num = n()) %>%
  arrange(date, event, fight_num) %>%
  ungroup() %>%
  mutate(fight_id = 1:n()) %>%
  lazy_dt()


missFights <- fightsGroup %>% 
  filter(num == 1) %>%
  left_join(fightsMMA) %>%
  lazy_dt()

  
fightsMMA <- missFights %>%
  mutate(fighter1idtemp = fighter1id,
         fighter1id = fighter2id,
         fighter2id = fighter1idtemp,
         result = case_when(
           result == "Win" ~ "Loss",
           result == "Loss" ~ "Win",
           TRUE ~ result
         )
  ) %>%
  select(-fighter1idtemp) %>%
  as.data.table() %>%
  list(missFights %>% as.data.table()) %>%
  rbindlist() %>%
  list(fightsGroup %>% 
          filter(num == 2) %>%
          left_join(fightsMMA) %>%
          as.data.table()) %>%
  rbindlist() %>%
  lazy_dt()

rm(missFights, fightsGroup)
gc()

fightsMMA <- fightsMMA %>% 
  filter(minId == fighter1id) %>% 
  left_join(fightsMMA %>% 
              filter(minId == fighter2id) %>% 
              select(fight_id, odds1), by="fight_id") %>%
  as.data.table() %>%
  list(fightsMMA %>% 
     filter(maxId == fighter1id) %>% 
     left_join(fightsMMA %>% 
                 filter(maxId == fighter2id) %>% 
                 select(fight_id, odds1), by="fight_id") %>%
       as.data.table()) %>%
  rbindlist() %>%
  lazy_dt()

fightsMMA <- fightsMMA %>%
  mutate(odds1 = ifelse(as.numeric(odds1.x) + as.numeric(odds1.y) >= 0 | is.na(odds1.y), NA, odds1.x) %>% as.numeric(),
         odds2 = ifelse(as.numeric(odds1.x) + as.numeric(odds1.y) >= 0 | is.na(odds1.x), NA, odds1.y) %>% as.numeric()
         ) %>%
  mutate(odds1 = ifelse(odds1 == -100, 100, odds1),
         odds2 = ifelse(odds2 == -100, 100, odds2)) %>%
  mutate(odds1 = ifelse(odds1+odds2>=0, NA, odds1),
         odds2 = ifelse(odds1+odds2>=0, NA, odds2)) %>%
  select(-num, -starts_with("odds1.")) %>%
  lazy_dt()



fightsMMA_clean <- fightsMMA %>% as_tibble() %>%
  left_join(fights %>% as_tibble() %>%
              select(opponent_fighter_name, opponent_fighter.data.id) %>% 
              mutate(opponent_fighter.data.id = as.numeric(opponent_fighter.data.id)) %>%
              distinct(), 
            by = c("fighter1id" = "opponent_fighter.data.id")) %>%
  left_join(fights %>% as_tibble() %>% 
              select(opponent_fighter_name, opponent_fighter.data.id) %>% 
              mutate(opponent_fighter.data.id = as.numeric(opponent_fighter.data.id)) %>%
              distinct(), 
            by = c("fighter2id" = "opponent_fighter.data.id")) %>%
  rename(short_name1 = opponent_fighter_name.x,
         short_name2 = opponent_fighter_name.y) %>%
  mutate(date = as.character(date),
         short_name1 = gsub("\\s+", " ", short_name1),
         short_name2 = gsub("\\s+", " ", short_name2))

################################################################################
mistakeFights <- fightsMMA_clean %>% 
  group_by(fight_id) %>% 
  summarise(num=n()) %>% 
  ungroup() %>% 
  filter(num>2) %>% 
  pull(fight_id)


mistakeFighters <- fightsMMA_clean %>% 
  filter(fight_id %in% mistakeFights) %>%
  select(fighter1id, short_name1) %>% 
  unique %>% 
  rename(fighter_id = fighter1id, short_name = short_name1) %>%
  rbind(fightsMMA_clean %>% 
          filter(fight_id %in% mistakeFights) %>%
          select(fighter2id, short_name2) %>% 
          unique %>%
          rename(fighter_id = fighter2id, short_name = short_name2)) %>%
  unique %>%
  group_by(fighter_id) %>%
  summarise(num = n()) %>%
  filter(num>1) %>%
  pull(fighter_id)

fix <- fighterAttributes %>% 
  filter(fighter_id %in% mistakeFighters) %>%
  select(fighter_id, short_name) %>%
  as.tibble()

rm(fights, fighterAttributes, fightsMMA)
gc()

setDT(fightsMMA_clean)

fightsMMA_clean <- fightsMMA_clean[!(fighter1id %in% fix$fighter_id | fighter2id %in% fix$fighter_id) | 
                                     (short_name1 %in% fix$short_name | short_name2 %in% fix$short_name)]

rm(mistakeFights, mistakeFighters, fix)

fightsMMA_clean <- fightsMMA_clean %>% 
  as_tibble() %>%
  select(fight_id, short_name1, result, short_name2, date, event, org, method,
         method_d, fight_num, rematch, bout_url, weight_class, weight_class_lb,
         billing, referee, round_structure, round, time, time_total, amateur, 
         odds1, odds2, fighter1id, fighter2id) %>%
  arrange(fight_id) %>%
  mutate(method = if_else(method == "Disqualificaton", "Disqualification", method)) %>%
  as.data.table()

wrongFights <- fightsMMA_clean %>%
  as.tibble() %>%
  group_by(fight_id) %>%
  summarise(r1 = max(result), r2 = min(result))


wrongFights <- wrongFights %>% filter(r1 == r2 & 
                           (r1 != "Draw" | r2 != "Draw") & 
                           (r1 != "No Contest" | r2 != "No Contest"))

fightsMMA_clean <- fightsMMA_clean[!(fight_id %in% wrongFights$fight_id)]


###############################################################################
source('./scripts/Tapology/3-records/recordAdder-tapology.R', echo=TRUE)

dbWriteTable(mydb, "fightsMMA_clean", fightsMMA_clean, overwrite = T)

rm(list=ls()[! ls() %in% c("fightsMMA_clean")])
gc()
