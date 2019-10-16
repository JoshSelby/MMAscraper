library(tidyverse)
library(lubridate)

mydb <- dbConnect(RSQLite::SQLite(), "MMA-db.sqlite")

if (!exists("fighterAttributes")) {
  fighterAttributes <- dbReadTable(mydb, "fighterAttributes") %>%
    as_tibble()
}

if (!exists("fightsElo")) {
  fightsElo <- dbReadTable(mydb, "fightsElo") %>%
    as_tibble()
}


curRat <- fightsElo %>%
  filter(!is.na(r1a)) %>%
  arrange(fight_id) %>%
  group_by(short_name1, fighter1id) %>%
  slice(n()) %>%
  ungroup() %>%
  arrange(desc(r1a))

fightsElo2 <- fightsElo %>%
  arrange(desc(fight_id)) %>%
  select(fight_id, short_name1, result, short_name2, method, date, org, r1b, r2b, r1a, r2a, 
         result_fix, odds1, odds2, ammy = amateur, kFactor, fighter1id, fighter2id)
  

rankings <- fighterAttributes %>%
  select(fighter_id, short_name, date_of_birth, gender, reach_in_inches, height_in_inches, current_weight_class) %>%
  left_join(curRat %>% 
              select(fighter1id, date, org, r1a, wins1, loss1, draw1, nc1, result), 
            by = c("fighter_id" = "fighter1id")) %>%
  arrange(desc(r1a)) %>%
  mutate(age = interval(as.Date(date_of_birth), Sys.Date()) %>% 
           time_length("years") %>%
           round(2),
         ape = reach_in_inches/height_in_inches,
         wins1 = if_else(result == "Win", wins1+1L, wins1),
         loss1 = if_else(result == "Loss", loss1+1L, loss1),
         draw1 = if_else(result == "Draw", draw1+1L, draw1),
         nc1 = if_else(result == "No Contest", nc1+1L, nc1)
         )%>%
  select(fighter_id, short_name, age, gender, reach = reach_in_inches, 
         height = height_in_inches, ape, weightclass = current_weight_class,
         lastFight = date, org, rating = r1a, wins = wins1, loss = loss1,
         draw = draw1, nc = nc1)


curFighters <- rankings %>% 
  filter(lastFight >= "2018-01-01")


avgStats_weightclass <- curFighters %>%
  filter(org == "Ultimate Fighting Championship") %>%
  group_by(gender, weightclass) %>%
  slice(1:25) %>%
  summarise(meanRat = mean(rating),
            maxRat = max(rating),
            minRat = min(rating),
            sdRat = sd(rating, na.rm = TRUE),
            age = mean(age, na.rm = TRUE),
            reach = mean(reach, na.rm = TRUE),
            height = mean(height, na.rm = TRUE),
            ape = mean(ape, na.rm = TRUE),
            count = n()
            ) %>% 
  arrange(desc(meanRat))  %>%
  filter(count > 1)



ratingHistory <- fightsElo %>%
  filter(!is.na(r1a)) %>%
  arrange(fight_id) %>%
  group_by(short_name1, fighter1id) %>%
  summarise(curRat = last(r1a),
            maxRat = max(r1a),
            minRat = min(r1a),
            rangeRat = max(r1a) - min(r1a)) %>%
  ungroup() %>%
  arrange(desc(maxRat))



##### ANALYZE TRILOGIES ######

rematches <- fightsElo %>% 
  group_by(fighter1id, fighter2id) %>% 
  summarise(num = n()) %>% 
  filter(num>=2) %>% 
  select(fighter1id, fighter2id) %>% 
  ungroup() %>%
  left_join(fightsElo) %>%
  distinct() %>%
  group_by(fighter1id, fighter2id, short_name1, short_name2) %>%
  summarise(first = nth(result, 1, default = NA_character_),
            second = nth(result, 2, default = NA_character_),
            third = nth(result, 3, default = NA_character_),
            fourth = nth(result, 4, default = NA_character_),
            seriesLength = n()) %>%
  ungroup() %>%
  left_join(ratingHistory %>% select(fighter1id, maxRat)) %>%
  left_join(ratingHistory %>% select(fighter1id, maxRat), 
            by = c("fighter2id" = "fighter1id"),
            suffix = c("1", "2"))

# prob of winner of first match winning second (66%)
rematches %>%
  group_by(first, second) %>%
  summarise(num = n()) %>%
  filter(first == "Win" & !is.na(second)) %>%
  mutate(prob = num/sum(num))


# prob of winner of first two winning third (69%)
rematches %>%
  group_by(first, second, third) %>%
  summarise(num = n()) %>%
  filter(first == "Win" & second == "Win" & !is.na(third)) %>%
  mutate(prob = num/sum(num))

# prob of loser of first but winner of second winning third (56%)
rematches %>%
  group_by(first, second, third) %>%
  summarise(num = n()) %>%
  filter(first == "Loss", second == "Win" & !is.na(third)) %>%
  mutate(prob = num/sum(num))


#########################



