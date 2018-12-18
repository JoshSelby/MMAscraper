if (!require("comperank")) install.packages("comperank")
library(comperank)
if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)
if (!require("lubridate")) install.packages("lubridate")
library(lubridate)
if (!require("Rcpp")) install.packages("Rcpp")
library(Rcpp)

fights <- readRDS(file = "./scripts/3-records/data/fights_records.rds")

fights2 <- fights %>%
  filter(Result %in% c("win", "draw") & Method != "DQ") %>%
  mutate(score1 = ifelse(Result == "win", 1, 0.5),
         score2 = ifelse(Result == "win", 0, 0.5)) %>%
  select(game = match_id, player1 = Link1, score1, player2 = Link2, score2)

fights2 <- as_widecr(fights2)


# 150 was shown to be the optimal K
elo <- add_elo_ratings(fights2, K=150, initial_ratings = 1000)

fightsM <- fights %>%
  filter(Result %in% c("win", "draw") & Method != "DQ")

fightsElo <- full_join(fightsM, elo, by = c("match_id" = "game")) %>%
  select(match_id, Link1, Result, Link2, Method, Method_d, Event, Date, 
         rating1Before, rating2Before, rating1After, rating2After) %>%
  rename(r1b = rating1Before, r2b = rating2Before, r1a = rating1After, r2a = rating2After) %>%
  mutate_at(.vars = vars(r1b:r2a),
            .funs = round)

fightsEloLong <- fightsElo %>% 
  select(match_id, Link1, Date, r1a) %>%
  full_join(fightsElo %>%
              select(match_id, Link2, Date, r2a), 
            by = c("match_id" = "match_id", "Link1" = "Link2", 
                   "Date" = "Date", "r1a" = "r2a")) %>%
  rename(Link = Link1, rating = r1a) %>%
  arrange(Date)

fights <- merge(fightsElo, fightsM) %>% 
  arrange(match_id)

rm(elo, fights2, fightsElo, fightsEloLong, fightsM)

saveRDS(fights, file = "./scripts/4-ratings/data/fightsElo.rds")
