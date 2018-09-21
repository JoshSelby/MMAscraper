library(comperank)
library(tidyverse)

fights <- readRDS(file = "~/GitHub/MMAscraper/records-3/fights_records.rds")

fights2 <- fights %>%
  filter(Result %in% c("win", "draw")) %>%
  mutate(score1 = ifelse(Result == "win", 1, 0.5),
         score2 = ifelse(Result == "win", 0, 0.5)) %>%
  select(game = match_id, player1 = Link1, score1, player2 = Link2, score2)

fights2 <- as_widecr(fights2)




elo <- add_elo_ratings(fights2, K=150, initial_ratings = 1000)
rankelo <- rank_elo(fights2, K=150, keep_rating = TRUE, initial_ratings = 1000)
