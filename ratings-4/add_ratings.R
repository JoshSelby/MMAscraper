library(comperank)
library(tidyverse)

fights <- readRDS(file = "~/GitHub/MMAscraper/records-3/fights_records.rds")

fights2 <- fights %>%
  filter(Result %in% c("win", "draw") & Method != "DQ") %>%
  mutate(score1 = ifelse(Result == "win", 1, 0.5),
         score2 = ifelse(Result == "win", 0, 0.5)) %>%
  select(game = match_id, player1 = Link1, score1, player2 = Link2, score2)

fights2 <- as_widecr(fights2)



# 150 was shown to be the optimal K
elo <- add_elo_ratings(fights2, K=150, initial_ratings = 1000)
rankelo <- rank_elo(fights2, K=150, keep_rating = TRUE, initial_ratings = 1000) %>% arrange(ranking_elo)


fightsM <- fights %>%
  filter(Result %in% c("win", "draw") & Method != "DQ")

fightsElo <- full_join(fightsM, elo, by = c("match_id" = "game")) %>%
  select(match_id, Link1, Result, Link2, Method, Method_d, Event, Date, 
         rating1Before, rating2Before, rating1After, rating2After) %>%
  rename(r1b = rating1Before, r2b = rating2Before, r1a = rating1After, r2a = rating2After) %>%
  mutate_at(.vars = vars(r1b:r2a),
            .funs = round)

# Fighters' highest elo achieved
fightsElo %>% group_by(Link1) %>% filter(r1a == max(r1a)) %>% arrange(-r1a) %>% View

fightsEloLong <- fightsElo %>% 
  select(match_id, Link1, Date, r1a) %>%
  full_join(fightsElo %>%
              select(match_id, Link2, Date, r2a), 
            by = c("match_id" = "match_id", "Link1" = "Link2", 
                   "Date" = "Date", "r1a" = "r2a")) %>%
  rename(Link = Link1, rating = r1a)



dates <- fightsElo %>% .$Date %>% unique



topN <- tibble(Link = as.character())
for (i in dates) {
  topNi <- fightsEloLong %>%
    filter(Date <= i) %>%
    arrange(Date) %>%
    group_by(Link) %>%
    filter(rating == last(rating)) %>%
    arrange(-rating) %>%
    select(Link, rating) %>%
    unique() %>%
    head(15)
  colnames(topNi)[2] <- i
  topN <- merge(topN, topNi, all = TRUE) %>% unique()
  print(i)
}




test <- fightsEloLong %>% 
  filter(Date <= "2005-01-01") %>%
  arrange(Date) %>%
  group_by(Link) %>% 
  filter(rating == last(rating)) %>%
  arrange(-rating) %>%
  select(Link, rating) %>%
  rename("2005-01-01" = rating) %>%
  head(15)

