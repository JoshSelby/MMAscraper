if (!require("comperank")) install.packages("comperank")
library(comperank)
if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)
if (!require("lubridate")) install.packages("lubridate")
library(lubridate)
if (!require("Rcpp")) install.packages("Rcpp")
library(Rcpp)
library(RSQLite)

mydb <- dbConnect(RSQLite::SQLite(), "MMA-db.sqlite")


if(!exists("fightsMMA_clean_records")) {
  fightsMMA_clean_records <- dbReadTable(mydb, "fightsMMA_clean") %>%
    as.tibble()
}

fightsMMA_clean_records <- fightsMMA_clean_records %>%
  filter(!(fighter1id %in% c(11614, 10675) & fighter2id %in% c(11614, 10675) & date == "2010-09-30"))


fights_wide <- fightsMMA_clean_records %>%
  filter(!is.na(result_fix)) %>%
  mutate(score1 = if_else(result_fix>1, 1, result_fix),
         score1 = if_else(result_fix<0, 0, score1),
         score2 = 1 - score1,
         debut = if_else(wins1 + loss1 + draw1 == 0 | wins2 + loss2 + draw2 == 0, 1, 0)) %>%
  select(game = fight_id, player1 = fighter1id, score1, player2 = fighter2id, score2,
         amateur, debut)

fights_wide <- as_widecr(fights_wide)

elo_rate_fun2 <- function (rating1, score1, rating2, score2, debut = 0, amateur = 0, K = 150, ksi = 400) {
  K = if_else(amateur == 0, K, K * 2/3)
  K = if_else(mean(c(rating1, rating2)) < 1300, mean(c(rating1, rating2))/1300 * K, K)
  K = if_else(debut == 0, K, K * 1/2)
  K = if_else(rating1 <= 900 | rating2 <= 900, K * 1/2, K)
  prob_win1 <- 1/(1 + 10^((rating2 - rating1)/ksi))
  game_res1 <- score1/(score1+score2)
  rating_delta <- K * (game_res1 - prob_win1)
  return(c(c(rating1, rating2) + rating_delta * c(1, -1), K))
}

sourceCpp('./scripts/Tapology/4-ratings/testRcpp.cpp')


add_iterative_ratings2 <- function(cr_data, rate_fun, initial_ratings = 0) {
  if("amateur" %in% colnames(cr_data)) {
    ammyFights <- cr_data %>% select(game, amateur) %>% distinct()
    cr_data$amateur <- NULL
  }
  if("debut" %in% colnames(cr_data)) {
    debutFights <- cr_data %>% select(game, debut) %>% distinct()
    cr_data$debut <- NULL
  }
  cr <- as_longcr(cr_data, repair = TRUE)
  comperank:::assert_pairgames(cr)
  used_players <- comperank:::unique_levels(cr$player)
  ref_players <- used_players[!is.na(used_players)]
  cr <- cr %>% as_widecr(repair = FALSE) %>% 
    filter((.data$player1 %in% 
              used_players) | is.na(.data$player1), 
           (.data$player2 %in% 
              used_players) | is.na(.data$player2)) %>% 
    mutate(.player1_id = comperank:::to_players_id(.data$player1, ref_players),
           .player2_id = comperank:::to_players_id(.data$player2, ref_players))
  
  if(exists("ammyFights")) {
    cr <- cr %>% left_join(ammyFights)
  }
  else {
    cr$amateur <- 0
  }
  if(exists("debutFights")) {
    cr <- cr %>% left_join(debutFights)
  }
  else {
    cr$debut <- 0
  }
  init_ratings <- comperank:::get_cr_initial_ratings(players = ref_players, 
                                         initial_ratings)
  ratings <- compute_iterative_ratings(rate_fun = rate_fun, 
                   player1_id = cr[[".player1_id"]], score1 = cr$score1, 
                   player2_id = cr[[".player2_id"]], score2 = cr$score2, 
                   amateur = cr$amateur, debut = cr$debut,
                   initial_ratings = init_ratings) %>% as_tibble()
  cr %>%
    bind_cols(y = ratings) %>% 
    select(-.data[[".player1_id"]], 
           -.data[[".player2_id"]]) %>% 
    as_widecr(repair = FALSE)
}


# 150 was shown to be the optimal K
elo <- add_iterative_ratings2(fights_wide, rate_fun=elo_rate_fun2, initial_ratings = 1000)


elo <- elo %>%
  bind_rows(
    elo %>% 
      mutate(player1temp = player1,
             player1 = player2,
             player2 = player1temp,
             score1 = 1 - score1,
             score2 = 1 - score2,
             rating1Beforetemp = rating1Before,
             rating1Before = rating2Before,
             rating2Before = rating1Beforetemp,
             rating1Aftertemp = rating1After,
             rating1After = rating2After,
             rating2After = rating1Aftertemp
             )
    ) %>%
  select(colnames(elo))


fightsElo <- full_join(fightsMMA_clean_records, elo, 
                       by = c("fight_id" = "game",
                              "fighter1id" = "player1",
                              "fighter2id" = "player2",
                              "amateur")) %>%
  select(fight_id, short_name1, result, short_name2, method, method_d, event, org, 
         date, rating1Before, rating2Before, rating1After, rating2After, score1, 
         fighter1id, fighter2id, fight_num, rematch, bout_url, weight_class,
         weight_class_lb, billing, referee, round_structure, round, time, time_total,
         odds1, odds2, wins1, loss1, draw1, nc1, wins2, loss2, draw2, nc2,
         result_fix, amateur, kFactor) %>%
  rename(r1b = rating1Before, r2b = rating2Before, r1a = rating1After, r2a = rating2After) %>%
  mutate_at(.vars = vars(r1b:r2a),
            .funs = round) %>%
  arrange(fight_id)


dbWriteTable(mydb, "fightsElo", fightsElo, overwrite = T)

rm(fights_wide, elo)
