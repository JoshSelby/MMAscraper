if (!require("comperank")) install.packages("comperank")
library(comperank)
if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)
if (!require("lubridate")) install.packages("lubridate")
library(lubridate)
if (!require("Rcpp")) install.packages("Rcpp")
library(Rcpp)

fights <- readRDS(file = "./scripts/11-decision-scraper/data/fights.RDS")

# Change select fights from NC to Wins/Losses
{
  fights <- fights %>%
    mutate(Result2 = Result,
           Result2 = ifelse(Link1 == "Daniel-Cormier-52311" & Link2 == "Jon-Jones-27944" & Date == "2017-07-29", "loss", Result2),
           Result2 = ifelse(Link1 == "Anderson-Silva-1356" & Link2 == "Nick-Diaz-2831" & Date == "2015-01-31", "win", Result2),
           Result2 = ifelse(Link1 == "Brian-Ortega-65310" & Link2 == "Mike-de-la-Torre-17915" & Date == "2014-07-26", "win", Result2),
           Result2 = ifelse(Link1 == "Khalil-Rountree-77674" & Link2 == "Michal-Oleksiejczuk-156397" & Date == "2017-12-30", "loss", Result2),
           Result2 = ifelse(Link1 == "Kelvin-Gastelum-74700" & Link2 == "Vitor-Belfort-156" & Date == "2017-03-11", "win", Result2),
           Result2 = ifelse(Link1 == "Adam-Milstead-60258" & Link2 == "Curtis-Blaydes-172939" & Date == "2017-02-04", "loss", Result2),
           Result2 = ifelse(Link1 == "Alex-Morono-64894" & Link2 == "Niko-Price-96921" & Date == "2017-02-04", "loss", Result2),
           Result2 = ifelse(Link1 == "Brock-Lesnar-17522" & Link2 == "Mark-Hunt-10668" & Date == "2016-07-09", "win", Result2),
           Result2 = ifelse(Link1 == "Bubba-Bush-29111" & Link2 == "Kevin-Casey-24121" & Date == "2014-07-05", "loss", Result2),
           Result2 = ifelse(Link1 == "Damon-Jackson-113767" & Link2 == "Rony-Mariano-Bezerra-38190" & Date == "2015-05-30", "loss", Result2),
           Result2 = ifelse(Link1 == "Hector-Lombard-11292" & Link2 == "Joshua-Burkman-10003" & Date == "2015-01-03", "win", Result2),
           Result2 = ifelse(Link1 == "Jerrod-Sanders-48156" & Link2 == "Pedro-Munhoz-52407" & Date == "2014-10-04", "loss", Result2),
           Result2 = ifelse(Link1 == "Keith-Berish-46314" & Link2 == "Robert-Drysdale-67894" & Date == "2014-07-06", "loss", Result2),
           Result2 = ifelse(Link1 == "Louis-Gaudinot-45230" & Link2 == "Phil-Harris-8753" & Date == "2014-03-08", "win", Result2),
           Result2 = ifelse(Link1 == "Chico-Camus-42850" & Link2 == "Yaotzin-Meza-15937" & Date == "2014-01-25", "win", Result2),
           Result2 = ifelse(Link1 == "Dennis-Siver-9817" & Link2 == "Manny-Gamburyan-5185" & Date == "2013-12-28", "win", Result2),
           Result2 = ifelse(Link1 == "Yancy-Medeiros-27738" & Link2 == "Yves-Edwards-344" & Date == "2013-11-06", "win", Result2),
           Result2 = ifelse(Link1 == "Jessica-Eye-39575" & Link2 == "Sarah-Kaufman-16524" & Date == "2013-10-19", "win", Result2),
           Result2 = ifelse(Link1 == "Jim-Miller-14463" & Link2 == "Pat-Healy-6246" & Date == "2013-04-27", "loss", Result2),
           Result2 = ifelse(Link1 == "Alex-Caceres-41586" & Link2 == "Kyung-Ho-Kang-24067" & Date == "2013-03-03", "win", Result2),
           Result2 = ifelse(Link1 == "Che-Mills-8800" & Link2 == "Matt-Riddle-34072" & Date == "2013-02-16", "loss", Result2),
           Result2 = ifelse(Link1 == "Igor-Pokrajac-7621" & Link2 == "Joey-Beltran-21219" & Date == "2012-12-15", "loss", Result2),
           Result2 = ifelse(Link1 == "Stanislav-Nedkov-27115" & Link2 == "Thiago-Silva-14396" & Date == "2012-11-10", "loss", Result2),
           Result2 = ifelse(Link1 == "Ed-Herman-6561" & Link2 == "Jake-Shields-502" & Date == "2012-08-11", "loss", Result2),
           Result2 = ifelse(Link1 == "Francisco-Rivera-11908" & Link2 == "Roland-Delorme-37351" & Date == "2012-07-21", "win", Result2),
           Result2 = ifelse(Link1 == "Chris-Clements-13469" & Link2 == "Matt-Riddle-34072" & Date == "2012-07-21", "loss", Result2),
           Result2 = ifelse(Link1 == "Brandon-Vera-4886" & Link2 == "Thiago-Silva-14396" & Date == "2011-01-01", "loss", Result2),
           Result2 = ifelse(Link1 == "Dong-Hyun-Kim-16374" & Link2 == "Karo-Parisyan-5153" & Date == "2009-01-31", "loss", Result2),
           Result2 = ifelse(Link1 == "Brandon-Vera-4886" & Link2 == "Thiago-Silva-14396" & Date == "2011-01-01", "loss", Result2)
    )
}


fights2 <- fights %>%
  filter(Result2 %in% c("win", "draw", "loss") & Method != "DQ") %>%
  mutate(score1 = ifelse(Result2 == "win", 1, 0.5),
         score1 = ifelse(Result2 == "loss", 0, score1),
         score1 = ifelse(!is.na(fighter1winAttr), fighter1winAttr, score1),
         score1 = ifelse(score1>1, 1, score1),
         score2 = 1-score1,
         Result2 = score1) %>%
  select(game = match_id, player1 = Link1, score1, player2 = Link2, score2)

fights2 <- as_widecr(fights2)

elo_rate_fun2 <- function (rating1, score1, rating2, score2, K = 150, ksi = 400) {
  prob_win1 <- 1/(1 + 10^((rating2 - rating1)/ksi))
  game_res1 <- score1/(score1+score2)
  rating_delta <- K * (game_res1 - prob_win1)
  c(rating1, rating2) + rating_delta * c(1, -1)
}

# 150 was shown to be the optimal K
elo <- add_iterative_ratings(fights2, rate_fun=elo_rate_fun2, initial_ratings = 1000)

fightsElo <- full_join(fights, elo, by = c("match_id" = "game")) %>%
  select(match_id, Link1, Result, Link2, Method, Method_d, Event, Date, 
         rating1Before, rating2Before, rating1After, rating2After, score1) %>%
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

fightsElo <- merge(fightsElo, fights) %>% 
  arrange(match_id)


saveRDS(fightsElo, file = "./scripts/4-ratings/data/fightsElo.rds")

rm(list=ls())
