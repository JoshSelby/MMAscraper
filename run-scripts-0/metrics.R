library(tidyverse)
library(zoo)

fights <- readRDS(file = "./ratings-4/fightsElo.rds") %>% as.tibble()

fights1 <- fights

fights2 <- fights %>%
  mutate(Result = ifelse(Result=="win", "loss", Result))

colnames(fights2) <- fights2 %>% 
  colnames %>% 
  gsub(1,3,.) %>% 
  gsub(2,1,.) %>%
  gsub(3,2,.)

fights3 <- full_join(fights1, fights2) %>%
  arrange(match_id)

fights3 <- fights3 %>% group_by(Link1) %>%
  mutate(fightLag1 = Date - lag(Date),
         ratIncrease1 = r1b - lag(r1b),
         ratIncrease1_3 = r1b - lag(r1b,3),
         oppRat1_3 = lag(rollapply(r2b, 3, mean, align='right', fill=NA),1)
  ) %>%
  ungroup(Link1) %>%
  group_by(Link2) %>%
  mutate(fightLag2 = Date - lag(Date),
         ratIncrease2 = r2b - lag(r2b),
         ratIncrease2_3 = r2b - lag(r2b,3),
         oppRat2_3 = lag(rollapply(r1b, 3, mean, align='right', fill=NA),1)
  ) %>%
  ungroup(Link2)

fights3 %>%
  select(-Method, -Method_d, -Event, -Fighter1, -Fighter2, -R, -Time, -Referee,
         -wins1, -wins2, -loss1, -loss2, -draw1, -draw2, -nc1, -nc2) %>% 
  filter(grepl("Khabib-Nur", Link1)) %>%
  View()


  filter(wins1 + loss1 + draw1 >= 5,
         wins2 + loss2 + draw2 >= 5)







fights3a <- fights3 %>%
  filter(r1b > r2b)

fights3b <- fights3 %>%
  filter(r1b == r2b)

fights3b1 <- fights3b %>%
  filter(wins1 > wins2)

fights3b2 <- fights3b %>%
  filter(wins1 == wins2,
         loss1 < loss2)

fights3b3 <- fights3b %>%
  filter(wins1 == wins2,
         loss1 == loss2,
         draw1 > draw2)

fights3b4 <- fights3b %>%
  filter(wins1 == wins2,
         loss1 == loss2,
         draw1 == draw2,
         Link1 < Link2)

fights3b14 <- full_join(fights3b1, fights3b2) %>%
  full_join(fights3b3) %>%
  full_join(fights3b4)


fights3b5 <- fights3 %>%
  filter(r1b == r2b,
         wins1 >= wins2,
         loss1 <= loss2,
         draw1 >= draw2,
         Link1 <= Link2)

fights4 <- full_join(fights3b1, fights3b2) %>%
  full_join(fights3b3) %>%
  full_join(fights3b4) %>%
  full_join(fights3a) %>%
  arrange(match_id)

rm(fights3, fights3a, fights3b, fights3b1, fights3b2, fights3b3, fights3b4)


rm(fights1, fights2)
