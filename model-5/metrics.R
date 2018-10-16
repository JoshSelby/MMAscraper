library(tidyverse)
library(data.table)
library(zoo)
library(profvis)

# Read in data
fights <- readRDS(file = "./ratings-4/fightsElo.rds")


# Double the data
fights1 <- fights
fights2 <- fights %>%
  mutate(Result = ifelse(Result=="win", "loss", Result))

colnames(fights2) <- fights2 %>% 
  colnames %>% 
  gsub(1,3,.) %>% 
  gsub(2,1,.) %>%
  gsub(3,2,.)

fights3 <- full_join(fights1, fights2) %>%
  arrange(match_id) %>% 
  as.data.table()


# Create new metrics
fights3[fights3[, .I[.N > 3], Link1]$V1, 
        ':='(fightLag1 = Date - lag(Date), # Days since last fight
            ratIncrease1 = r1b - lag(r1b), # Rating increase since last fight
            ratIncrease1_3 = r1b - lag(r1b,3), # Rating increase from 3 fights ago
            oppRat1_3 = lag(rollmeanr(r2b, 3, fill=NA)), # Average rating of last 3 opponents
            koLosses1 = lag(cumsum(Result == "loss" & 
                                  (Method=="TKO"|Method=="KO"))) # number of KO losses
        ),
        by=Link1]

fights3[fights3[, .I[.N > 3], Link2]$V1, 
        ':='(fightLag2 = Date - lag(Date),
             ratIncrease2 = r2b - lag(r2b),
             ratIncrease2_3 = r2b - lag(r2b,3),
             oppRat2_3 = lag(rollmeanr(r1b, 3, fill=NA)),
             koLosses2 = lag(cumsum(Result == "win" & 
                                      (Method=="TKO"|Method=="KO")))
        ),
        by=Link2]


# Check with Khabib fights
reem <- fights3[grepl("Alistair-Ove", Link1)]
reem %>%
  select(-Method, -Method_d, -Event, -Fighter1, -Fighter2, -R, -Time, -Referee,
         -wins1, -wins2, -loss1, -loss2, -draw1, -draw2, -nc1, -nc2) %>% 
  View()


  filter(wins1 + loss1 + draw1 >= 5,
         wins2 + loss2 + draw2 >= 5)



  A <- iris[50:150,]
  setDT(A)
  A[,stdev := rollmeanr(Petal.Width, k=3, fill=NA), by=Species]
  
  
  
  



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
