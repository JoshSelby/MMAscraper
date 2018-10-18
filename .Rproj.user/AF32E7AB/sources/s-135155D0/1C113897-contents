library(tidyverse)
library(data.table)
library(zoo)

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

fightMetrics <- full_join(fights1, fights2) %>%
  arrange(match_id) %>% 
  as.data.table()

rm(fights1, fights2)


# Create new metrics
fightMetrics[fightMetrics[, .I[.N > 5], Link1]$V1, 
        ':='(fightLag1 = as.numeric(Date - lag(Date)), # Days since last fight
            ratIncrease1 = r1b - lag(r1b), # Rating increase since last fight
            ratIncrease1_3 = r1b - lag(r1b,3), # Rating increase from 3 fights ago
            oppRat1_3 = lag(rollmeanr(r2b, 3, fill=NA)), # Average rating of last 3 opponents
            highestDef1_5 = lag(rollmaxr((Result=="win")*r2b, 5, fill=NA)), # Highest rated fighter defeated in last 5 fights
            koLosses1 = lag(cumsum(Result == "loss" & 
                                  (Method=="TKO"|Method=="KO"))) # number of KO losses
        ),
        by=Link1]


fightMetrics[fightMetrics[, .I[.N > 5], Link2]$V1, 
        ':='(fightLag2 = as.numeric(Date - lag(Date)),
             ratIncrease2 = r2b - lag(r2b),
             ratIncrease2_3 = r2b - lag(r2b,3),
             oppRat2_3 = lag(rollmeanr(r1b, 3, fill=NA)),
             highestDef2_5 = lag(rollmaxr((Result=="loss")*r1b, 5, fill=NA)),
             koLosses2 = lag(cumsum(Result == "win" & 
                                      (Method=="TKO"|Method=="KO")))
        ),
        by=Link2]


# Check with reem fights
fighter <- fightMetrics[grepl("Alistair-Overeem", Link1)]
fighter %>%
  select(-Method, -Method_d, -Event, -Fighter1, -Fighter2, -R, -Time, -Referee,
         -wins1, -wins2, -loss1, -loss2, -draw1, -draw2, -nc1, -nc2) %>% 
  View()



saveRDS(fightMetrics, file = "./metrics-5/fightMetrics.rds")

