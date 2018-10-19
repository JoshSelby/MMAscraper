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
fightMetrics[, 
        ':='(fightLag1 = as.numeric(c(0, diff(Date))), # Days since last fight
            ratIncrease1 = c(0, diff(r1b)), # Rating increase since last fight
            ratIncrease1_3 = c(0, cumsum(diff(r1b[1:3])), 
                               diff(r1b, 3)), # Rating increase from 3 fights ago
            oppRat1_5 = lag(c(cumsum(r2b[1:4])/1:4,
                              rollmeanr(r2b, 5))), # Average rating of last 3 opponents
            highestDef1_5 = lag(c(cummax(((Result=="win")*r2b)[1:4]), 
                              rollmaxr((Result=="win")*r2b, 5))), # Highest rated fighter defeated in last 5 fights
            koLosses1 = lag(cumsum(Result == "loss" & 
                                  (Method=="TKO"|Method=="KO"))), # number of KO losses
            diff1_5 = lag(c(cumsum((r1b-r2b)[1:4]),
                            rollsumr((r1b-r2b), 5)))
        ),
        by=Link1]



fightMetrics[, 
        ':='(fightLag2 = as.numeric(c(0, diff(Date))),
             ratIncrease2 = c(0, diff(r2b)),
             ratIncrease2_3 = c(0, cumsum(diff(r2b[1:3])), 
                                diff(r2b, 3)),
             oppRat2_5 = lag(c(cumsum(r1b[1:4])/1:4,
                               rollmeanr(r1b, 5))),
             highestDef2_5 = lag(c(cummax(((Result=="loss")*r1b)[1:4]), 
                                   rollmaxr((Result=="loss")*r1b, 5))),
             koLosses2 = lag(cumsum(Result == "win" & 
                                      (Method=="TKO"|Method=="KO"))),
             diff2_5 = lag(c(cumsum((r2b-r1b)[1:4]),
                             rollsumr((r2b-r1b), 5)))
        ),
        by=Link2]


# Check with reem fights
fighter <- fightMetrics[grepl("Colby", Link1)]

fightMetrics %>%
  select(-Method, -Method_d, -Event, -Fighter1, -Fighter2, -R, -Time, -Referee,
         -wins1, -wins2, -loss1, -loss2, -draw1, -draw2, -nc1, -nc2) %>% 
  View()



saveRDS(fightMetrics, file = "./metrics-5/fightMetrics.rds")

