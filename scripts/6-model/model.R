library(tidyverse)
library(data.table)

filtfightsOdds <- readRDS(file = "./scripts/8-append-odds/data/filtfightsOdds.rds")

filtfightsOdds2 <- filtfightsOdds %>%
  filter(Result != "draw" & r1b > r2b|
         (r1b == r2b & ratIncrease1_3 > ratIncrease2_3)) %>%
  mutate(eloWinProb = 1/(1+10^((r2b-r1b)/400)),
         diffAge = Age1 - Age2,
         ResultBin = ifelse(Result == "win", 1, 0))

set.seed(1)
train <- filtfightsOdds2 %>%
  sample_n(0.8*nrow(filtfightsOdds2)) %>%
  select(-Method, -Method_d, -Event, -Date, -r1a, -r2a, -Fighter1, -Fighter2, -R, 
         -Time, -Referee, -nc1, -nc2)

test <- anti_join(filtfightsOdds2, train)


model <- glm(ResultBin ~ eloWinProb + diffAge +
             oppRat1_5 + oppRat2_5 + koLosses1 + koLosses2, 
             family = binomial(link="logit"), data = train)



summary(model)

fitted.results <- predict(model,newdata=test,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test$ResultBin, na.rm = TRUE)
print(paste('Accuracy',1-misClasificError))
table("predicted" = fitted.results, "actual" = test$Result2)


filtfightsOdds2$pred <- predict(model,newdata=filtfightsOdds2,type='response')

filtfightsOdds2 %>% filter(Date >= "2018-01-01") %>% View()
