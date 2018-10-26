library(tidyverse)
library(data.table)

filtfights <- readRDS(file = "./metrics-5/filtfights.rds")

filtfights2 <- filtfights %>%
  filter(Result != "draw" & r1b > r2b|
         (r1b == r2b & ratIncrease1_3 > ratIncrease2_3)) %>%
  mutate(eloWinProb = 1/(1+10^((r2b-r1b)/400)),
         Result2 = ifelse(Result == "win", 1, 0))

set.seed(1)
train <- filtfights2 %>%
  sample_n(0.8*nrow(filtfights2)) %>%
  select(-Method, -Method_d, -Event, -Date, -r1a, -r2a, -Fighter1, -Fighter2, -R, 
         -Time, -Referee, -nc1, -nc2)

test <- anti_join(filtfights2, train)


model <- glm(Result2 ~ r1b + r2b + wins1 + wins2 + fightLag1 + 
             oppRat1_5 + oppRat2_5 + koLosses1 + koLosses2, 
             family = binomial(link="logit"), data = train)


summary(model)

fitted.results <- predict(model,newdata=test,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test$Result2, na.rm = TRUE)
print(paste('Accuracy',1-misClasificError))
table("predicted" = fitted.results, "actual" = test$Result2)
