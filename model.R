library(tidyverse)
library(data.table)

filtfights <- readRDS(file = "./metrics-5/filtfights.rds")

filtfights2 <- filtfights %>%
  filter(Result != "draw" & r1b > r2b|
         (r1b == r2b & ratIncrease1_3 > ratIncrease2_3)) %>%
  mutate(Result2 = ifelse(Result == "win", 1, 0))

train <- filtfights2 %>%
  sample_n(0.8*nrow(filtfights2)) %>%
  select(-Result, -Method, -Method_d, -Event, -Date, -r1a, -r2a, -Fighter1, -Fighter2, -R, 
         -Time, -Referee, -nc1, -nc2)

test <- anti_join(filtfights2, train)


model <- glm(Result2 ~ r1b + r2b + wins1 + loss1 + wins2 + loss2 + fightLag1 + fightLag2 +
             ratIncrease1 + ratIncrease2 + ratIncrease1_3 + ratIncrease2_3 + oppRat1_3 + 
             oppRat2_3 + koLosses1 + koLosses2, family = binomial(link="logit"), data = train)


summary(model)

anova(model, test="Chisq")

library(pscl)
pR2(model)
pR2(model2)


fitted.results <- predict(model,newdata=test,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test$Result2)
print(paste('Accuracy',1-misClasificError))


library(ROCR)
p <- predict(model, newdata=test, type="response")
pr <- prediction(p, test$Result2)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
