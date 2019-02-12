birthdayTable <- readRDS(file = "./scripts/2-clean-data/data/birthdayTable.rds")

birthdayTable <- birthdayTable %>%
  mutate(birthday = if_else(link == "Callan-Potter-105689", as.Date("1984-09-06"), birthday),
         birthday = if_else(link == "Sergei-Pavlovich-184051", as.Date("1992-05-13"), birthday),
         birthday = if_else(link == "Nathaniel-Wood-82395", as.Date("1993-08-05"), birthday),
         birthday = if_else(link == "Cory-Sandhagen-112869", as.Date("1992-04-20"), birthday),
         link=if_else(link=="Dong-Hyun-Kim-21673", "Dong-Hyun-Ma-21673", link)
         )

saveRDS(birthdayTable, "./scripts/2-clean-data/data/birthdayTable.rds")


fights <- readRDS("./scripts/2-clean-data/data/fights_clean.rds")
fights <- fights %>% 
  mutate(Link1=ifelse(Link1=="Dong-Hyun-Kim-21673", "Dong-Hyun-Ma-21673", Link1),
         Link2=ifelse(Link2=="Dong-Hyun-Kim-21673", "Dong-Hyun-Ma-21673", Link2),
         Fighter1=ifelse(Link1=="Dong-Hyun-Ma-21673", "Dong Hyun Ma", Fighter1),
         Fighter2=ifelse(Link2=="Dong-Hyun-Ma-21673", "Dong Hyun Ma", Fighter2))

saveRDS(fights, "./scripts/2-clean-data/data/fights_clean.rds")