birthdayTable <- readRDS(file = "./scripts/2-clean-data/data/birthdayTable.rds")

birthdayTable <- birthdayTable %>%
  mutate(birthday = if_else(link == "Callan-Potter-105689", as.Date("1984-09-06"), birthday),
         birthday = if_else(link == "Sergei-Pavlovich-184051", as.Date("1992-05-13"), birthday),
         birthday = if_else(link == "Nathaniel-Wood-82395", as.Date("1993-08-05"), birthday),
         birthday = if_else(link == "Cory-Sandhagen-112869", as.Date("1992-04-20"), birthday)
         )

saveRDS(birthdayTable, "./scripts/2-clean-data/data/birthdayTable.rds")

