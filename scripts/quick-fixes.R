birthdayTable <- readRDS(file = "./scripts/2-clean-data/data/birthdayTable.rds")

birthdayTable <- birthdayTable %>%
  mutate(birthday = if_else(link == "Callan-Potter-105689", as.Date("1984-09-06"), birthday),
         birthday = if_else(link == "Sergei-Pavlovich-184051", as.Date("1992-05-13"), birthday),
         birthday = if_else(link == "Nathaniel-Wood-82395", as.Date("1993-08-05"), birthday),
         birthday = if_else(link == "Cory-Sandhagen-112869", as.Date("1992-04-20"), birthday),
         birthday = if_else(link == "Daniel-Teymur-163939", as.Date("1988-02-03"), birthday),
         birthday = if_else(link == "Peter-Queally-81180", as.Date("1989-02-19"), birthday),
         birthday = if_else(link == "Brendan-Allen-201703", as.Date("1995-12-28"), birthday),
         birthday = if_else(link == "Valerie-Loureda-313891", as.Date("1998-07-19"), birthday),
         link=if_else(link=="Dong-Hyun-Kim-21673", "Dong-Hyun-Ma-21673", link)
         )

saveRDS(birthdayTable, "./scripts/2-clean-data/data/birthdayTable.rds")


fights <- readRDS("./scripts/2-clean-data/data/fights_clean.rds")
fights <- fights %>% 
  mutate(Link1=ifelse(Link1=="Dong-Hyun-Kim-21673", "Dong-Hyun-Ma-21673", Link1),
         Link2=ifelse(Link2=="Dong-Hyun-Kim-21673", "Dong-Hyun-Ma-21673", Link2),
         Fighter1=ifelse(Link1=="Dong-Hyun-Ma-21673", "Dong Hyun Ma", Fighter1),
         Fighter2=ifelse(Link2=="Dong-Hyun-Ma-21673", "Dong Hyun Ma", Fighter2)) %>%
  filter(Event != "RITC - Rage in the Cage OKC 35" &
         Event != "NOH 5 - Horror Show: Sportowe Zaglebie vs. Reszta Swiata" &
         !(Event=="Rings - Rings/The Outsider - Ota Gymnasium Special" & Link1=="Keinosuke-Yoshinaga-31182"))

saveRDS(fights, "./scripts/2-clean-data/data/fights_clean.rds")
