birthdayTable <- readRDS(file = "./scripts/2-clean-data/data/birthdayTable.rds")

birthdayTable <- birthdayTable %>%
  mutate(link = if_else(link=="Jussier-da-Silva-36939", "Jussier-Formiga-36939", link),
         birthday = if_else(link == "Rodrigo-Vera-191425", as.Date("1995-10-08"), birthday),
         birthday = if_else(link == "Grigory-Popov-131039", as.Date("1984-04-18"), birthday),
         birthday = if_else(link == "Allen-Crowder-157353", as.Date("1989-11-08"), birthday),
         birthday = if_else(link == "Jussier-Formiga-36939", as.Date("1985-04-14"), birthday),
         birthday = if_else(link == "Dalcha-Lungiambula-167487", as.Date("1985-07-31"), birthday),
         link=if_else(link=="Dong-Hyun-Kim-21673", "Dong-Hyun-Ma-21673", link)
         )

saveRDS(birthdayTable, "./scripts/2-clean-data/data/birthdayTable.rds")


fights <- readRDS("./scripts/2-clean-data/data/fights_clean.rds")
fights <- fights %>% 
  mutate(Link1=ifelse(Link1=="Holli-Salazar-243759", "Holli-Logan-243759", Link1),
         Link2=ifelse(Link2=="Holli-Salazar-243759", "Holli-Logan-243759", Link2),
         Fighter1=ifelse(Link1=="Holli-Logan-243759", "Holli Logan", Fighter1),
         Fighter2=ifelse(Link2=="Holli-Logan-243759", "Holli Logan", Fighter2),
         Link1=ifelse(Link1=="Jussier-da-Silva-36939", "Jussier-Formiga-36939", Link1),
         Link2=ifelse(Link2=="Jussier-da-Silva-36939", "Jussier-Formiga-36939", Link2),
         Fighter1=ifelse(Link1=="Jussier-Formiga-36939", "Jussier Formiga", Fighter1),
         Fighter2=ifelse(Link2=="Jussier-Formiga-36939", "Jussier Formiga", Fighter2)
         ) %>%
  filter(Event != "RITC - Rage in the Cage OKC 35" &
         Event != "NOH 5 - Horror Show: Sportowe Zaglebie vs. Reszta Swiata" &
         !(Event=="Rings - Rings/The Outsider - Ota Gymnasium Special" & Link1=="Keinosuke-Yoshinaga-31182"))

saveRDS(fights, "./scripts/2-clean-data/data/fights_clean.rds")
