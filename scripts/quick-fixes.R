birthdayTable <- readRDS(file = "./scripts/2-clean-data/data/birthdayTable.rds")

birthdayTable <- birthdayTable %>%
  mutate(birthday = if_else(link == "Priscila-Cachoeira-227399", as.Date("1988-08-19"), birthday),
         birthday = if_else(link == "Danny-Henry-59830", as.Date("1988-07-17"), birthday),
         birthday = if_else(link == "Nicolae-Negumereanu-237147", as.Date("1994-09-04"), birthday),
         birthday = if_else(link == "Cortney-Casey-91121", as.Date("1987-05-05"), birthday),
         birthday = if_else(link == "Valentin-Moldavsky-191187", as.Date("1992-02-06"), birthday),
         birthday = if_else(link == "Daniel-Zellhuber-238903", as.Date("1999-07-07"), birthday),
         birthday = if_else(link == "Rodrigo-Vera-191425", as.Date("1995-10-08"), birthday),
         birthday = if_else(link == "Grigory-Popov-131039", as.Date("1984-04-18"), birthday),
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
         Fighter1=ifelse(Link1=="Jussier-da-Silva-36939", "Jussier Formiga", Fighter1),
         Fighter2=ifelse(Link2=="Jussier-da-Silva-36939", "Jussier Formiga", Fighter2),
         ) %>%
  filter(Event != "RITC - Rage in the Cage OKC 35" &
         Event != "NOH 5 - Horror Show: Sportowe Zaglebie vs. Reszta Swiata" &
         !(Event=="Rings - Rings/The Outsider - Ota Gymnasium Special" & Link1=="Keinosuke-Yoshinaga-31182"))

saveRDS(fights, "./scripts/2-clean-data/data/fights_clean.rds")
