library(RSQLite)


rm(list=ls())

mydb <- dbConnect(RSQLite::SQLite(), "MMA-db.sqlite")

dbExecute(mydb, "UPDATE fighterAttributes 
          SET date_of_birth = '1989-02-19', age_in_years = 30
          WHERE fighter_id = 21195")






fighterAttributes <- dbReadTable(mydb, "fighterAttributes") %>% 
  select(sherdog_url, date_of_birth) %>%
  mutate(sherdog_url = gsub("http://www.sherdog.com/fighter/", "", sherdog_url),
         date_of_birth = as.Date(date_of_birth))

inner_join(birthdayTable, fighterAttributes, by = c("link" = "sherdog_url")) %>% filter(birthday != date_of_birth)
