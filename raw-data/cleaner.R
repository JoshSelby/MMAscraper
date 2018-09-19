library(tidyverse)

fights_list <- readRDS("~/GitHub/MMAscraper/raw-data/fights_table.rds")
fights <- fights_list[[1]]

fights2 <- fights %>% 
  select(Fighter1 = Fighter2, Result, Fighter2 = Fighter1, Method, Method_d, R, Time, Referee,
         Event, Date, Link1 = Link2, Link2 = Link1) %>% 
  mutate(Result = ifelse(Result == "win", "loss1", Result),
         Result = ifelse(Result == "loss", "win", Result),
         Result = ifelse(Result == "loss1", "loss", Result))
fights <- rbind(fights, fights2)
fights <- unique(fights)

# Remove all fights where Link2 is an unknown fighter
# Remove fight where Link1=Link2
# Remove Marcus Vinicius Cruz
fights <- fights %>%
  filter(Fighter2 != "Unknown Fighter") %>%
  filter(Link1 != Link2)
  



timesAppearing <- fights %>% 
  select(Link1) %>% 
  table + 
  fights %>% 
  select(Link2) %>% 
  table()

oddNames <- timesAppearing[which(timesAppearing %% 2 == 1)] %>% names()

fights %>% filter(Link1 == oddNames[2] | Link2 == oddNames[2]) %>% 
  select(Link1, Link2) %>% 
  unlist(use.names = FALSE) %>% 
  table


test1 <- fights %>% filter(Result == "win") %>% select(Link1) %>% arrange(Link1) %>% .[[1]]
test2 <- fights %>% filter(Result == "loss") %>% select(Link2) %>% arrange(Link2) %>% .[[1]]
setdiff(test1, test2)


















# Remove Referee names from Method and Method_d columns
# Convert all foreign/accented letters to latin letters
# Remove /fighter/ from links
fights <- fights_list[[1]] %>%
  mutate(Method = Method %>% 
           str_remove(Referee) %>%
           stri_trans_totitle() %>%
           stri_trans_general("latin"),
         Method_d = Method_d %>% 
           str_remove(Referee) %>%
           stri_trans_totitle() %>%
           stri_trans_general("latin"),
         Fighter1 = stri_trans_general(Fighter1, "latin"),
         Fighter2 = stri_trans_general(Fighter2, "latin"),
         Referee = stri_trans_general(Referee, "latin"),
         Link1 = Link1 %>% gsub("/fighter/", "", .),
         Link2 = Link2 %>% gsub("/fighter/", "", .)
         ) %>%
  mutate_all(str_trim) %>%
  mutate_all(str_squish)

# Make individual corrections here
fights <- fights %>% 
  mutate(Referee = ifelse(Referee == 18062, "Steve Mazzagatti", Referee),
         Referee = ifelse(Referee == 194661, "", Referee),
         Referee = ifelse(Referee == "N/A", "", Referee),
         Referee = gsub("\\\\", "", Referee),
         Date = ifelse(grepl("GCVT - Gaisei Challenge", Event), "1994-07-25", Date),
         Date = ifelse(grepl("Combat Super Fight - Carangolas", Event), "2018-07-16", Date),
         Date = ifelse(grepl("NG - New Generation", Event) & Date=="0000-11-30", "2017-11-30", Date),
         Method = ifelse(Method=="Isaac Yap", "Submission", Method),
         Method_d = ifelse(Method_d=="Isaac Yap", "Triangle Choke", Method_d),
         Method_d = ifelse(Method == Method_d, "", Method_d),
         Method = ifelse(grepl("^Nc$|N/C|No Conest", Method), "No Contest", Method),
         Method = ifelse(Method == 83185, "", Method),
         Method = gsub("imssion$|mision$|misison$|missio$|misson$|mssion$|mbission$", "mission", Method),
         Method = gsub("Ubmission", "Submission", Method),
         Method = gsub("Techincal|Technial", "Technical", Method),
         Method = gsub("Sumission", "Submission", Method), 
         Method_d = gsub("No .* - ", "", Method),
         Method = gsub(" - .*", "", Method),
         Method = gsub("Tko|tko", "TKO", Method),
         Method = gsub("Submission.*", "Submission", Method)
         )


# Make corrections for unknown fighters here


fights %>% select(Method) %>% table %>% View
