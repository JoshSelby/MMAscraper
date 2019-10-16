if (!require("httr")) install.packages("httr")
library(httr)
if (!require("xml2")) install.packages("xml2")
library(xml2)
if (!require("RSQLite")) install.packages("RSQLite")
library(RSQLite)
if (!require("rvest")) install.packages("rvest")
library(rvest)
if (!require("lubridate")) install.packages("lubridate")
library(lubridate)
if (!require("data.table")) install.packages("data.table")
library(data.table)

source('./scripts/Tapology/1-raw-data/parallel-scraper-tapology.R', echo=TRUE)

mydb <- dbConnect(RSQLite::SQLite(), "MMA-db.sqlite")

# Get all fights you already scraped
# Fix a few issues with both fighters winning or losing
fights <- dbReadTable(mydb, "fights") %>%
  as.data.table()

fighterAttributes <- dbReadTable(mydb, "fighterAttributes") %>%
  as.data.table()

lastDate <- fights[, max(event_date) %>% as.Date()]


events <- tibble(link = character(),
                 date = as.Date(x = integer(0), origin = "1970-01-01"))

currentYear <- Sys.Date() %>% year
i=1
# Get all recent events you haven't scraped yet
while(TRUE) {
  url <- paste0("https://www.tapology.com/fightcenter_events")
  
  fd <- list(
    group = "all",
    region = "",
    page = i,
    schedule = "results",
    sport = "all"
  )
  
  postdata <- POST(url = url, query = fd, encode = "form",
                   add_headers(
                     "Accept" = "text/javascript, application/javascript, application/ecmascript, application/x-ecmascript, */*; q=0.01",
                     "Content-Type" = "application/x-www-form-urlencoded; charset=UTF-8",
                     "Cookie" = "_ga=GA1.2.1873043703.1537368153; __utmz=88071069.1563301531.1.1.utmcsr=(direct)|utmccn=(direct)|utmcmd=(none); remember_id=149246; remember_token=315e68b7a95fa6cda391fc3e2ae0e1fb1466335ed9a15480558bd4ef8d52d832; __utmc=88071069; __utma=88071069.1873043703.1537368153.1563983348.1563985208.3; _tapology_mma_session=Z2RWaU1XZ0hOQmIwcUhjN1Bac0twN0JZQktnVUlLUjVsVkdMMDR4bTBITGdnSDFlRW9WeHprQ2lRaWdJM0lRbW5PNTFYSG9kbVlaMWFlR3liZmEyZWhnRWVVNm03UVIwRUJLWHl1MmJXRlQ1dEFJTGJsTnVLQWx4MWpUMTJOYlBxQ1N1Y0pQREZlZTNzMDA0NTJINEpLS2FMNXZvaXZjQ3g2dFMzM1dJeTRmekc4TG5JTk9YZDlZdWx5WnpZd3luZlY1ZXliQ0RWS1B1aXJYQnpqVVp4UT09LS10am5XNVI0c0pXa2p1dHJ5OW9PME5nPT0%3D--7488fef85f733279f15da594ea47f0345aa16938",
                     "Host" = "www.tapology.com",
                     "Origin" = "https://www.tapology.com",
                     "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/75.0.3770.100 Safari/537.36",
                     "Referer" = "https://www.tapology.com/fightcenter",
                     "X-CSRF-Token" = "NS9M1Y5RMShdIfFaIKpYiqr+JuOZ8kwZvn9KSW7daZmgT9eJ4Q0ZyGLZSUHR4wjCdiE840HcQzLHHZSe0WgVJw==",
                     "X-Requested-With" = "XMLHttpRequest"
                   )
  )
  
  resp <- content(postdata, "text") 
  
  
  resp2 <- gsub('\\$\\("\\.fightcenterEvents")\\.html\\(\\"', "", 
                resp %>% substr(1,nchar(.)-4)) %>%
    gsub("\\\\n", "", .) %>%
    gsub("\\\\", "", .) %>%
    gsub("\\s{2,}", "", .)
  
  doc <- read_html(resp2, options = "HUGE") %>%
    gsub("\\\\'", "", .) %>% 
    read_html(options = "HUGE")
  
  eventLinks <- doc %>% 
    html_nodes(".promotion .name a") %>% 
    html_attr("href")
  
  monthsVec <- doc %>% 
    html_nodes(".promotion .datetime") %>% 
    html_text() %>% 
    gsub("(.*, )(.*)( \\d*,.*)", "\\2", .)
  
  
  changeYear <- ((match(monthsVec %>% head(1), month.name) - 
    match(monthsVec %>% tail(1), month.name)) < 0)
  
  currentYear <- currentYear - changeYear
  
  pasteYears <- ((match(monthsVec, month.name) - 
                     match(monthsVec %>% tail(1), month.name)) < 0) %>% 
    as.numeric +
    currentYear
    
  
  eventDates <- doc %>% 
    html_nodes(".promotion .datetime") %>% 
    html_text() %>% 
    gsub("(.*, )(.*)(,.*)", "\\2", .) %>% 
    paste(pasteYears) %>% 
    as.Date(format = "%B %d %Y")
  
  events <- tibble(link = eventLinks, 
                   date = eventDates) %>%
    mutate(date = ifelse((Sys.Date() - date) < 0, date - years(1), date) %>% as.Date(origin = "1970-01-01")) %>%
    rbind(events) %>%
    unique %>%
    arrange(desc(date))
  
  print(paste0("event: ", i))
  if(events %>% pull(date) %>% min < lastDate) {
    events <- events %>% 
      filter(date >= lastDate)
    rm(i)
    break
  }
  i <- i+1
}


fighterLinks <- as.character()
i=1
for (i in i:nrow(events)) {
  url <- paste0("https://www.tapology.com", events$link[i])
  
  fighterLinks <- read_html(url) %>% 
    html_nodes(".fightCardFighterName a") %>% 
    html_attr("href") %>%
    c(fighterLinks) %>%
    unique()
  print(paste0("event scraped: ", i))
  if(i == nrow(events)) {
    print(paste0("finished scraping events for fighter links"))
  }
}

toSearch <- tibble(link = fighterLinks %>% gsub("/fightcenter/fighters/", "", .),
                   fighter_id = NA_real_) %>%
  mutate(fighter_id = gsub("(\\d+)(-.*)", "\\1", link) %>% as.numeric) %>%
  left_join(fights %>% 
              select(fighter_url, fighter.data.id) %>% 
              unique, by=c("link" = "fighter_url")) %>%
  mutate(fighter_id = coalesce(fighter_id, as.numeric(fighter.data.id))) %>%
  select(link, fighter_id)

fights_list2 <- list(fights = tibble(),
                    fighterAttributes = tibble(),
                    searched = tibble(),
                    toSearch = toSearch
                    )

i=0
while (TRUE) {
  fights_init <- fights_list2$fights
  searched_init <- fights_list2$searched
  toSearch_init <- fights_list2$toSearch
  fighterAttributes_init <- fights_list2$fighterAttributes
  
  parScrap(fights_list2, min(nrow(fights_list2$toSearch), 100))
  
  fights_list2$fights <- rbind(fights_init, fights_list2$fights) %>% unique
  fights_list2$fighterAttributes <- rbind(fighterAttributes_init, fights_list2$fighterAttributes) %>% unique
  fights_list2$toSearch <- toSearch_init %>% anti_join(fights_list2$searched)
  fights_list2$searched <- rbind(searched_init, fights_list2$searched) %>% unique
  
  
  print(paste(nrow(fights_list2$fights), "fights written"))
  print(paste(nrow(fights_list2$fighterAttributes), "fighterAttributes written"))
  
  
  i=i+1
  print(i)
  if(nrow(fights_list2$toSearch)<1)
    break
}

fights <- rbind(fights, fights_list2$fights) %>% 
  distinct()


setkey(fights, id)
fightsSymmetry <- fights[, .(num = .N), id]


fights <- fights %>%
  .[fightsSymmetry[num>1]] %>%
  .[fights %>%
      .[fightsSymmetry[num>1]] %>%
      .[, .I[which.max(as.Date(lastUpdate))], by = id] %>%
      .$V1 
    ] %>%
  list(., fights[fightsSymmetry[num==1]]) %>%
  rbindlist() %>%
  .[,num := NULL] %>%
  .[order(lastUpdate)]


pro_cols = fighterAttributes %>% 
  names() %>%
  grep("^pro_|^amateur_|age_in_", ., value = T)

in_cols = fighterAttributes %>% 
  names() %>%
  grep("reach_in_|height_in_", ., value = T)


fighterAttributes <- fighterAttributes %>%
  list(., fights_list2$fighterAttributes %>% as.data.table()) %>%
  rbindlist(use.names = TRUE) %>%
  distinct() %>%
  .[order(fighter_id)] %>%
  .[, (pro_cols) := lapply(.SD, function(x) as.integer(x)), .SDcols = pro_cols] %>%
  .[, (in_cols) := lapply(.SD, function(x) as.numeric(x)), .SDcols = in_cols] %>%
  setkey(fighter_id)

fighterAttributes <- fighterAttributes %>% 
  .[fighterAttributes %>%
      .[, .I[which.max(sum(pro_wins + pro_losses + pro_draws + pro_no_contests + 
                             amateur_wins + amateur_losses + amateur_draws + amateur_no_contests))], by = fighter_id] %>%
      .$V1
    ]

dbWriteTable(mydb, "fights", fights, overwrite = T)
dbWriteTable(mydb, "fighterAttributes", fighterAttributes, overwrite = T)

rm(list=ls()[! ls() %in% c("fights", "fighterAttributes")])
