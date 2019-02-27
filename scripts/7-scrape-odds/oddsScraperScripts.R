# Scrapes all fights from a given Event
eventOddsScraper <- function(eventLink, future = "n") {
  eventPage <- read_html(paste0("https://www.bestfightodds.com", eventLink))
  
  if(eventPage %>%
     html_nodes("td") %>%
     html_text() %>%
     .[1] %>%
     grepl("No betting lines available for this event", .)) return(tibble())
  
  
  fighters <- eventPage %>% 
    html_nodes("tr th a") %>%
    html_attr("href") %>%
    grep("fighters", ., value=T) %>%
    head(., length(.)/2) %>%
    gsub("/fighters/", "", .)
  
  if(length(fighters) == 0) return(tibble())
  
  books <- eventPage %>% 
    html_nodes("thead a") %>%
    html_text()
  
  odds <- eventPage %>%
    html_nodes(".odd td:nth-child(-n+14), .even td:nth-child(-n+14)") %>%
    html_text() %>%
    gsub("[^[:digit:]-]", "", .) %>%
    as.numeric() %>%
    head(length(books) * length(fighters)) %>%
    matrix(ncol = length(books), 
           nrow = length(fighters),
           byrow=T,
           dimnames = list(fighters, books))
  
  ### Pulls date listed on event page (warning, missing year) ###
  date_md <- eventPage %>%
    html_nodes(".table-header-date") %>%
    html_text() %>% 
    gsub("(.{3})(.* )(\\d*)(\\D*)", "\\1 \\3", .)
  
  if(length(date_md) == 0) return(tibble())
  
  ### Pulls last day odds were updated ###
  date_y <- eventPage %>%
    html_nodes(".table-last-changed span") %>%
    html_attr("title") %>%
    sub("(\\d{4}).*", "\\1", .) %>%
    sub("( \\d{,2})\\w{2}", "\\1", .) %>%
    as.Date(format = "%b %d %Y")
  
  # Provides date with correct year
  eventDate <- paste(date_md, substr(date_y, 1, 4)) %>% as.Date(format = "%b %d %Y")
  if(future == "y" & eventDate < today()) eventDate <- eventDate %m+% months(12)
  
  x <- 1:length(fighters)
  oppIndex <- x-1 + 2*(x %% 2)
  
  odds_tbl <- tibble(fighter = fighters, 
                     opponent = fighters[oppIndex], 
                     eventName = eventPage %>% 
                       html_nodes(".table-header a") %>% 
                       html_text(),
                     eventLink = eventLink, 
                     Date = eventDate) %>%
    cbind(odds %>% as.tibble)
  
  return(odds_tbl)
}

# Scrapes all events for a single fighter
oddsScraper <- function(link) {
  print(link)
  
  searched_links <- c()
  links_to_search <- c()
  odds_tbl <- tibble()
  
  fighterPage <- tryCatch({
    read_html(paste0("https://www.bestfightodds.com/fighters/", link))
  },
  error=function(cond) {
    message(cond)
    no_errors = FALSE
    return(fighterPage)
  },
  warning=function(cond) {
    message(cond)
    no_errors = FALSE
    return(fighterPage)
  })
  
  
  fighterEventLinks <- fighterPage %>% 
    html_nodes(".item-non-mobile a") %>%
    html_attr("href")
  
  index <- !(fighterEventLinks %in% odds_list$searched_events)
  fighterEventLinks <- fighterEventLinks[index]
  
  fighterEventNames <- fighterPage %>% 
    html_nodes(".item-non-mobile a") %>%
    html_text() %>%
    .[index]
  
  fighterDates <- fighterPage %>% 
    html_nodes("td.item-non-mobile:nth-child(6)") %>%
    html_text() %>%
    sub("( \\d{,2})\\w{2}", "\\1", .) %>%
    as.Date(format = "%b %d %Y") %>%
    .[index]
  
  if (length(fighterEventLinks) > 0) {
    for (i in 1:length(fighterEventLinks)) {
      if (fighterEventLinks[i] %in% odds_list$searched_events) next
      odds_tbl <- eventOddsScraper(fighterEventLinks[i]) %>%
        rbind(odds_tbl, .)
      print(i)
    }
  }
  searched_links <- searched_links %>% 
    append(link) %>% 
    unique
  
  searched_events <- fighterEventLinks
  
  links_to_search = odds_tbl$fighter[odds_tbl$fighter != link] %>% 
    unique
  
  
  return(list("odds" = odds_tbl,
              "searched_links" = searched_links,
              "searched_events" = searched_events,
              "toSearch" = links_to_search))
}

# Combines scraped odds of two fighters
bind_odds <- function(a, b) {
  odds_tbl = rbind(a[[1]], b[[1]])
  searched_links = append(a[[2]], b[[2]])
  searched_events = append(a[[3]], b[[3]]) %>% unique
  toSearch = append(a[[4]], b[[4]]) %>% 
    setdiff(searched_links)
  
  return(list("odds" = odds_tbl,
              "searched_links" = searched_links,
              "searched_events" = searched_events,
              "toSearch" = toSearch))
}