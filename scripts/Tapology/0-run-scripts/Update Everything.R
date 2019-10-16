# Run following files to completely update everything

#source('./scripts/Tapology/0-run-scripts/Initial Run-tapology.R', echo=TRUE)
memory.limit(50000)
source('./scripts/Tapology/1-raw-data/update-scraper-tapology.R', echo=TRUE)
source('./scripts/Tapology/2-clean-data/cleanFights.R', echo=TRUE)
source('./scripts/Tapology/2-clean-data/fix_NC_fights_tapology.R', echo=TRUE)
source('./scripts/Tapology/11-decision-scraper/decisionScraper.R', echo = TRUE)
source('./scripts/Tapology/11-decision-scraper/appendDecision-tapology.R', echo = TRUE)
source('./scripts/Tapology/4-ratings/add_ratings-tapology.R', echo = TRUE)