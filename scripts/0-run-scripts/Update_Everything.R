# Run following files to completely update everything

source('./scripts/1-raw-data/update-scraper.R', echo=TRUE)
source('./scripts/3-records/recordAdder.R', echo=TRUE)
source('./scripts/4-ratings/add_ratings.R', echo=TRUE)
source('./scripts/5-metrics/metrics.R', echo=TRUE)
source('./scripts/5-metrics/events.R', echo=TRUE)
source('./scripts/7-scrape-odds/futureOdds.R', echo=TRUE)
source('./scripts/7-scrape-odds/updateOdds.R', echo=TRUE)


source('./model-6/model.R', echo=TRUE)




