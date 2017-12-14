library(gdata)
library(ggmap)
library(googleway)
library(readr)

#setwd()
nycbuildingstable <- read.csv("nyc_disclosure.csv", header = TRUE, sep = ",")
nycbuildingstable$Street.Number<-trim(nycbuildingstable$Street.Number)
nycbuildingstable$Street.Name<-trim(nycbuildingstable$Street.Name)

nycbuildingstable$fulladdress <- paste(nycbuildingstable$Street.Number, " ",nycbuildingstable$Street.Name,",", " ",nycbuildingstable$Borough,","," ","NY", " ",nycbuildingstable$Zip.Code, sep="")

sample_fulladdress <- na.omit(nycbuildingstable)

#geokey <- "API key"

register_google(key = geokey, account_type = "premium", day_limit = 100000)

coordinates <- geocode(sample_fulladdress$fulladdress)
sample_fulladdress$lon <-coordinates[1]
sample_fulladdress$lat <-coordinates[2]

write_csv(sample_fulladdress, "geocoded.csv", append = FALSE)

googlemapdf <- as.data.frame(c(sample_fulladdress$lat, sample_fulladdress$lon))

google_map(key = geokey, data = googlemapdf, location = googlemapdf) %>% add_heatmap()
