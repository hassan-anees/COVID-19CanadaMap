library(tidyverse)
library(geojsonio)
library(sf)
library(leaflet)


options(scipen=10000)

#loading COVID rates as of april 21 2021
canadaData <- read.csv(file = "./covid19-download.csv")

recentData <- canadaData %>%  filter(date == "2021-04-21")
recentData <- head(recentData, 13) 

recentData  <- recentData %>% 
  rename(
    name = prname )

### Geo spatial data 
provinces <- geojson_sf("canada_provinces.geojson")

#TOTAL NUMBER OF CASES IN CANADA
inner_join(provinces, recentData, by = 'name') -> MapData 


m <- leaflet(MapData) %>%
  setView(-96, 37.8, 4) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))

m %>% addPolygons()

bins <- c(0, 1000, 5000, 10000, 50000, 100000, 300000, Inf)
pal <- colorBin("YlOrRd", domain = MapData$numconf, bins = bins)

m %>% addPolygons(
  fillColor = ~pal(numconf),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7) %>% 
  addLegend(pal = pal, values = ~numconf, opacity = 0.7, title = "Active COVID Cases",
            position = "topright") 


######### 

# Onatrio Graph -----------------------------------------------------------

#Ontario Spatial Data PHU Region  
library(rmapshaper)
library(rgdal)


##Covid CSV data
ontarioDataByRegion <- read.csv(file = "./cases_by_status_and_phu.csv")
#need to clean the data so that it is the most recent 

presentOntario <- ontarioDataByRegion %>% filter(FILE_DATE == "2021-04-22")
presentOntario <- head(presentOntario, 34)

presentOntario  <- presentOntario %>% 
  rename(
    PHU_ID = PHU_NUM
    )



#SHAPE FILE ATM, need to change this to geospatial data
publicHealthShp =  readOGR(dsn = "./Ministry_of_Health_Public_Health_Unit_Boundary-shp", layer = "Ministry_of_Health_Public_Health_Unit_Boundary")
#plot(publicHealthShp)
##spatial map data 

#inner_join(presentOntario, publicHealthShp, by = 'PHU_ID') -> ontarioCovidMapData
ontarioCovidMapData <- merge(publicHealthShp, presentOntario, by = "PHU_ID")

ontarioMap <- leaflet(ontarioCovidMapData) %>%
  setView(-96, 37.8, 4) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))

ontarioMap %>% addPolygons()

bins2 <- c(0, 20, 50, 100, 200, 500, 800, 1000, 1500, Inf)
pal <- colorBin("YlOrRd", domain =  ontarioCovidMapData$ACTIVE_CASES, bins = bins2)

ontarioMap %>% addPolygons(
  fillColor = ~pal(ACTIVE_CASES),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7) %>% 
  addLegend(pal = pal, values = ~ACTIVE_CASES, opacity = 0.7, title = "Active COVID Cases",
            position = "bottomleft") 


