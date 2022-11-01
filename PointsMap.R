## ---------------------------
##
## Title: Points Maps
##
## Topic: 30DayMapChallenge, Map 1
##
## Author: Sebastian Hanika
##
## Date Created: 2022-10-21
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------


library(tidyverse)
library(sf)
library(geojsonsf)
library(osmdata)
library(ggpspatial)
library(downloader)
library(leaflet)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))



# load country data -------------------------------------------------------

# get data from eurostat
url_countries <- "https://gisco-services.ec.europa.eu/distribution/v2/countries/download/ref-countries-2020-10m.geojson.zip"
download(url = url_countries, 
         dest="countries.zip",
         mode="wb") # downloads zip folder into current directory

unzip("countries.zip",
      exdir = "data/europe_countries",
      files = "CNTR_RG_10M_2020_4326.geojson") # unzips file into current working directory

# loads geojson into R
nordics <- geojson_sf("data/europe_countries/CNTR_RG_10M_2020_4326.geojson") %>% 
  filter(ISO3_CODE %in% c("SWE", "FIN", "NOR", "DNK"))


# Load OSM data -----------------------------------------------------------

#get extent of nordic countries
nordics.ext <-  unname(st_bbox(nordics, crs = st_crs(4326)))

# build query
osm.query <- opq(bbox = nordics.ext,
                 nodes_only = T,
                 timeout = 500) %>% 
  add_osm_feature(key = 'shelter_type', value = 'lean_to')

#call query, can take a while 
shelter.query <- osmdata_sf(osm.query)

#extract points
shelter.raw <- shelter.query$osm_points

# only points inside the nordic countries
shelter.nordics.filt <- st_intersection(shelter.raw, nordics)


# Plot map ----------------------------------------------------------------

shelter.nordics.filt %>% ggplot()+
  geom_sf(size = 0.4, shape = 1, color = "grey90") + 
  theme_void()+
  theme(panel.background = element_rect(fill = "grey10"),
        axis.title = element_text(size = 14)) +

   annotate("text",
           x = 27,
           y = 55, 
           label = "Camping shelters in the Nordics \n© OpenStreetMap contributors",
           size = 3,
           color = "grey90") +
  labs(title = "", x = "", y = "") 


ggsave('points.png', width = 12, height = 17, units = 'cm')

