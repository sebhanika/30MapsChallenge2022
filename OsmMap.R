## ---------------------------
##
## Script name: OSM Data Map
##
## Topic: Map using only OSM data
##
## Author: Sebastian Hanika
##
## Date Created: 2022-10-26
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

## set working directory for to current folder

setwd(dirname(rstudioapi::getActiveDocumentContext()))


library(tidyverse)
library(sf)
library(geojsonsf)
library(osmdata)
library(ggpspatial)
library(downloader)
library(leaflet)



# Query OSM data ----------------------------------------------------------


cityname <- 'Copenhagen'


boundaries <- opq(bbox = cityname) %>%
  add_osm_feature(key = 'admin_level', value = '7') %>% 
  osmdata_sf %>% unique_osmdata


municipalities <- boundaries$osm_multipolygons %>% 
  filter(grepl("Copenhagen|Frederiksberg ", name.en))



ggplot()+
  geom_sf(data = municipalities)




# Query for bike infrastrucutre points
q.bike.infra <- opq(bbox = cityname,
                    nodes_only = T,
                    timeout = 500) %>% 
  add_osm_feature(key = 'amenity', value = c('bicycle_parking', 'bicycle_rental')) %>% 
  osmdata_sf()

#extract points
bike.infra.raw <- q.bike.infra$osm_points




# Query for cyclways, liness
q.bike.lanes <- opq(bbox = cityname,
                    timeout = 500) %>% 
  add_osm_feature(key = 'cycleway', value = c('lane', 'track', 'share_busway')) %>% 
  osmdata_sf()

#extract lines
bike.lanes.raw <- q.bike.lanes$osm_lines





# Query for cyclways, liness
q.bike.lanes.hw <- opq(bbox = cityname,
                    timeout = 500) %>% 
  add_osm_feature(key = 'highway', value = c('cycleway')) %>% 
  osmdata_sf()

#extract lines
bike.lanes.raw.hw <- q.bike.lanes.hw$osm_lines



ggplot()+
  geom_sf(data = bike.infra.raw,
          aes(color = amenity), size = 0.4, shape = 2) +
  geom_sf(data = bike.lanes.raw) +
  geom_sf(data = bike.lanes.raw.hw, color = "green")




leaflet() %>%
  addProviderTiles(providers$OpenStreetMap) %>% 
  addCircles(data = bike.infra.raw,
             color = "#14232A", radius = 0.01) %>% 
  addPolylines(data = bike.lanes.raw.hw, color = "red") %>% 
  addPolylines(data = bike.lanes.raw, color = "orange")
  
