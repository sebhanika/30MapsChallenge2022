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
library(RColorBrewer)

`%!in%` = Negate(`%in%`) # function needed for later 



# Query OSM data ----------------------------------------------------------

## in case correct admin boundries are needed
# cityname <- 'Copenhagen'
# 
# boundaries <- opq(bbox = cityname) %>%
#   add_osm_feature(key = 'admin_level', value = '7') %>% 
#   osmdata_sf %>% unique_osmdata
# 
# municipalities <- boundaries$osm_multipolygons %>% 
#   filter(grepl("Copenhagen|Frederiksberg ", name.en))
# 
# ggplot()+
#   geom_sf(data = municipalities)



cityname <- "Malmö"

# Query for bike infrastrucutre points
q.bike.infra <- opq(bbox = cityname,
                    nodes_only = T,
                    timeout = 500) %>% 
  add_osm_feature(key = 'amenity', value = c('bicycle_parking', 'bicycle_rental')) %>% 
  osmdata_sf()

#extract points
bike.infra.raw <- q.bike.infra$osm_points


# Query for cycleways, lines
q.bike.lanes <- opq(bbox = cityname,
                    timeout = 500) %>% 
  add_osm_feature(key = 'cycleway') %>% 
  osmdata_sf()

#extract lines
bike.lanes.raw <- q.bike.lanes$osm_lines %>% 
  select(cycleway, highway, name, geometry)


# Query for cycleways, lines
q.bike.lanes.hw <- opq(bbox = cityname,
                    timeout = 500) %>% 
  add_osm_feature(key = 'highway', value = c('cycleway')) %>% 
  osmdata_sf()

#extract lines
bike.lanes.raw.hw <- q.bike.lanes.hw$osm_lines %>% 
  select(cycleway, highway, name, geometry)




bike.lanes <- rbind(bike.lanes.raw, bike.lanes.raw.hw)

bike.lanes <- bike.lanes %>% 
  mutate(type = ifelse(is.na(cycleway), highway, cycleway)) %>% 
  select(-c(cycleway, highway)) %>% 
  filter(type %!in% c('no', 'none', 'designated')) %>% 
  mutate(type = ifelse(type %!in% c('cycleway', 'track', 'lane'), "other", type))







# Trying out stuff --------------------------------------------------------









# Query for cycleways, lines
try.total <- opq(bbox = cityname,
                       timeout = 500) %>% 
  add_osm_feature(key = 'highway') %>% 
  osmdata_sf()

try.total.lines <- try.total$osm_lines %>% 
  select(name, highway, bicycle, geometry) %>% 
  filter(bicycle %in% c('yes', 'designated'))


ggplot()+
  #geom_sf(data = bike.infra.raw,
  #       aes(shape = amenity), size = 0.4) +
  
  geom_sf(data = bike.lanes, color = "grey50") 
  #geom_sf(data = try.total.lines, color = "red") 
  

colnames(try.total.lines)



ggplot()+
  #geom_sf(data = bike.infra.raw,
   #       aes(shape = amenity), size = 0.4) +
  geom_sf(data = bike.lanes,
          aes(color = type))




types <- sort(unique(bike.lanes$type))
#custom_pal <- c("#FBE697" , "#C9DACA", 
 #               "#14232A", "#8FC0CE")


pal <- colorFactor("Set3", domain = types)




leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  #addCircles(data = bike.infra.raw,
   #          color = "#14232A", radius = 0.01) %>% 
  addPolylines(data = bike.lanes, 
               color = ~pal(type),
               popup = ~type
               ) %>% 
  addLegend(data = bike.lanes, "bottomright", pal = pal, values = ~type,
            title = "Types of bike lanes",
            opacity = 1)







