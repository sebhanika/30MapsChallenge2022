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


# build query
osm.query <- opq(bbox = 'copenhagen, denmark',
                 nodes_only = T,
                 timeout = 500) %>% 
  add_osm_feature(key = 'amenity', value = 'bicycle_parking')

#call query, can take a while 
bikes.query <- osmdata_sf(osm.query)

#extract points
bikes.raw <- bikes.query$osm_points




bikes.raw %>% ggplot()+
  geom_sf(size = 0.4, shape = 2, color = "grey20")
