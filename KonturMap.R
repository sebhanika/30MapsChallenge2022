
#KonturPop

## ---------------------------
##
## Script name: 
##
## Topic:
##
## Author: Sebastian Hanika
##
## Date Created: 2022-11-09
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

## set working directory for to current folder

setwd(dirname(rstudioapi::getActiveDocumentContext()))



# Libraries and setup -----------------------------------------------------

library(tidyverse)
library(rgdal)
library(sf)
library(downloader)
library(R.utils)
library(osmdata)


# get data from eurostat
url.kontur.data <- "https://geodata-eu-central-1-kontur-public.s3.amazonaws.com/kontur_datasets/kontur_population_20220630.gpkg.gz"
download(url = url.kontur.data, 
         dest="kontur_data.gz",
         mode="wb") # downloads zip folder into current directory

R.utils::gunzip("kontur_data.gz", destname = "kontur_data.gpkg")



# OSM boundries -----------------------------------------------------------


boundaries <- opq(bbox = "Brandenburg") %>%
     add_osm_feature(key = 'admin_level', value = '4') %>% 
     osmdata_sf %>% unique_osmdata


Brb.bb <- boundaries$osm_multipolygons %>% 
  filter(name %in% c("Brandenburg", "Berlin"))


brb.bb.merged <- Brb.bb %>% 
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup()





wkt2 = st_as_text(st_geometry(st_transform(brb.bb.merged, 3857)))




# load data ---------------------------------------------------------------


# 
# data <- st_read(dsn = 'kontur_data.gpkg', layer = "population",
#                 query='SELECT * FROM population WHERE FID ="1"')


data <- st_read(dsn = 'kontur_data.gpkg', layer = "population",
                wkt_filter = wkt2)




ggplot()+
  geom_sf(data = data, aes(fill = population)) +
  scale_fill_viridis() + theme_bw()



# example -----------------------------------------------------------------


nc = st_read(system.file("shape/nc.shp", package="sf"))
#> Reading layer `nc' from data source 
#>   `/tmp/RtmpH4YqwH/temp_libpath649bcfb769e/sf/shape/nc.shp' using driver `ESRI Shapefile'
#> Simple feature collection with 100 features and 14 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -84.32385 ymin: 33.88199 xmax: -75.45698 ymax: 36.58965
#> Geodetic CRS:  NAD27
summary(nc) # note that AREA was computed using Euclidian area on lon/lat degrees



# spatial filter, as wkt:
wkt = st_as_text(st_geometry(nc[1,]))
# filter by (bbox overlaps of) first feature geometry:
st_read(system.file("gpkg/nc.gpkg", package="sf"), wkt_filter = wkt)



