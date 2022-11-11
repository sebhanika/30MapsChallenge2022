
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
library(viridis)
library(rgdal)
library(leaflet)
library(sf)
library(downloader)
library(R.utils)
library(osmdata)


# download kontur data ----------------------------------------------------

#download Konutr data
url.kontur.data <- "https://geodata-eu-central-1-kontur-public.s3.amazonaws.com/kontur_datasets/kontur_population_20220630.gpkg.gz"
download(url = url.kontur.data, 
         dest="kontur_data.gz",
         mode="wb") # downloads zip folder into current directory

R.utils::gunzip("kontur_data.gz", destname = "kontur_data.gpkg")



# OSM boundries -----------------------------------------------------------


# There are two options depending on what you want to achieve. 
# Either get a squared bounding box of the place
# Or get the exact boundires, however this does not work for every place




pol.berlin <- getbb(place.name, format_out = 'polygon')

#stop here to get the exact polygons
borders.berlin <- st_polygon(pol[1]) %>%
                st_sfc(crs = 4326) %>% 
                st_transform(3857)

# here you get the squared bounding box
bbox <- st_as_sfc(sf::st_bbox(borders))




wkt2 = st_as_text(st_geometry(x))











# load data ---------------------------------------------------------------


# 
# data <- st_read(dsn = 'kontur_data.gpkg', layer = "population",
#                 query='SELECT * FROM population WHERE FID ="1"')


data <- st_read(dsn = 'data/kontur_data.gpkg', layer = "population",
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



