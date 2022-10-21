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
  geom_sf(size = 0.4, shape = 2, color = "grey20") + 
  theme_void()+
  theme(panel.border = element_rect(colour = "black", 
                                     fill=NA,
                                     size=0.5),
        axis.title = element_text(size = 14)) +
  
   annotate("text",
           x = 27.5,
           y = 55, 
           label = "Source: OpenStreetMap",
           size = 2.5)+
  
  labs(title = "Camping shelters in the Nordics", x = "", y = "") +
  
  ggspatial::annotation_scale(
    location = "br") +
  ggspatial::annotation_north_arrow(
    location = "br",
    height = unit(0.75, "cm"),
    width = unit(0.75, "cm"),
    which_north = "true",
    pad_x = unit(0.1, "cm"),
    pad_y = unit(0.6, "cm"),
    style = ggspatial::north_arrow_fancy_orienteering(
      fill = c("grey40", "white"),
      line_col = "grey20")
  )


# Interactive webmap ------------------------------------------------------

shelter.nordics.filt %>% 
  leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron, 
                   group = "Basemap - greyscale") %>% #adding basemaps
  addProviderTiles(providers$CartoDB.DarkMatter, 
                   group = "Basemap - dark") %>%
  addLayersControl(
    baseGroups = c("Basemap - greyscale", "Basemap - dark"), # adding control for base maps
    options = layersControlOptions(collapsed = TRUE)) %>% 
  clearShapes() %>%
  addCircles(color = "#14232A",
             popup = ~paste(name),
             highlightOptions = highlightOptions(color = "#E2068A"))


