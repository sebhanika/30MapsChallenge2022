
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
library(cowplot)


# download kontur data ----------------------------------------------------

#download Konutr data
url.kontur.data <- "https://geodata-eu-central-1-kontur-public.s3.amazonaws.com/kontur_datasets/kontur_population_20220630.gpkg.gz"
download(url = url.kontur.data, 
         dest="kontur_data.gz",
         mode="wb") # downloads zip folder into current directory

R.utils::gunzip("kontur_data.gz", destname = "kontur_data.gpkg")



# OSM boundries -----------------------------------------------------------



# getting mask
place.names <- c("Vatican City", "Liechtenstein")


place.names <- c("Vatican City", "Nauru", "Malta", "Grenada", "Barbados", "Andorra", "Liechtenstein", "San Marino", "Singapore")
pol.borders <- list()
borders <- list()
data.try <- list()
xplot <- list()

# loop through place names, takes a long time, should change it to be faster
for (i in place.names){
  
  pol.borders <- getbb(i, format_out = 'polygon', featuretype = "country")
  
  if (is.list(pol.borders)) {
    
    borders[[i]] <- st_polygon(pol.borders) %>%
      st_sfc(crs = 4326) %>% 
      st_transform(3857) %>% 
      st_geometry() %>% 
      st_as_text()
    
  } else {
    borders[[i]] <- st_polygon(list(pol.borders)) %>%
      st_sfc(crs = 4326) %>% 
      st_transform(3857) %>% 
      st_geometry() %>% 
      st_as_text()
    
  }
  
  
  data.try[[i]] <- st_read(dsn = 'data/kontur_data.gpkg', layer = "population",
                           wkt_filter = borders[[i]])
  
  
  xplot[[i]] <- data.try[[i]] %>% 
    ggplot(aes(fill = population)) +
    geom_sf(lwd = NA) +
    scale_fill_gradient(name = 'Pop', low="#9c9ce3", high="#b3fe74") +
    theme_void() +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank())

  
}



invisible(
  lapply(
    seq_along(xplot), 
    function(x) ggsave(filename=paste0("myplot", x, ".png"), plot=xplot[[x]], bg = "white")
  ) )


?seq_along





#combine sf_dataframes together for plotting
data.comb <- sf::st_as_sf(data.table::rbindlist(data.try, idcol = "country"))










#shared map 
maps_shared <- map(.x = place.names, 
                   .f = function(x) data.comb %>% 
                     filter(country == x) %>% 
                     ggplot(aes(fill = population)) +
                     geom_sf(lwd = NA) +
                     scale_fill_gradient(name = 'Pop', low="#9c9ce3", high="#b3fe74") +
                     theme_void() +
                     theme(axis.text = element_blank(),
                           axis.ticks = element_blank()))



## use COWplot to combine and add single legend
plot_grid(plotlist = c(map(.x = maps_shared,
                           .f = function(x) x )))

















# benfords law ------------------------------------------------------------



leaflet(pol.la$multipolygon) %>% 
  addPolygons() %>% 
  addProviderTiles(provider = "CartoDB")




leaflet(pol.la) %>% 
  addPolygons() %>% 
  addProviderTiles(provider = "CartoDB")




# here you get the squared bounding box
#bbox.ber <- st_as_sfc(sf::st_bbox(borders.ber))