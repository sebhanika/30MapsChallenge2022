
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
place.names <- c("Nauru", "France")


place.names <- c("Vatican City", "Nauru", "Malta", "Grenada", "Barbados", "Andorra", "Liechtenstein", "San Marino", "Singapore")
pol.borders <- list()
borders <- list()
data.try <- list()
xplot <- list()

# loop through place names, takes a long time, should change it to be faster
for (i in place.names){
  
  pol.borders <- getbb(i, format_out = 'polygon', featuretype = "country")
  
  if (is.list(pol.borders)) {
    
    borders <- st_polygon(pol.borders) %>%
      st_sfc(crs = 4326) %>% 
      st_transform(3857) %>% 
      st_geometry() %>% 
      st_as_text()
    
  } else {
    borders <- st_polygon(list(pol.borders)) %>%
      st_sfc(crs = 4326) %>% 
      st_transform(3857) %>% 
      st_geometry() %>% 
      st_as_text()
    
  }
  
  
  data.try[[i]] <- st_read(dsn = 'data/kontur_data.gpkg', layer = "population",
                           wkt_filter = borders)
  
  
  xplot[[i]] <- data.try[[i]] %>% 
    ggplot(aes(fill = population)) +
    geom_sf(lwd = NA) +
    scale_fill_gradient(name = 'Population', low="#efedf5", high="#756bb1", 
                        guide = guide_colourbar(direction = "horizontal", 
                                                barwidth = 10)) +
    theme_bw() +
    theme(axis.text = element_blank(),
          panel.background = element_rect(fill = "grey20"),
          legend.position = "bottom",
          panel.grid = element_blank(),
          axis.ticks = element_blank()) 

  
}


# save all plots as pngs
invisible(
  lapply(
    seq_along(xplot), 
    function(x) ggsave(filename=paste0("plot_", names(xplot[x]), ".png"), 
                       plot=xplot[[x]],
                       bg = "grey20",
                       width = 22,
                       height = 22,
                       unit = "cm")
    )
  )





# Trying things -----------------------------------------------------------





data.try <- list()
i = "Greece"
pol.borders <- getbb(i, format_out = 'polygon', featuretype = "country")
borders_try <- st_polygon((pol.borders)) %>%
  st_sfc(crs = 4326) %>% 
  st_transform(3857) %>% 
  st_geometry() %>% 
  st_as_text()

data.try <- st_read(dsn = 'data/kontur_data.gpkg', layer = "population",
                         wkt_filter = borders_try)






ggplot(data = data.try,
       aes(fill = population)) +
  geom_sf(lwd = NA) 


