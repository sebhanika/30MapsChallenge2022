
## ---------------------------
##
## Script name: 30DayMapChallenge 21 Kontur Map
##
## Topic: Making a map with the Kontur Population: Global Population Density dataset 
##
## Author: Sebastian Hanika
##
## Date Created: 2022-11-09
##
## ---------------------------
##
## Notes: This scripts downloads the Kontur dataset but crucially does not
##        load the entire dataset into R. The user can specifiy one or multiple
##        countries to load and then only the corresponding data is loaded making
##        it much easier to handle for R.   
##
## ---------------------------

## set working directory for to current folder

setwd(dirname(rstudioapi::getActiveDocumentContext()))


# Libraries and setup -----------------------------------------------------

list.of.packages <- c("tidyverse", "rgdal", sf, downloader)
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


library(tidyverse)
library(rgdal)
library(sf)
library(downloader)
library(R.utils)
library(osmdata)

# download kontur data ----------------------------------------------------

# this script downloads the entire dataset

url.kontur.data <- "https://geodata-eu-central-1-kontur-public.s3.amazonaws.com/kontur_datasets/kontur_population_20220630.gpkg.gz"
download(url = url.kontur.data, 
         dest="kontur_data.gz",
         mode="wb") # downloads zip folder into current directory

#unziping the dataset
R.utils::gunzip("kontur_data.gz", destname = "kontur_data.gpkg")

<<<<<<< HEAD

=======
>>>>>>> ef48f98b2316f76749a4a69dd7e3b6afc601d297
# OSM boundries -----------------------------------------------------------

# Specify countries here
place.names <- c("Nauru", "Andorra", "Grenada", "Barbados", "Liechtenstein")

# initate lists (too cluncky, might be shortend)
pol.borders <- list()
borders <- list()
data.pop <- list()
xplot <- list()

# loop through user specified place names
for (i in place.names){
  
  # get polygon of country
  pol.borders <- getbb(i, format_out = 'polygon', featuretype = "country")
  
  # Checking if borders are saved as a list or not, depends on complexity of country shape
  if (is.list(pol.borders)) {
    
    # converting polygons into well-known-text for filtering
    # ordering of list is important otherwise it does not work for some countries (e.g UK)
    # Reproject of CRS is also neccessary
    borders <- st_polygon(pol.borders[order(sapply(pol.borders, length),
                                            decreasing = TRUE)]) %>%
      st_sfc(crs = 4326) %>% 
      st_transform(3857) %>% 
      st_geometry() %>% 
      st_as_text()
    
    
  } else {
    # execute with simple country shapes, simlar steps as above
    borders <- st_polygon(list(pol.borders)) %>%
      st_sfc(crs = 4326) %>% 
      st_transform(3857) %>% 
      st_geometry() %>% 
      st_as_text()
    
  }
  
  # load country specific data into R
  data.pop[[i]] <- st_read(dsn = 'data/kontur_data.gpkg', layer = "population",
                           wkt_filter = borders)
  
  # Create list of plots
  xplot[[i]] <- data.pop[[i]] %>% 
    ggplot(aes(fill = population)) +
    geom_sf(lwd = NA) +
    scale_fill_gradient(name = 'Population', low="#efedf5", high="#756bb1", 
                        guide = guide_colourbar(direction = "horizontal", 
                                                barwidth = 10)) +
    theme_bw() +
    theme(axis.text = element_blank(),
          panel.background = element_rect(fill = "white"),
          legend.position = "bottom",
          panel.grid = element_blank(),
          axis.ticks = element_blank()) 
  
  # End loop
  
}


# save all plots in list as pngs
invisible(
  lapply(
    seq_along(xplot), 
    function(x) ggsave(filename=paste0("plot_", names(xplot[x]), ".png"), 
                       plot=xplot[[x]],
                       bg = "white",
                       width = 22,
                       height = 22,
                       unit = "cm")
    )
  )





# Trying things -----------------------------------------------------------




data.try <- list()
i = "Egypt"
pol.borders <- getbb(i, format_out = 'polygon', featuretype = "country")


borders_try <- st_polygon(pol.borders[order(sapply(pol.borders, length), decreasing = T)]) %>%
  st_sfc(crs = 4326) %>% 
  st_transform(3857) %>% 
  st_geometry() %>% 
  st_as_text()

data.try <- st_read(dsn = 'data/kontur_data.gpkg', layer = "population",
                         wkt_filter = borders_try)




x <- ggplot(data = data.try,
       aes(fill = population)) +
  geom_sf(lwd = NA) 




ggsave(filename="try1.png", 
       plot=x,
       bg = "grey20",
       width = 22,
       height = 22,
       unit = "cm")


