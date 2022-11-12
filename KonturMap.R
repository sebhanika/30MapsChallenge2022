
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

place.names <- c("Andorra", "Liechtenstein", "San Marino")
pol.borders <- list()
borders <- list()
data.try <- list()

# loop through place names, takes a long time, should change it to be faster
for (i in place.names){
  
  pol.borders <- getbb(i, format_out = 'polygon', featuretype = "country")
  
  borders[i] <- st_polygon(list(pol.borders)) %>%
    st_sfc(crs = 4326) %>% 
    st_transform(3857) %>% 
    st_geometry() %>% 
    st_as_text()
  
  data.try[[i]] <- st_read(dsn = 'data/kontur_data.gpkg', layer = "population",
                           wkt_filter = borders[[i]])
}

#combine sf_dataframes together for plotting
data.comb <- sf::st_as_sf(data.table::rbindlist(data.try, idcol = "country"))

# max range for common legend
pop_range <- range(data.comb$population)


maps_shared <- map(.x = place.names, 
                   .f = function(x) data.comb %>% 
                     filter(country == x) %>% 
                     ggplot(aes(fill = population)) +
                     geom_sf(lwd = NA) +
                     scale_fill_continuous(limits = pop_range,
                                           name = 'Pop') +
                     theme_bw() +
                     theme(axis.text = element_blank(),
                           axis.ticks = element_blank()))



## use COWplot to combine and add single legend
plot_grid(plotlist = c(map(.x = maps_shared,
                           .f = function(x) x + theme(legend.position = 'none')),
                       list(get_legend(maps_shared[[1]]))),
          labels = LETTERS[1:3])







# benfords law ------------------------------------------------------------



library(benford.analysis)

hist(data.comb$population)


# perform benford analysis
trends.ber = benford(data.ber$population, number.of.digits = 1, discrete = T, sign = "positive") 


# plot results
plot(trends.ber)


# perform benford analysis
trends.mad = benford(data.la$population, number.of.digits = 1, discrete = T, sign = "positive") 


# plot results
plot(trends.mad)



leaflet(pol.la$multipolygon) %>% 
  addPolygons() %>% 
  addProviderTiles(provider = "CartoDB")




leaflet(pol.la) %>% 
  addPolygons() %>% 
  addProviderTiles(provider = "CartoDB")




# here you get the squared bounding box
#bbox.ber <- st_as_sfc(sf::st_bbox(borders.ber))