
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
wkt <- list()
data.try <- list()


for (i in place.names){
  
  pol.borders <- getbb(i, format_out = 'polygon', featuretype = "country")
  
  borders[i] <- st_polygon(list(pol.borders)) %>%
    st_sfc(crs = 4326) %>% 
    st_transform(3857) %>% 
    st_geometry() %>% 
    st_as_text()
  

 data.try[[i]] <- as.data.frame(st_read(dsn = 'data/kontur_data.gpkg', layer = "population",
                        wkt_filter = borders[[i]]))
  
  
}


data.comb <- bind_rows(data.try, .id = "country")


pop_range <- range(data.comb$population)







maps_shared <- map(.x = place.names, 
                   .f = function(x) data.comb %>% 
                     filter(country == x) %>% 
                     ggplot(aes(fill = population, geometry = "geom")) +
                     geom_sf(lwd = NA) +
                     theme_bw() +
                     theme(axis.text = element_blank(),
                           axis.ticks = element_blank()))




## use COWplot to combine and add single legend
plot_grid(plotlist = c(map(.x = maps_shared,
                           .f = function(x) x + theme(legend.position = 'none'))),
          labels = LETTERS[1:3], label_size = 10, nrow = 2)















#shared legend, not yet working

maps_shared <- map(.x = place.names, 
                   .f = function(x) data.comb %>% 
                     filter(country == x) %>% 
                     ggplot(aes(fill = population, geometry = "geom")) +
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
          labels = LETTERS[1:3], label_size = 10, nrow = 2)






rm(maps_shared)



























# getting berlin mask

place.name1 <- "berlin, Germany"

pol.borders <- getbb(place.name1, format_out = 'polygon')

#stop here to get the exact polygons
borders.ber <- st_polygon(pol.borders[1]) %>%
                st_sfc(crs = 4326) %>% 
                st_transform(3857)

  


wkt.ber = st_as_text(st_geometry(borders.ber))



# getting Los Angeles mask

place.name2 <- "Madrid"

pol.la <- getbb("Madrid", format_out = 'polygon', featuretype = "city")

#stop here to get the exact polygons
borders.la <- st_polygon(pol.la[1]) %>%
  st_sfc(crs = 4326) %>% 
  st_transform(3857)




# here you get the squared bounding box
bbox.la <- st_as_sfc(sf::st_bbox(borders.la))

wkt.la = st_as_text(st_geometry(bbox.la))





# getting Los Angeles mask

place.name3 <- "Liechtenstein"

pol.bur <- getbb(place.name3, format_out = 'polygon', featuretype = "country")

#stop here to get the exact polygons
borders.bur <- st_polygon(list(pol.bur)) %>%
  st_sfc(crs = 4326) %>% 
  st_transform(3857)




# here you get the squared bounding box
bbox.bur <- st_as_sfc(sf::st_bbox(borders.bur))

wkt.bur = st_as_text(st_geometry(borders.bur))




data.bur <- st_read(dsn = 'data/kontur_data.gpkg', layer = "population",
                   wkt_filter = wkt.bur)



ggplot(data = data.bur) +
  geom_sf(aes(fill = population), lwd = NA) +
  theme_void()












# load data ---------------------------------------------------------------


# 
# data <- st_read(dsn = 'kontur_data.gpkg', layer = "population",
#                 query='SELECT * FROM population WHERE FID ="1"')


data.ber <- st_read(dsn = 'data/kontur_data.gpkg', layer = "population",
                wkt_filter = wkt.ber)


data.la <- st_read(dsn = 'data/kontur_data.gpkg', layer = "population",
                    wkt_filter = wkt.la)



data.ber.new <- data.ber %>% 
  mutate(city = "Berlin")

data.comb <- data.la %>% 
  mutate(city = "Los Angeles") %>% 
  rbind(data.ber.new)


pop_range <- range(data.comb$population)


cities <- c("Berlin", "Los Angeles")


maps_shared <- map(.x = cities, 
                   .f = function(x) data.comb %>% 
                     filter(city == x) %>% 
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
          labels = LETTERS[1:1], label_size = 10, nrow = 2)







  



# example -----------------------------------------------------------------



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