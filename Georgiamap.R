library(sf)
library(ggplot2)
library(tmap)
library(tmaptools)
library(leaflet)
library(dplyr)
library(readxl)
options(scipen = 999)
?options

setwd('C:/Users/alber/Downloads/Stat382/Project4') # Use your own

data <- read_excel('Cleaned_Data.xlsx')
mymap <- st_read("Georgia_Counties.shp", stringsAsFactors=F)
str(mymap)


colnames(mymap)[colnames(mymap) %in% c('NAME10')] <- 'County'
map_and_data <- inner_join(mymap,data)

#?colnames
#?inner_join

#ggplot(map_and_data)+
  #geom_sf(aes(fill= Education))

ggplot(map_and_data)+
  geom_sf(aes(fill=Education))+
  scale_fill_gradient(low= '#56B1F7',high='#132B43')
  


