#Packages
library(tidyverse)
library(tidytuesdayR)
library(rnaturalearth)
#data
tuesdata <- tidytuesdayR::tt_load(2023, week = 10)

numbats <- tuesdata$numbats

#Shape file
australia_map <- ne_countries(country = "australia", returnclass = "sf")

#Map
g1 <-  ggplot() +
  geom_sf(data = australia_map,fill="#00843D") +
geom_point(data=numbats,aes(y= decimalLatitude,x=decimalLongitude),color="#FFCD00")+
  xlab("Longitude")+
  ylab("Latitude")+
  theme(axis.line = element_blank(),
    panel.grid = element_blank(),    
    panel.background = element_rect(fill = '#006994'))+
  ggtitle("Numbat sightings in Australia")
g1


