library(shiny)
library(tidyverse)
library(leaflet)
library(fontawesome)

mm_df <- read.csv("Global Missing Migrants Dataset 2.csv") %>% 
  separate(Coordinates, into = c("lat", "lon"), sep=",") %>% 
  mutate(lat = as.numeric(lat),
         lon = as.numeric(lon))

icons <- makeAwesomeIcon(
  icon = 'person-circle-question',
  iconColor = 'black',
  library = 'fa'
)

worldmap <- leaflet(mm_df) %>% 
  addTiles() %>%
  addAwesomeMarkers(~lon, ~lat, icon = icons, 
  clusterOptions = markerClusterOptions(iconCreateFunction=JS("function (cluster) {    
    var childCount = cluster.getChildCount();  
    if (childCount < 100) {  
      c = 'rgba(255, 215, 0, 0.8);'
    } else if (childCount < 500){
      c = 'rgba(255, 69, 0, 0.8);'
    }
    else if (childCount < 1000) {  
      c = 'rgba(255, 0, 0, 0.8);'  
    } else { 
      c = 'rgba(203, 0, 199, 0.8);'  
    }    
    return new L.DivIcon({ html: '<div style=\"background-color:'+c+'\"><span>' + childCount + '</span></div>', className: 'marker-cluster', iconSize: new L.Point(40, 40) });
    }")))

worldmap  
