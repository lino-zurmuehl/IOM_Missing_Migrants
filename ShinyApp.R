library(shiny)
library(tidyverse)
library(leaflet)
library(fontawesome)
library(htmlwidgets)

mm_df <- read.csv("Global Missing Migrants Dataset 2.csv") %>% 
  separate(Coordinates, into = c("lat", "lon"), sep=",") %>% 
  mutate(lat = as.numeric(lat),
         lon = as.numeric(lon))

icons <- makeIcon(iconUrl = "person-circle-question-solid.png",
  iconWidth = 18, iconHeight = 18)


worldmap <- leaflet(mm_df) %>% 
  addTiles() %>%
  addMarkers(
    ~lon, ~lat, 
    icon = icons, 
    clusterOptions = markerClusterOptions(maxClusterSize = 10,
      iconCreateFunction = JS("
        function (cluster) {    
          var childCount = cluster.getChildCount();
          var size = 40;
          var c = 'rgba(255, 215, 0, 0.8)'; 
          
          if (childCount < 100) {  
            c = 'rgba(255, 215, 0, 0.8)';
            size = 10;
          } else if (childCount < 500){
            c = 'rgba(255, 69, 0, 0.8)';
            size = 20;
          } else if (childCount < 1000) {  
            c = 'rgba(255, 0, 0, 0.8)';
            size = 30;
          } else { 
            c = 'rgba(203, 0, 199, 0.8)';
            size = 40;
          }    
          
          return new L.DivIcon({ 
            html: '<div style=\"background-color:' + c + '\"><span>' + childCount + '</span></div>', 
            className: 'marker-cluster', 
            iconSize: new L.Point(size, size) 
          });
        }
      ")))


worldmap  


