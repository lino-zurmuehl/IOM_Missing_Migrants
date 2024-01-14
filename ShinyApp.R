library(shiny)
library(tidyverse)
library(leaflet)
library(htmltools)

# Read the data
mm_df <- read.csv("Global Missing Migrants Dataset 2.csv") %>% 
  separate(Coordinates, into = c("lat", "lon"), sep=",") %>% 
  mutate(lat = as.numeric(lat),
         lon = as.numeric(lon))

# Define icons and labels
icons <- makeIcon(iconUrl = "person-circle-question-solid.png",
                  iconWidth = 18, iconHeight = 18)

labels <- sprintf(
  "<strong>Number of Deaths: %s</strong><br/>Cause of Death: %s<br/>Migration Route: %s",
  mm_df$Number.of.Dead, mm_df$Cause.of.Death, mm_df$Migration.route) %>%
  lapply(htmltools::HTML)

# Define the UI
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("category", "Select Geographical Grouping", choices = unique(mm_df$UNSD.Geographical.Grouping)),
      selectInput("cause", "Select Cause of Death", choices = unique(mm_df$Cause.of.Death))
    ),
    mainPanel(leafletOutput("worldmap"))
  )
)

# Define the server
server <- function(input, output, session) {
  output$worldmap <- renderLeaflet({
    leaflet(mm_df) %>% 
      addTiles() %>%
      addMarkers(
        ~lon, ~lat, 
        icon = icons, 
        label = labels,
        clusterOptions = markerClusterOptions(maxClusterSize = 10,
                                              iconCreateFunction = JS("
                                                function (cluster) {    
                                                  var childCount = cluster.getChildCount();
                                                  var size = 20;

                                                  var c = 'rgba(255, 215, 0, 0.8)'; 
                                                  
                                                  if (childCount < 50) {  
                                                    c = 'rgba(255, 165, 0, 0.7)';
                                                    size = 10;
                                                  } else if (childCount < 100){
                                                    c = 'rgba(255, 140, 0, 0.7)';
                                                    size = 20;
                                                  } else if (childCount < 500) {  
                                                    c = 'rgba(255, 69, 0, 0.7)';
                                                    size = 30;
                                                  } else if (childCount < 1000) {  
                                                    c = 'rgba(255, 0, 0, 0.7)';
                                                    size = 30;
                                                  } else { 
                                                    c = 'rgba(139, 0, 0, 0.7)';
                                                    size = 40;
                                                  }    
                                                  
                                                  return new L.DivIcon({ 
                                                    html: '<div style=\"background-color:' + c + '\"><span>' + childCount + '</span></div>', 
                                                    className: 'marker-cluster', 
                                                    iconSize: new L.Point(size, size) 
                                                  });
                                                }
                                              ")))
  })
}

# Run the app
shinyApp(ui, server)
