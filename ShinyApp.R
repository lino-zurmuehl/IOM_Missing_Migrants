library(shiny)
library(tidyverse)
library(leaflet)
library(htmltools)

# Read the data
mm_df <- read.csv("Global Missing Migrants Dataset 2.csv") %>% 
  separate(Coordinates, into = c("lat", "lon"), sep=",") %>% 
  mutate(lat = as.numeric(lat),
         lon = as.numeric(lon)) %>% 
  drop_na(lat, lon) %>%
  filter(Incident.Type == "Incident") %>% 
  mutate(Migration.route = case_when(
    Migration.route == "" ~ "Uncategorized",
    Migration.route == "Western Africa / Atlantic route to the Canary Islands" 
                        ~ "Atlantic route to the Canary Islands",
    TRUE ~ Migration.route))

# Define icons and labels
icons <- makeIcon(iconUrl = "person-circle-question-solid.png",
                  iconWidth = 18, iconHeight = 18)



# Define the UI
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("category", "Select Geographical Grouping", choices = c("None", unique(mm_df$UNSD.Geographical.Grouping))),
      selectInput("year", "Select Year", choices = c("None", unique(mm_df$Incident.year))),
      selectInput("month", "Select Month of Year", choices = c("None", unique(mm_df$Reported.Month))),
      plotOutput("incidents_plot")),
    mainPanel(leafletOutput("worldmap"))
  )
)

# Define the server
server <- function(input, output, session) {
  filtered_data <- reactive({
    filter(
      mm_df,
      if (input$category != "None") UNSD.Geographical.Grouping == input$category else TRUE,
      if (input$year != "None") Incident.year == as.numeric(input$year) else TRUE,
      if (input$month != "None") Reported.Month == input$month else TRUE
    )
  })
  
  
  output$worldmap <- renderLeaflet({
    labels <- sprintf(
      "<strong>Number of Deaths: %s</strong><br/>Cause of Death: %s<br/>Migration Route: %s",
      filtered_data()$Number.of.Dead, filtered_data()$Cause.of.Death, filtered_data()$Migration.route) %>%
      lapply(htmltools::HTML)
    
    leaflet(filtered_data()) %>% 
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
  
  output$incidents_plot <- renderPlot({
    incidents_per_mr <- filtered_data() %>%
      group_by(Migration.route) %>%
      summarise(Incidents = n())
    
    ggplot(incidents_per_mr, aes(x = Migration.route, y = Incidents, fill = Incidents)) +
      geom_bar(stat = "identity") +
      scale_fill_gradient(low = "red", high = "darkred")+
      labs(title = "Incidents per Migration Route",
           x = "Migration Routes",
           y = "Number of Incidents (Log Scale)") +
      scale_y_log10() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, ))
  })
}

# Run the app
shinyApp(ui, server)
