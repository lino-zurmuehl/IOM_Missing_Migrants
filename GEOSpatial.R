library(tidyverse)
library(leaflet)
library(htmltools)
library(magrittr)

# Read the data
mm_df <- read.csv("Missing_Migrants_2025.csv") %>% 
  # replace the whitespeace in column names to _
  setNames(gsub(" ", "_", names(.))) %>%
  separate(Coordinates, into = c("lat", "lon"), sep=",") %>% 
  mutate(lat = as.numeric(lat),
         lon = as.numeric(lon)) %>% 
  drop_na(lat, lon) %>%
  filter(Incident.Type == "Incident") %>% 
  mutate(Migration.Route = case_when(
    Migration.Route == "" ~ "Uncategorized",
    Migration.Route == "Western Africa / Atlantic route to the Canary Islands" 
    ~ "Atlantic route to the Canary Islands",
    TRUE ~ Migration.Route)) %>%
  filter(Country.of.Origin != "Unknown") %>% 
  filter(!str_detect(Region.of.Incident, "Asia")) %>% 
  rename(country = Country.of.Origin) %>% 
  # Split comma-separated countries into multiple rows
  separate_rows(country, sep = ",") %>%
  
  # Group by the original record ID
  group_by(Main.ID) %>%
  
  # Count how many countries came from the same original row
  mutate(num_countries = n()) %>%
  
  # Divide 'Total.Number.of.Dead.and.Missing' by the number of countries
  mutate(total_number_of_dead_and_missing = Total.Number.of.Dead.and.Missing / num_countries) %>%
  
  ungroup() %>%
  
  # # Clean whitespace in country names
  # mutate(country = trimws(country)) %>% 
  # { 
  #   assign("mm_df_coords", ., envir = .GlobalEnv)
  #   .
  # }
   #%>% 
  { mm_df_coords <<- . } %>%  # saves to global env
  group_by(country) %>% 
  summarise(count = n()) %>% 
  ungroup() %>%  
  rename(count_tot_n_dead_and_missing = count)


mm_df_test <- mm_df_coords %>%
  filter(!Migration.Route %in% c("Eastern Route to/from EHOA", "Route to Southern Africa"))   %>% 
  mutate(route_category = case_when(
    Migration.Route %in% c("US-Mexico border crossing", "Darien", "Caribbean to US", 
                 "Venezuela to Caribbean", "Caribbean to Central America", 'Dominican Republic to Puerto Rico', 'Haiti to Dominican Republic') ~ "US-Mexico",
    
    Migration.Route %in% c("Central Mediterranean", "Atlantic route to the Canary Islands", 
                 "Italy to France", 'Sahara Desert crossing', 'DRC to Uganda') ~ "Central Mediterranean",
    
    Migration.Route == "Western Mediterranean" ~ "Western Mediterranean",
    
    Migration.Route %in% c("Eastern Mediterranean", "Türkiye-Europe land route", 
                 "Horn of Africa Route", "Northern Route from EHOA") ~ "Eastern Mediterranean",
    
    Migration.Route %in% c("Western Balkans", "Belarus-EU border", "Ukraine to Europe", 'English Channel to the UK') ~ "Balkan",
    
    Migration.Route %in%  "Uncategorized" & 
      str_detect(Region.of.Incident, "America") ~ "US-Mexico",
    
    Region.of.Incident %in%  "Carribean" ~ "US-Mexico",
    
    TRUE ~ NA_character_
  ))



mm_df_coords
acled_df <- read.csv("acled.csv") %>%
  rename(lat = latitude, lon = longitude) %>%
  drop_na(lat, lon, country) %>%
  # drop columns exept for lat, lon, event_type, country, year, event_id
  select(lat, lon, event_type, country, year, event_id_cnty) %>% 
  { acled_df_coords <<- . } %>%
  group_by(country) %>% 
  summarise(count = n()) %>% 
  rename(count_conflict_events = count) %>%  #should specify whcih conflict events exactly 
  ungroup()   

acled_iom_df <- left_join(acled_df, mm_df, by = "country")
test_acled_iom_df <-  left_join(acled_df_coords, mm_df_coords, by = "lon")

# save list of all countries in mm_df to then filter out the countries in acled_df
countries <- mm_df %>% 
  select(country) %>% 
  distinct() %>% 
  pull()

# filter out the countries in acled_df
acled_df <- acled_df %>% 
  filter(country %in% countries)

library(giscoR)
library(tmap)
library(sf)
routes <- st_read("IOM_Missing_Migrants/migration_routes_verified.geojson", )

# Create a leaflet map

# Install these if not installed:
# install.packages("leaflet")
# install.packages("sf")

library(sf)
library(leaflet)

# 1. Read the GeoJSON file
routes <- st_read("migration_routes_verified.geojson")

# 2. If needed, transform to WGS84 (lat/lon)
# Check your layer's existing CRS; only do this if routes isn't already in EPSG:4326
routes <- st_transform(routes, 4326)

# 3. Create a leaflet map
leaflet(routes) %>%
  # A tile provider of your choice:
  addProviderTiles("CartoDB.Positron") %>%
  
  # Since these are routes, we likely need polylines:
  addPolylines()


