library(tidyverse)
library(leaflet)
library(htmltools)
# Read the data
mm_df <- read.csv("IOM_Missing_Migrants/Missing_Migrants_2025.csv") %>% 
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
  
  # Clean whitespace in country names
  mutate(country = trimws(country)) %>%
  
  group_by(country) %>% 
  summarise(count = n()) %>% 
  ungroup()


acled_df <- read.csv("IOM_Missing_Migrants/acled.csv") %>%
  rename(lat = latitude, lon = longitude) %>%
  drop_na(lat, lon, country) %>%
  # drop columns exept for lat, lon, event_type, country, year, event_id
  select(lat, lon, event_type, country, year, event_id_cnty) %>% 
  group_by(country) %>% 
  summarise(count = n()) %>% 
  ungroup()

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


