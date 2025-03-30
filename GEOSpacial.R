library(tidyverse)
library(leaflet)
library(htmltools)
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
  rename(country = Country.of.Origin) %>% 
  group_by(country) %>% 
  summarise(count = n()) %>% 
  ungroup()

acled_df <- read.csv("acled.csv") %>%
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
routes <- st_read("migration_routes_verified.geojson", )

# Create a leaflet map



