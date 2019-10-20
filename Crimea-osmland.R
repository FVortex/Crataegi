# Script for processing Crimea coastline from OpenStreetMap
# Date: 20 October 2019

library(sf)
library(dplyr)
library(osmdata)
library(mapview)
library(ggplot2)

# ================
# 1. Preprocessing
# ================

# 1.1. OSM data on admin borders

# Download osm data on administrative border of Crimea
crimea_admin_level_4 <- opq(bbox = "Республика Крым") %>% 
  add_osm_feature(key = "admin_level", value = 4) %>% 
  osmdata_sf()

# # In case of 451 error change server
# set_overpass_url("https://overpass.kumi.systems/api/interpreter")

# Filter the region, remove attributes we do not need
crimea_admin_level_4$osm_multipolygons %>% 
  filter(name %in% c("Республика Крым", "Севастополь")) %>% 
  select(name) %>% as('Spatial') %>% st_as_sf() %>% st_union() -> crimea_admin_level_4

# Check
crimea_admin_level_4 %>% mapview()

# 1.2. Land

# Since OSM does not have tag "land", we use global dataset, based on osm data:
# https://osmdata.openstreetmap.de/data/land-polygons.html

# read global data and union splited polygons
osm_land <- read_sf("data/osm-land/osm-land.shp")
osm_land %>% st_union() -> osm_land

# Let's take a look
osm_land %>% mapview() + crimea_admin_level_4

# crop land 
crimea_land <- st_intersection(osm_land, crimea_admin_level_4)

# Save as Rdata
save(crimea_land, file = "data/Crimea-osmland.Rdata")
