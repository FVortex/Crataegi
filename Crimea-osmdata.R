# Script for processing Crimea admin borders, built-up area and land polygon from OpenStreetMap
# Date: 20 October 2019

library(sf)
library(dplyr)
library(osmdata)
library(mapview)
library(ggplot2)

# ====================
# 1. OSM admin borders
# ====================

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

# Save to Rdata file
save(crimea_admin_level_4, file = "data/crimea_adm_border.Rdata")

# ====================
# 2. OSM built up area
# ====================

# Residential
crimea_residential <- opq(bbox = "Республика Крым") %>% 
  add_osm_feature(key = "landuse", value = "residential") %>% 
  osmdata_sf()

crimea_residential$osm_polygons %>% select(osm_id, name) %>% 
  filter(!osm_id %in% c(146735682, 165881819)) %>%
  as('Spatial') %>% st_as_sf() %>% .[crimea_admin_level_4,] -> crimea_built_up_area

crimea_residential$osm_multipolygons %>% select(osm_id, name) %>% 
  filter(!osm_id %in% c(146735682, 165881819)) %>%
  as('Spatial') %>% st_as_sf() %>% .[crimea_admin_level_4,] %>% rbind(crimea_built_up_area) -> crimea_built_up_area

# Other values
landuse_values <- c("industrial", "commercial", "retail", "construction",
                    "brownfield", "garages")

for (i in landuse_values) {
  print(i)
  temp <- opq(bbox = "Республика Крым") %>% 
    add_osm_feature(key = "landuse", value = i) %>% 
    osmdata_sf()
  temp$osm_polygons %>% select(osm_id, name) %>% 
    as('Spatial') %>% st_as_sf() %>% .[crimea_admin_level_4,] %>% rbind(crimea_built_up_area) -> 
    crimea_built_up_area
  temp$osm_multipolygons %>% select(osm_id, name) %>% 
    as('Spatial') %>% st_as_sf() %>% .[crimea_admin_level_4,] %>% rbind(crimea_built_up_area) -> 
    crimea_built_up_area
}

# Save data to Rdata file
save(crimea_built_up_area, file = "data/crimea_built_up_area.Rdata")

# Visualize and 
crimea_built_up_plot <- ggplot()+
  geom_sf(data = crimea_land, fill = "white")+
  geom_sf(data = crimea_built_up_area, fill = "black", col = "black", lwd = 0.2)+
  coord_sf()+
  theme(legend.position = "bottom")

ggsave(crimea_built_up_plot, filename = "crimea_built_up_area.jpeg",
       path = "plots/",
       dpi = 300, width = 18, height = 10, units = "cm")

# ===========
# 3. OSM land
# ===========

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
