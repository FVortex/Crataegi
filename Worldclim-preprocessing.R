# Crop worldclim data by Crimea osmland

library(dplyr)
library(sf)
library(stars)
library(mapview)
library(ggplot2)
library(viridis)
library(ggthemes)

# ================
# 1. Preprocessing
# ================

# Load Crimea land polygon data
load("data/Crimea-osmland.Rdata")

# List tif files from the folder
worldclimfiles <- list.files(path = "data/wc2/", pattern = ".tif")

# read as stars-proxy objects
# @proxy: allows to work on subsets of data without loading into memory all the data
worldclim <- read_stars(paste0("data/wc2/", worldclimfiles), quiet = T, proxy = T)

# Crop worldclim data by 
worldclim[crimea_land] -> worldclim_crimea
rm(worldclim) # delete original object

# Convert to stars object
worldclim_crimea %>% st_as_stars() -> worldclim_crimea

# Save the file into
save(worldclim_crimea, file = "data/worldclim_crimea.Rdata")

c(worldclim_crimea, along = list(vars = bioclim_vars))

st_redimension(worldclim_crimea, new_dims = st_dimensions(worldclim_crimea), 
               along = list(vars = bioclim_vars))

ggplot()+
  # geom_sf(data = crimea_land)+
  geom_stars(data = st_redimension(worldclim_crimea, new_dims = st_dimensions(worldclim_crimea), 
                                   along = list(vars = bioclim_vars)),
             alpha = 0.8) + 
  facet_wrap("vars") +
  scale_fill_viridis(na.value = "white") +
  # coord_cartesian()+
  theme_map()+
  theme(legend.position = "bottom")

worldclim_crimea
# ================
# 2. Visualization
# ================

ggplot()+
  geom_sf(data = crimea_land)+
  geom_stars(data = worldclim_crimea[3], alpha = 0.8) + 
  # facet_wrap("time") +
  scale_fill_viridis(na.value = "white") +
  # coord_cartesian()+
  theme_map()+
  theme(legend.position = "bottom")


