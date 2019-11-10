# Terrain covariates for SDM

# Original data sourse: ASTERGDEM3

# The data was preprocessed. To investigate the effects of relief at different scales we use 30`` and 2.5` resolution

library(dplyr)
library(sf)
library(stars)
library(raster)
library(mapview)
library(ggplot2)
library(viridis)

# Read data 

# Load Crimea land polygon data
load("data/Crimea-osmland.Rdata")
load("data/worldclim_wind_vars.Rdata")

# Read as stars-proxy objects and crop by crimea land polygon
elevation_30s <- read_stars("data/ASTERGDEM3/Processed/Elevation_30s.tif", quiet = T, proxy = T) %>% .[crimea_land]
elevation_2p5min <- read_stars("data/ASTERGDEM3/Processed/Elevation_2p5min.tif", quiet = T, proxy = T) %>% .[crimea_land]
elevation_5min <- read_stars("data/ASTERGDEM3/Processed/Elevation_5min.tif", quiet = T, proxy = T) %>% .[crimea_land]

slope_30s <- read_stars("data/ASTERGDEM3/Processed/Slope_30s.tif", quiet = T, proxy = T) %>% .[crimea_land]
slope_2p5min <- read_stars("data/ASTERGDEM3/Processed/Slope_2p5min.tif", quiet = T, proxy = T) %>% .[crimea_land]
slope_5min <- read_stars("data/ASTERGDEM3/Processed/Slope_5min.tif", quiet = T, proxy = T) %>% .[crimea_land]

aspect_30s <- read_stars("data/ASTERGDEM3/Processed/Aspect_30s.tif", quiet = T, proxy = T) %>% .[crimea_land]
aspect_2p5min <- read_stars("data/ASTERGDEM3/Processed/Aspect_2p5min.tif", quiet = T, proxy = T) %>% .[crimea_land]
aspect_5min <- read_stars("data/ASTERGDEM3/Processed/Aspect_5min.tif", quiet = T, proxy = T) %>% .[crimea_land]

# Elevation
# Convert from stars-proxy to stars obj and resample as worldclim data
elevation_30s %>% 
  st_as_stars() %>% 
  st_warp(worldclim_wind_stars) -> elevation_30s
elevation_2p5min %>% 
  st_as_stars() %>% 
  st_warp(worldclim_wind_stars) -> elevation_2p5min
# Plot
plot(elevation_30s, col = viridis(30))
plot(elevation_2p5min, col = viridis(30))

# Slope
slope_30s %>% 
  st_as_stars() %>% 
  st_warp(worldclim_wind_stars) -> slope_30s
slope_2p5min %>% 
  st_as_stars() %>% 
  st_warp(worldclim_wind_stars) -> slope_2p5min
slope_5min %>% 
  st_as_stars() %>% 
  st_warp(worldclim_wind_stars) -> slope_5min

# Plot
plot(slope_30s, col = viridis(30))
plot(slope_2p5min, col = viridis(30))
plot(slope_5min, col = viridis(30))

# Aspect
aspect_30s %>% 
  st_as_stars() %>% 
  st_warp(worldclim_wind_stars) -> aspect_30s
aspect_2p5min %>% 
  st_as_stars() %>% 
  st_warp(worldclim_wind_stars) -> aspect_2p5min
aspect_5min %>% 
  st_as_stars() %>% 
  st_warp(worldclim_wind_stars) -> aspect_5min
# Plot
plot(aspect_30s, col = rainbow(24))
plot(aspect_2p5min, col = rainbow(24))
plot(aspect_5min, col = rainbow(24))

# Convert to RasterLayers
elevation_30s <- as(elevation_30s, "Raster")
elevation_2p5min <- as(elevation_2p5min, "Raster")
slope_30s <- as(slope_30s, "Raster")
slope_2p5min <- as(slope_2p5min, "Raster")
slope_5min <- as(slope_5min, "Raster")
aspect_30s <- as(aspect_30s, "Raster")
aspect_2p5min <- as(aspect_2p5min, "Raster")
aspect_5min <- as(aspect_5min, "Raster")

# Save to Rdata file
save(elevation_30s, elevation_2p5min, slope_30s, slope_2p5min, slope_5min,
     aspect_30s, aspect_2p5min, aspect_5min, file = "data/terrain_vars.Rdata")
