# Terrain covariates for SDM

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
elevation <- read_stars("data/ASTERGDEM3/Processed/Elevation_lowres.tif", quiet = T, proxy = T) %>% .[crimea_land]
slope <- read_stars("data/ASTERGDEM3/Processed/Slope.tif", quiet = T, proxy = T) %>% .[crimea_land]
aspect <- read_stars("data/ASTERGDEM3/Processed/Aspect.tif", quiet = T, proxy = T) %>% .[crimea_land]

# Elevation
# Convert from stars-proxy to stars obj
elevation %>% st_as_stars() -> elevation
# Resample as worldclim data
elevation %>% st_warp(worldclim_wind_stars, use_gdal = T, method = "bilinear") -> elevation
# Plot original and resampled data
plot(elevation, col = viridis(30))

# Slope
slope %>% st_as_stars() -> slope
slope %>% st_warp(worldclim_wind_stars, use_gdal = T, method = "average") -> slope
plot(slope, col = viridis(30))

# Aspect
aspect %>% st_as_stars() -> aspect
aspect %>% st_warp(worldclim_wind_stars, use_gdal = T, method = "near") -> aspect
plot(aspect, col = rainbow(24))

# Convert to RasterLayers
elevation <- as(elevation, "Raster")
slope <- as(slope, "Raster")
aspect <- as(aspect, "Raster")

# Save to Rdata file
save(elevation, slope, aspect, file = "data/terrain_vars.Rdata")