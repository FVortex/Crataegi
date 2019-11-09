# Crop worldclim data by Crimea osmland

library(dplyr)
library(sf)
library(stars)
library(raster)
library(mapview)
library(ggplot2)
library(viridis)
library(ggthemes)

# Load Crimea land polygon data
load("data/Crimea-osmland.Rdata")

# ==================
# 1. Worldclim2 wind
# ==================

# 1.1. Preprocessing

# List tif files from the folder
worldclim_wind_files <- list.files(path = "data/wc2 wind/", pattern = ".tif")

# read as stars-proxy objects
# @proxy: allows to work on subsets of data without loading into memory all the data
worldclim_wind <- read_stars(paste0("data/wc2 wind/", worldclim_wind_files), quiet = T, proxy = T)

# Crop worldclim data by 
worldclim_wind[crimea_land] -> worldclim_crimea_wind
rm(worldclim_wind) # delete original object

# Convert to stars object
worldclim_crimea_wind %>% st_as_stars() -> worldclim_wind_stars

# Convert to RasterStack
worldclim_wind_stack <- stack()
for(i in 1:length(worldclim_wind_stars)){
  worldclim_wind_stack <- stack(worldclim_wind_stack, as(worldclim_wind_stars[i], 'Raster'))
}

# 1.2. Calculate predictors
# List of variables to calculate
# annual min month (2)
# annual max month (3)

# Which are the windiest and less windiest monthes?
getValues(worldclim_wind_stack) %>% 
  boxplot()
# the windienst is February
worldclim_wind_raster <- worldclim_wind_stack[[2]]
names(worldclim_wind_raster) <- c("max month")

# Let's take a look
worldclim_wind_raster %>% plot(col = viridis(29))

# Select February in star obj
worldclim_wind_stars <- worldclim_wind_stars["wc2.0_30s_wind_02.tif"]

# Save as Rdata file
save(worldclim_wind_raster,worldclim_wind_stars, file = "data/worldclim_wind_vars.Rdata")

# ================================
# 2. Worldclim2 standard variables
# ================================

# Since 30' res worldclim data file is broken, we use 2.5 degree res and then resamle

# List tif files from the folder
worldclimfiles <- list.files(path = "data/wc2 standard/", pattern = ".tif")

# read as stars-proxy objects
# @proxy: allows to work on subsets of data without loading into memory all the data
worldclim <- read_stars(paste0("data/wc2 standard/", worldclimfiles), quiet = T, proxy = T)

# Crop worldclim data by 
worldclim[crimea_land] -> worldclim_crimea
rm(worldclim) # delete original object

# Convert to stars object
worldclim_crimea %>% st_as_stars() -> worldclim_crimea

# Resample
worldclim_crimea %>% st_warp(worldclim_wind_stars) -> worldclim_crimea

# # Save as file
# save(worldclim_crimea, file = "data/worldclim_crimea_star.Rdata")

# Convert into RasterStack
# Create empty stack and add layers one by one
worldclim_crimea_stack <- stack()
for(i in 1:length(worldclim_crimea)){
  worldclim_crimea_stack <- stack(worldclim_crimea_stack, as(worldclim_crimea[i], 'Raster'))
}

# Set varible names
names(worldclim_crimea_stack) <- paste0("BIO", 1:19)

# Save the file into
save(worldclim_crimea_stack, file = "data/worldclim_crimea.Rdata")

# c(worldclim_crimea, along = list(vars = bioclim_vars))
# 
# st_redimension(worldclim_crimea, new_dims = st_dimensions(worldclim_crimea), 
#                along = list(vars = bioclim_vars))
# 
# ggplot()+
#   # geom_sf(data = crimea_land)+
#   geom_stars(data = st_redimension(worldclim_crimea, new_dims = st_dimensions(worldclim_crimea), 
#                                    along = list(vars = bioclim_vars)),
#              alpha = 0.8) + 
#   facet_wrap("vars") +
#   scale_fill_viridis(na.value = "white") +
#   # coord_cartesian()+
#   theme_map()+
#   theme(legend.position = "bottom")

# ========================
# 3. Worldclim2 insolation
# ========================

# 3.1. Preprocessing

# List tif files from the folder
worldclim_solar_files <- list.files(path = "data/wc2 solar radiation/", pattern = ".tif")

# read as stars-proxy objects
# @proxy: allows to work on subsets of data without loading into memory all the data
worldclim_solar <- read_stars(paste0("data/wc2 solar radiation/", worldclim_solar_files), quiet = T, proxy = T)

# Crop worldclim data by 
worldclim_solar[crimea_land] -> worldclim_solar_crimea
rm(worldclim_solar) # delete original object

# Convert to stars object
worldclim_solar_crimea %>% st_as_stars() -> worldclim_solar_crimea

# Convert to RasterStack
worldclim_solar_stack <- stack()
for(i in 1:length(worldclim_solar_crimea)){
  worldclim_solar_stack <- stack(worldclim_solar_stack, as(worldclim_solar_crimea[i], 'Raster'))
}

# 3.2. Calculate predictors
# List of variables to calculate
# annual solar unflux (1)
# max month (3)

# Which are the sunniest and less sunniest monthes?
getValues(worldclim_solar_stack) %>% 
  boxplot()
# the sunniest is July

# Select variables
worldclim_solar_vars <- stack(sum(worldclim_solar_stack)*30,
                              worldclim_solar_stack[[7]])
names(worldclim_solar_vars) <- c("annual sum", "max month")

# Let's take a look
plot(worldclim_solar_vars, col = viridis(29))

# Save as Rdata file
save(worldclim_solar_vars, file = "data/worldclim_solar_vars.Rdata")
