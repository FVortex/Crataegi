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

# ================================
# 1. Worldclim2 standard variables
# ================================

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

# # Save as file
# save(worldclim_crimea, file = "data/worldclim_crimea_star.Rdata")

# Convert into RasterStack
# Create empty stack and add layers one by one
worldclim_crimea_stack <- stack()
for(i in 1:length(worldclim_crimea)){
  worldclim_crimea_stack <- stack(worldclim_crimea_stack, as(worldclim_crimea[i], 'Raster'))
}

# Set varible names
raw <- 'BIO1 = Annual Mean Temperature, BIO2 = Mean Diurnal Range (Mean of monthly (max temp - min temp)), BIO3 = Isothermality (BIO2/BIO7) (* 100), BIO4 = Temperature Seasonality (standard deviation *100), BIO5 = Max Temperature of Warmest Month, BIO6 = Min Temperature of Coldest Month, BIO7 = Temperature Annual Range (BIO5-BIO6), BIO8 = Mean Temperature of Wettest Quarter, BIO9 = Mean Temperature of Driest Quarter, BIO10 = Mean Temperature of Warmest Quarter, BIO11 = Mean Temperature of Coldest Quarter, BIO12 = Annual Precipitation, BIO13 = Precipitation of Wettest Month, BIO14 = Precipitation of Driest Month, BIO15 = Precipitation Seasonality (Coefficient of Variation), BIO16 = Precipitation of Wettest Quarter, BIO17 = Precipitation of Driest Quarter, BIO18 = Precipitation of Warmest Quarter, BIO19 = Precipitation of Coldest Quarter'
#raw1 <- gsub('BIO.*? = ', '', raw)
bioclim_vars <- unlist(strsplit(raw, split = ', '))
bioclim_vars <- substr(bioclim_vars, start = 8, 100)
names(worldclim_crimea_stack) <- bioclim_vars

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

# ==================
# 2. Worldclim2 wind
# ==================

# 2.1. Preprocessing

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

# 2.2. Calculate predictors
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

# Save as Rdata file
save(worldclim_wind_raster,worldclim_wind_stars, file = "data/worldclim_wind_vars.Rdata")

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
# min month (2)
# max month (3)

# Which are the sunniest and less sunniest monthes?
for(i in 1:12){
  print(mean(worldclim_solar_stack[[i]] %>% getValues(), na.rm = T))
}
# the sunniest is July
# the less sunniest is December
worldclim_solar_vars <- stack(sum(worldclim_solar_stack)*30, 
                              worldclim_solar_stack[[12]],
                              worldclim_solar_stack[[7]])
names(worldclim_solar_vars) <- c("annual sum", "min month", "max month")

# Let's take a look
plot(worldclim_solar_vars, col = viridis(29))

# Save as Rdata file
save(worldclim_solar_vars, file = "data/worldclim_solar_vars.Rdata")

# # ======================================================
# # 4. Assessing worldclim data with sdmpredictors package
# # ======================================================
# 
# library(sdmpredictors)
# 
# # list the layercoded for worldclim
# worldclim <- list_layers("WorldClim")
# # filter standart bioclim vars
# worldclim %>% 
#   filter(stringr::str_detect(layer_code, "WC_bio")) -> worldclim