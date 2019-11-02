####Crimean Crataegus species. Worldclim2 data preprocessing
#Orlov and Sheludkov
#libraries and clearence

rm(list = ls())
library(dplyr)
library(sp)
library(sf)
library(stars)
library(rgbif) 
library(sdm)
library(maps)
library(raster)
library(mapview)
library(ggplot2)

# ================
# 1. Preprocessing
# ================

# ==========================
# 1.1. Load OSM land polygon 
load("data/Crimea-osmland.Rdata")

# # Define coordinate reference system (crs) not-projected data (in degrees, not metres)
# WGS84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# ==============
# 1.2. Worldclim
load("data/worldclim_crimea.Rdata")
load("data/worldclim_solar_vars.Rdata")
load("data/worldclim_wind_vars.Rdata")

# ==============
# 1.3. GBIF data

# download data within the bounding box of land polygon
occ_crataegi_crimea <- occ_search(scientificName = 'Crataegus', hasCoordinate = T,
                                  skip_validate = T,
                                  geometry = st_bbox(crimea_land))

# Extract data frame
clean <- (occ_crataegi_crimea$data)

# Create sp obj, convert to sf, extract the localities within the land polygon
localities <- sp::SpatialPointsDataFrame(coords = clean %>% dplyr::select(decimalLongitude, decimalLatitude), 
                                         proj4string = CRS(WGS84), 
                                         data = clean %>% dplyr::select(-c(decimalLatitude, decimalLongitude))) %>% 
  st_as_sf() %>% .[crimea_land,]

# Let's take a look
mapview(localities)
# 348 localities in total

#where did they came from?
localities$basisOfRecord %>% table %>% pie(col = c('orange', 'yellow', 'cyan'), main = 'Localities data sources')

#phylum Crateagus with no species is denoted 'NA'
#lets change to ''
localities$species[is.na(localities$species)] <- 'Crataegus sp.'
localities$species <- gsub(pattern = 'Crataegus', replacement = 'C.', localities$species)
localities$species ->  sps

#plots for number of records per species
par(mar = c(10,2,2,2))
sps %>% table %>% sort(decreasing = T) %>% 
  barplot(main = 'Crimean Crataegus', las = 2, cex.names = 1.25, col = c(rainbow(4), rep('grey', 7)))
abline(h = 50, lty = 2)

#lets take top-4 (>=50)
sps %>% table %>% sort(decreasing = T) -> tmp
chosen <- names(tmp[1:4])
#plot for Crataegi per species
Co <- localities[localities$species == chosen[1],]
Cme <- localities[localities$species == chosen[2],]
Cr <- localities[localities$species == chosen[3],]
Cmo <- localities[localities$species == chosen[4],]

localities_chosen <- localities %>% filter(species %in% chosen)

# plotting
localities_plot <- ggplot()+
  geom_sf(data = crimea_land, fill = "white", col = "grey20")+
  geom_point(data = localities_chosen, aes(x = st_coordinates(localities_chosen)[,1], 
                                           y = st_coordinates(localities_chosen)[,2],
                                           col = species, fill = species),
             pch = 21, size = 1.2, alpha = 0.5)+
  coord_sf()+
  theme(axis.title = element_blank())

# Export to jpeg
ggsave(localities_plot, filename = "localities.jpeg",
       path = "plots/",
       dpi = 300, width = 18, height = 10, units = "cm")

# Convert sf to spdf

# Crataegus orientalis
Co_spdf_crimea <- Co %>% as('Spatial')
plot(worldclim_crimea_stack[[1]], main = bioclim_vars[1])
points(Co_spdf_crimea, pch = 4, lwd = 3)
# #class(Co_spdf_crimea)

# meyeri
Cme_spdf_crimea <- Cme %>% as('Spatial')
plot(worldclim_crimea_stack[[1]], main = bioclim_vars[1])
points(Cme_spdf_crimea, pch = 4, lwd = 3)
# #class(Cme_spdf_crimea)

# rhipidophylla
Cr_spdf_crimea <- Cr %>% as('Spatial')
plot(worldclim_crimea_stack[[1]], main = bioclim_vars[1])
points(Cr_spdf_crimea, pch = 4, lwd = 3)
# #class(Cr_spdf_crimea)

# monogyna
Cmo_spdf_crimea <- Cmo %>% as('Spatial')
plot(worldclim_crimea_stack[[1]], main = bioclim_vars[1])
points(Cmo_spdf_crimea, pch = 4, lwd = 3)
# #class(Cmo_spdf_crimea)

# ============
# 2. Modelling
# ============

# Generate 100 random points for no-presence data
set.seed(90)
bg_spdf_crimea <- sampleRandom(worldclim_crimea_stack, size = 100, sp = T)

##### Crateagus orientalis comes first

# Combine presence/no-presence data
crimea_Co_and_bg <- SpatialPointsDataFrame(coords = rbind(bg_spdf_crimea@coords, Co_spdf_crimea@coords), 
                                           data = data_frame(Occurence = c(rep(0, nrow(bg_spdf_crimea)), rep(1, nrow(Co_spdf_crimea)))), 
                                           proj4string = worldclim_crimea_stack@crs)

# PART ML 
d_Co <- sdmData(formula = Occurence ~ ., train=crimea_Co_and_bg, predictors=worldclim_crimea_stack)
set.seed(90)
m1_Co <-sdm(Occurence ~ ., data = d_Co, methods = 'rf', replication='sub',  test.percent = 30, n = 10)

#model diagnostics

roc(m1_Co)

vi_Co <- getVarImp(m1_Co, id=1)
#model evaluation
#selecting the best-performing model
eval_Co <- getEvaluation(m1_Co)
which_best_Co <- which.max(eval_Co$AUC)
#what about all of the models (ids)?
# vis_all_Co <- c()
# for (i in 1:10) {
#   tmp <- getVarImp(m1_Co, id=i)
#   vis_all_Co <- cbind(vis_all_Co, tmp@varImportance$AUCtest)
# }

#setting proper variables names
vi_Co@variables <- bioclim_vars


##### Crateagus meyeri comes second ## 
#replacing variables with zeros indicating no-presence
outcomes_crimea_Cme <- as.data.frame(rep(1, nrow(Cme_spdf_crimea@data)))
outcomes_crimea_bg <- as.data.frame(c(rep(0, nrow(bg_spdf_crimea))))
outcomes_both_Cme <- as.data.frame(unlist(append(x = outcomes_crimea_bg, outcomes_crimea_Cme)), row.names = as.character(1:(nrow(Cme_spdf_crimea@data) + nrow(bg_spdf_crimea_50_points))))

#are these scaterred? yes
crimea <- map(ylim=c(44.3, 46), xlim=c(32.5,36.6), col='gray90', fill=TRUE)  
plot(bg_spdf_crimea_50_points, add = T)

#combining background and localities data frames
dim(Cme_spdf_crimea)
dim(bg_spdf_crimea)
raster::intersect(x = Cme_spdf_crimea, y = bg_spdf_crimea_50_points)
#or creating from scratch

#crimea_Dc_and_bg_presence_no <- SpatialPointsDataFrame(coords = rbind(bg_spdf_crimea_50_points@coords, Dc_spdf_crimea@coords), 
#                      data = cbind(outcomes_both, rbind(Dc_spdf_crimea, bg_spdf_crimea_50_points)),
#                     proj4string = world_clim_cropped_crimea@crs
#                    )
# class(crimea_Cme_and_bg_presence_no); dim(crimea_Cme_and_bg_presence_no)
# crimea <- map(ylim=c(44.3, 46), xlim=c(32.5,36.6), col='gray90', fill=TRUE)  
# plot(crimea_Dc_and_bg_presence_no, add = T)
# #the object is RasterBrick class. Munging it to SpatialPointsDataFrame

crimea_Cme_and_bg <- SpatialPointsDataFrame(coords = rbind(bg_spdf_crimea@coords, Cme_spdf_crimea@coords), 
                                            data = as.data.frame(c(rep(0, nrow(bg_spdf_crimea)), rep(1, nrow(Cme_spdf_crimea)))), 
                                            proj4string = world_clim_cropped_crimea@crs
)

names(crimea_Cme_and_bg) <- 'Occurence'
## presence / no-presence data
#PART ML 
d_Cme <- sdmData(formula = Occurence ~ ., train=crimea_Cme_and_bg, predictors=preds)
set.seed(90)
m1_Cme <-sdm(Occurence ~ ., data = d_Cme, methods = 'rf', replication='sub',  test.percent = 30, n = 10)

#model diagnostics

roc(m1_Cme)
#model evaluation
#selecting the best-performing model
eval_Cme <- getEvaluation(m1_Cme)
which_best_Cme <- which.max(eval_Cme$AUC)

vi_Cme <- getVarImp(m1_Cme, id = which_best_Cme)

#setting proper variables names
vi_Cme@variables <- bioclim_vars


##### Crateagus rhipidophylla is third ## 
#replacing variables with zeros indicating no-presence
outcomes_crimea_Cr <- as.data.frame(rep(1, nrow(Cr_spdf_crimea@data)))
outcomes_crimea_bg <- as.data.frame(c(rep(0, nrow(bg_spdf_crimea))))
outcomes_both_Cr <- as.data.frame(unlist(append(x = outcomes_crimea_bg, outcomes_crimea_Cr)), row.names = as.character(1:(nrow(Cr_spdf_crimea@data) + nrow(bg_spdf_crimea_50_points))))

#are these scaterred? yes
crimea <- map(ylim=c(44.3, 46), xlim=c(32.5,36.6), col='gray90', fill=TRUE)  
plot(bg_spdf_crimea_50_points, add = T)

#combining background and localities data frames
dim(Cr_spdf_crimea)
dim(bg_spdf_crimea)
raster::intersect(x = Cr_spdf_crimea, y = bg_spdf_crimea_50_points)
#or creating from scratch

#crimea_Dc_and_bg_presence_no <- SpatialPointsDataFrame(coords = rbind(bg_spdf_crimea_50_points@coords, Dc_spdf_crimea@coords), 
#                      data = cbind(outcomes_both, rbind(Dc_spdf_crimea, bg_spdf_crimea_50_points)),
#                     proj4string = world_clim_cropped_crimea@crs
#                    )
# class(crimea_Cr_and_bg_presence_no); dim(crimea_Cr_and_bg_presence_no)
# crimea <- map(ylim=c(44.3, 46), xlim=c(32.5,36.6), col='gray90', fill=TRUE)  
# plot(crimea_Dc_and_bg_presence_no, add = T)
# #the object is RasterBrick class. Munging it to SpatialPointsDataFrame

crimea_Cr_and_bg <- SpatialPointsDataFrame(coords = rbind(bg_spdf_crimea@coords, Cr_spdf_crimea@coords), 
                                            data = as.data.frame(c(rep(0, nrow(bg_spdf_crimea)), rep(1, nrow(Cr_spdf_crimea)))), 
                                            proj4string = world_clim_cropped_crimea@crs
)

names(crimea_Cr_and_bg) <- 'Occurence'
## presence / no-presence data
#PART ML 
d_Cr <- sdmData(formula = Occurence ~ ., train=crimea_Cr_and_bg, predictors=preds)
set.seed(90)
m1_Cr <-sdm(Occurence ~ ., data = d_Cr, methods = 'rf', replication='sub',  test.percent = 30, n = 10)

#model diagnostics

roc(m1)
#model evaluation
#selecting the best-performing model
eval_Cr <- getEvaluation(m1_Cr)
which_best_Cr <- which.max(eval_Cr$AUC)

vi_Cr <- getVarImp(m1_Cme, id = which_best_Cr)

#setting proper variables names
vi_Cr@variables <- bioclim_vars

##### Crateagus monoguna is the last ## 
#replacing variables with zeros indicating no-presence
outcomes_crimea_Cmo <- as.data.frame(rep(1, nrow(Cmo_spdf_crimea@data)))
outcomes_crimea_bg <- as.data.frame(c(rep(0, nrow(bg_spdf_crimea))))
outcomes_both_Cmo <- as.data.frame(unlist(append(x = outcomes_crimea_bg, outcomes_crimea_Cmo)), row.names = as.character(1:(nrow(Cmo_spdf_crimea@data) + nrow(bg_spdf_crimea_50_points))))

#are these scaterred? yes
crimea <- map(ylim=c(44.3, 46), xlim=c(32.5,36.6), col='gray90', fill=TRUE)  
plot(bg_spdf_crimea_50_points, add = T)

#combining background and localities data frames
dim(Cmo_spdf_crimea)
dim(bg_spdf_crimea)
raster::intersect(x = Cmo_spdf_crimea, y = bg_spdf_crimea_50_points)
#or creating from scratch

#crimea_Dc_and_bg_presence_no <- SpatialPointsDataFrame(coords = rbind(bg_spdf_crimea_50_points@coords, Dc_spdf_crimea@coords), 
#                      data = cbind(outcomes_both, rbind(Dc_spdf_crimea, bg_spdf_crimea_50_points)),
#                     proj4string = world_clim_cropped_crimea@crs
#                    )
# class(crimea_Cmo_and_bg_presence_no); dim(crimea_Cmo_and_bg_presence_no)
# crimea <- map(ylim=c(44.3, 46), xlim=c(32.5,36.6), col='gray90', fill=TRUE)  
# plot(crimea_Dc_and_bg_presence_no, add = T)
# #the object is RasterBrick class. Munging it to SpatialPointsDataFrame

crimea_Cmo_and_bg <- SpatialPointsDataFrame(coords = rbind(bg_spdf_crimea@coords, Cmo_spdf_crimea@coords), 
                                           data = as.data.frame(c(rep(0, nrow(bg_spdf_crimea)), rep(1, nrow(Cmo_spdf_crimea)))), 
                                           proj4string = world_clim_cropped_crimea@crs
)

names(crimea_Cmo_and_bg) <- 'Occurence'
## presence / no-presence data
#PART ML 
d_Cmo <- sdmData(formula = Occurence ~ ., train=crimea_Cmo_and_bg, predictors=preds)
set.seed(90)
m1_Cmo <-sdm(Occurence ~ ., data = d_Cmo, methods = 'rf', replication='sub',  test.percent = 30, n = 10)

#model diagnostics

roc(m1_Co)
roc(m1_Cme)
roc(m1_Cr)
roc(m1_Cmo)
#all are over 0.95
#model evaluation
#selecting the best-performing model
eval_Cmo <- getEvaluation(m1_Cmo)
which_best_Cmo <- which.max(eval_Cmo$AUC)

vi_Cmo <- getVarImp(m1_Cmo, id = which_best_Cmo)

#setting proper variables names
vi_Cmo@variables <- bioclim_vars


###################### vs
# par(mfrow = c(2,1),
#     mar = c(1,15,1,1))
# plot(vi_Co, 'auc', col = rainbow(19), main = 'C. orientalis')
# plot(vi_Cme, 'auc', main = 'C. meyeri', col = rainbow(19))
# plot(vi_Cmo, 'auc', main = 'C. rhipidophylla', col = rainbow(19))



#prediction; distibution

p1_Co <-predict(m1_Co[[which_best_Co]], newdata = worldclim_crimea_stack, overwrite = T, filename = 'p3.img')
p1_Cme <-predict(m1_Cme[[which_best_Cme]], newdata = worldclim_crimea_stack, overwrite = T, filename = 'p4.img')
p1_Cr <-predict(m1_Cr[[which_best_Cr]], newdata = worldclim_crimea_stack, overwrite = T, filename = 'p5.img')
p1_Cmo <-predict(m1_Cmo[[which_best_Cmo]], newdata = worldclim_crimea_stack, overwrite = T, filename = 'p6.img')


par(mar = rep(0.9, 4),
    mfrow = c(2, 2))
#best model fitted only
plot(p1_Co, main = 'C. orientalis')
plot(p1_Cme, main = 'C. meyeri')
plot(p1_Cr, main = 'C. rhipidophylla')
plot(p1_Cmo, main = 'C. monogyna')

#varimps comparison

sapply(list(vi_Co@varImportance$AUCtest,
vi_Cme@varImportance$AUCtest,
vi_Cr@varImportance$AUCtest,
vi_Cmo@varImportance$AUCtest),
barplot
)

vi_list <- list(vi_Co@varImportance$AUCtest,
                vi_Cme@varImportance$AUCtest,
                vi_Cr@varImportance$AUCtest,
                vi_Cmo@varImportance$AUCtest)

vi_list <- lapply(vi_list, FUN = function(x) {x/sum(x)})
#sanity check - are sums equal 1?  
sapply(vi_list,  sum) #sane!

#par(mar = c(9,3,3,3))
#barplot(vi_list, col = rep(rainbow(4), 19), beside = T,  cex.names = 2, names.arg = paste('BIO', 1:19), las =2 )
par(mfrow = c (1, 19),
    mar = rep(0.4, 4))
for (p in 1:19){
  l <- c(vi_list[[1]][p],
       vi_list[[2]][p],
       vi_list[[3]][p],
       vi_list[[4]][p])
  barplot(l, col = rainbow(4), space = F, ylim = c(0, 0.31), axes = F)
}
# 
# # poster accesoriees
# crimea <- map(ylim=c(44.3, 46), xlim=c(32.5,36.6), col='green', fill=TRUE)  
# pale <- rainbow(4)
# par(bg = pale[1])
# plot(1, type="n", xlab="", ylab="", xlim=c(0, 10), ylim=c(0, 10), axes = F)
# par(bg = pale[2])
# plot(1, type="n", xlab="", ylab="", xlim=c(0, 10), ylim=c(0, 10), axes = F)
# par(bg = pale[3])
# plot(1, type="n", xlab="", ylab="", xlim=c(0, 10), ylim=c(0, 10), axes = F)
# par(bg = pale[4])
# plot(1, type="n", xlab="", ylab="", xlim=c(0, 10), ylim=c(0, 10), axes = F)
# 
# vars <- as.data.frame(cbind(paste('BIO', 1:10),
#                             bioclim_vars[1:10],
#                             paste('BIO', 11:19),
#                             bioclim_vars[11:19]))
# 
# # #View(vars)
# 
# barplot(1:19, names.arg = paste('BIO', 1:19), las = 2)
