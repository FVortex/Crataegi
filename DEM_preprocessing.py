import os
import rasterio
from rasterio.merge import merge
from rasterio.plot import show
from rasterio.enums import Resampling
import glob
import richdem as rd
from rasterio import Affine

# Set the working directory
os.chdir("/Users/aleksandrsheludkov/Desktop/ИГРАН/Ecological Niche by SDM")

# ==================================================
# 1. Read tif files and merge them insto single file

# File and folder paths
dirpath = "data/ASTERGDEM3"

# Make a search criteria to select the DEM files
search_criteria = "*.tif"
q = os.path.join(dirpath, search_criteria)

# List all dem files with glob() function
dem_fps = glob.glob(q)

# Create an empty list for the datafiles that will be part of the mosaic
src_files_to_mosaic = []

# Open all those files in read mode with raterio and add those files into a our source file list
for fp in dem_fps:
    src = rasterio.open(fp)
    src_files_to_mosaic.append(src)

# Merge the files and create a mosaic with rasterio’s merge function
# Merge function returns a single mosaic array and the transformation info
mosaic, out_trans = merge(src_files_to_mosaic)

# Let's take a look
show(mosaic, cmap='terrain')

# Copy and update the metadata
out_meta = src.meta.copy()
out_meta.update({"driver": "GTiff",
                 "height": mosaic.shape[1],
                 "width": mosaic.shape[2],
                 "transform": out_trans,
                 "crs": "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
                 }
    )

# Write the mosaic raster to disk
# First, specify outputfile path
out_tif = "data/ASTERGDEM3/Processed/Elevation_raw.tif"
with rasterio.open(out_tif, "w", **out_meta) as dest:
    dest.write(mosaic)

# =========================================
# 2. Resample elevation to lower resolution
    
# The original resolution is 1`` (~30m)
# To estimate the relief affects of different scale we resample dem to 30``,
# 2.5` and 5` resolution

# 2.1. 30`` resolution

# Read elevation and downsample to 30''
with rasterio.open("data/ASTERGDEM3/Processed/Elevation_raw.tif") as dataset:
    elevation_low = dataset.read(
        out_shape=(dataset.count, dataset.height // 30, dataset.width // 30),
        resampling=Resampling.average
    )

show(elevation_low, cmap='terrain')

# Copy and update the metadata
out_meta = dataset.meta.copy()
out_meta.update({"driver": "GTiff",
                 "height": elevation_low.shape[1],
                 "width": elevation_low.shape[2],
                 "transform": Affine(0.000277777777777778 * 30, 0.0, 31.9998611111111, 
                                     0.0, -0.000277777777777778 * 30, 47.0001388888889),
                 "crs": "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
                 }
    )

# Write the mosaic raster to disk
# First, specify outputfile path
out_tif = "data/ASTERGDEM3/Processed/Elevation_30s.tif"
with rasterio.open(out_tif, "w", **out_meta) as dest:
    dest.write(elevation_low)
    
# 2.2. 2.5` resolution
    
# Read elevation and downsample to 30''
with rasterio.open("data/ASTERGDEM3/Processed/Elevation_raw.tif") as dataset:
    elevation_low = dataset.read(
        out_shape=(dataset.count, dataset.height // 150, dataset.width // 150),
        resampling=Resampling.average
    )

show(elevation_low, cmap='terrain')

# Copy and update the metadata
out_meta = dataset.meta.copy()
out_meta.update({"driver": "GTiff",
                 "height": elevation_low.shape[1],
                 "width": elevation_low.shape[2],
                 "transform": Affine(0.000277777777777778 * 150, 0.0, 31.9998611111111, 
                                     0.0, -0.000277777777777778 * 150, 47.0001388888889),
                 "crs": "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
                 }
    )

# Write the mosaic raster to disk
# First, specify outputfile path
out_tif = "data/ASTERGDEM3/Processed/Elevation_2p5min.tif"
with rasterio.open(out_tif, "w", **out_meta) as dest:
    dest.write(elevation_low)
    
    
# 2.3. 5` resolution
    
# Read elevation and downsample to 30''
with rasterio.open("data/ASTERGDEM3/Processed/Elevation_raw.tif") as dataset:
    elevation_low = dataset.read(
        out_shape=(dataset.count, dataset.height // 300, dataset.width // 300),
        resampling=Resampling.average
    )

show(elevation_low, cmap='terrain')

# Copy and update the metadata
out_meta = dataset.meta.copy()
out_meta.update({"driver": "GTiff",
                 "height": elevation_low.shape[1],
                 "width": elevation_low.shape[2],
                 "transform": Affine(0.000277777777777778 * 300, 0.0, 31.9998611111111, 
                                     0.0, -0.000277777777777778 * 300, 47.0001388888889),
                 "crs": "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
                 }
    )

# Write the mosaic raster to disk
# First, specify outputfile path
out_tif = "data/ASTERGDEM3/Processed/Elevation_5min.tif"
with rasterio.open(out_tif, "w", **out_meta) as dest:
    dest.write(elevation_low)

# =============================
# 3. Calculate slope and aspect

# 3.1. 30`` resolution

# Read elevarion with rd.LoadGDAL
elevation_raw = rd.LoadGDAL("data/ASTERGDEM3/Processed/Elevation_30s.tif", no_data = -9999)

# Calculate slope
slope = rd.TerrainAttribute(elevation_raw, attrib='slope_riserun')
rd.rdShow(slope, axes=False, cmap='magma', figsize=(8, 5.5))

# Export as tif
rd.SaveGDAL("data/ASTERGDEM3/Processed/Slope_30s.tif", slope)

# Calculate aspect
aspect = rd.TerrainAttribute(elevation_raw, attrib='aspect')
rd.rdShow(aspect, axes=False, cmap='jet', figsize=(8, 5.5))

# Export as tif
rd.SaveGDAL("data/ASTERGDEM3/Processed/Aspect_30s.tif", aspect)

# 3.2. 2.5` resolution

# Read elevarion with rd.LoadGDAL
elevation_raw = rd.LoadGDAL("data/ASTERGDEM3/Processed/Elevation_2p5min.tif", no_data = -9999)

# Calculate slope
slope = rd.TerrainAttribute(elevation_raw, attrib='slope_riserun')
rd.rdShow(slope, axes=False, cmap='magma', figsize=(8, 5.5))

# Export as tif
rd.SaveGDAL("data/ASTERGDEM3/Processed/Slope_2p5min.tif", slope)

# Calculate aspect
aspect = rd.TerrainAttribute(elevation_raw, attrib='aspect')
rd.rdShow(aspect, axes=False, cmap='jet', figsize=(8, 5.5))

# Export as tif
rd.SaveGDAL("data/ASTERGDEM3/Processed/Aspect_2p5min.tif", aspect)

# 3.3. 5` resolution

# Read elevarion with rd.LoadGDAL
elevation_raw = rd.LoadGDAL("data/ASTERGDEM3/Processed/Elevation_5min.tif", no_data = -9999)

# Calculate slope
slope = rd.TerrainAttribute(elevation_raw, attrib='slope_riserun')
rd.rdShow(slope, axes=False, cmap='magma', figsize=(8, 5.5))

# Export as tif
rd.SaveGDAL("data/ASTERGDEM3/Processed/Slope_5min.tif", slope)

# Calculate aspect
aspect = rd.TerrainAttribute(elevation_raw, attrib='aspect')
rd.rdShow(aspect, axes=False, cmap='jet', figsize=(8, 5.5))

# Export as tif
rd.SaveGDAL("data/ASTERGDEM3/Processed/Aspect_5min.tif", aspect)