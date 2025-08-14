library(terra)
library(stringr)

dem_crop <- rast(paste0(data_storage_path, "Datasets/DEM_HKH/DEM_HKH.tif"))


#---------------------------------------------#
# Step 0: set paths & template
#---------------------------------------------#
in_dir   <- file.path(data_storage_path, "Datasets", "species_list", "rasterfiles")
files    <- list.files(in_dir, pattern = "\\.tif$", full.names = TRUE)
template <- dem_crop  # use your DEM as the reference grid

message("Found ", length(files), " rasters.")

#---------------------------------------------#
# Step 1: read all rasters
#---------------------------------------------#
ras_list <- lapply(files, rast)

plot(template)

#---------------------------------------------#
# Step 2: align each raster to DEM (CRS/res/extent)
#         (needed so they can be stacked)
#---------------------------------------------#
align_to_dem <- function(r) {
  # project if CRS differs
  if (!identical(crs(r), crs(template))) {
    r <- project(r, template, method = "near")
  }
  # resample to DEM grid
  r <- resample(r, template, method = "near")
  # extend to DEM extent
  r <- extend(r, template)
  # fill NAs with 0
  r <- ifel(is.na(r), 0, r)
  r
}

# ♻️ takes some time❗
 
ras_aligned <- lapply(ras_list, align_to_dem)

#---------------------------------------------#
# Step 3: stack them
#---------------------------------------------#
stk <- rast(ras_aligned)
plot(stk)
#---------------------------------------------#
# Step 4: mask with DEM (restrict to DEM footprint)
#   (crop to extent + mask out DEM NAs)
#---------------------------------------------#
stk <- crop(stk, template)
stk_masked <- mask(stk, template)
plot(stk_masked)
#---------------------------------------------#
# Step 5: richness = sum of 0/1 layers
#---------------------------------------------#
richness <- sum(stk_masked, na.rm = TRUE)

plot(richness)

# save (optional)
out_file <- file.path(data_storage_path, "Datasets", "species_list", "HKH_species_richness.tif")
writeRaster(richness, out_file, overwrite = TRUE, datatype = "INT2U",
            gdal = c("TILED=YES", "COMPRESS=LZW", "PREDICTOR=2", "ZLEVEL=9"))

plot(richness, main = "Species richness (count)")
