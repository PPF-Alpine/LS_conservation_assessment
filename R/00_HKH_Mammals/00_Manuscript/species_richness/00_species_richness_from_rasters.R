library(terra)
library(stringr)
library(terra)
library(sf)
library(stringr)
library(purrr)
library(dplyr)
library(tibble)
library(tools)

source(here::here("R/00_Config_file_HKH.R"))

dem_crop <- rast(paste0(data_storage_path, "Datasets/DEM_HKH/DEM_HKH.tif"))
template<-dem_crop

#---------------------------------------------#
# Step 0: set paths & template
#---------------------------------------------#
species_from_file <- function(path, with_space = FALSE) {
  bn <- file_path_sans_ext(basename(path))
  parts <- strsplit(bn, "_", fixed = TRUE)[[1]]
  sp <- parts[1:min(2, length(parts))]              # first two tokens
  if (with_space) paste(sp, collapse = " ") else paste(sp, collapse = "_")
}

align_to_dem <- function(r, template) {
  if (!identical(crs(r), crs(template))) r <- project(r, template, method = "near")
  r <- resample(r, template, method = "near")
  r <- extend(r, template)
  r <- ifel(is.na(r), 0, r)
  r
}

#---------------------------------------------#
# Step 1: read all rasters
#---------------------------------------------#
in_dir <- file.path(data_storage_path, "Datasets", "species_list", "rasterfiles")
files  <- list.files(in_dir, pattern = "\\.tif$", full.names = TRUE)
message("Found ", length(files), " rasters.")

## do in subests
in_dir  <- file.path(data_storage_path, "Datasets", "species_list", "rasterfiles")
files   <- list.files(in_dir, pattern = "\\.tif$", full.names = TRUE)
n_files <- length(files)
message("Found ", n_files, " rasters.")

# choose a block of files (300 max is too much already)
# 1-100 works
block    <- 401:497
files_subset <- files[block]

# check how many actually exist (useful at the end of the folder)
files_subset <- files_subset[!is.na(files_subset)]
#---------------------------------------------#
# Step 2: align each raster to DEM (CRS/res/extent)
#         (needed so they can be stacked)
#---------------------------------------------#
ras_aligned <- lapply(files_subset, function(fp) {
  r <- rast(fp)
  r <- align_to_dem(r, dem_crop)
  names(r) <- species_from_file(fp, with_space = FALSE)  # use "_" to be safe for layer names
  r
})

# Ensure unique names 
ln <- vapply(ras_aligned, function(r) names(r), character(1))
if (any(duplicated(ln))) {
  names(ras_aligned) <- make.unique(ln, sep = "_")
  for (i in seq_along(ras_aligned)) names(ras_aligned[[i]]) <- names(ras_aligned)[i]
}

#---------------------------------------------#
# Step 3: stack them
#---------------------------------------------#
stk <- rast(ras_aligned)
plot(stk)
names(stk)

# from here can be saved and stack can be further used to 
# 1. calculate richness
# 2. calculate endemism

#---------------------------------------------#
# Step 4: mask with DEM extent (which is HKH boundary)
#   (crop to extent + mask out DEM NAs)
#---------------------------------------------#

# dataframe with number and percentage of cells that fall within HKH boundary for each species 

stk <- crop(stk, template)
stk_masked <- mask(stk, template)
plot(stk_masked)
#---------------------------------------------#
# Step 5: richness = sum of 0/1 layers
#---------------------------------------------#
richness <- sum(stk_masked, na.rm = TRUE)

plot(richness)

# save 
out_file <- file.path(data_storage_path, "Datasets", "species_list","species_richness","HKH_species_richness_401_497.tif")
writeRaster(richness, out_file, overwrite = TRUE, datatype = "INT2U",
            gdal = c("TILED=YES", "COMPRESS=LZW", "PREDICTOR=2", "ZLEVEL=9"))

plot(richness, main = "Species richness (count)")

