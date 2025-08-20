library(terra)
library(stringr)
library(terra)
library(sf)
library(stringr)
library(purrr)
library(dplyr)
library(tibble)

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
gc()

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
gc()
# from here can be saved and stack can be further used to 
# 1. calculate richness
# 2. calculate endemism

#---------------------------------------------#
# Step 4: mask with DEM extent (which is HKH boundary)
#   (crop to extent + mask out DEM NAs)
#---------------------------------------------#

# dataframe with number and percentage of cells that fall within HKH boundary for each species 

stk <- crop(stk, template)
gc()
stk_masked <- mask(stk, template)
gc()

#---------------------------------------------#
# Step 5: endemism in hkh 
#---------------------------------------------#

# species cell counts within HKH and total
total_cells <- as.numeric(global(stk,        fun = "sum", na.rm = TRUE)[,1])
hkh_cells   <- as.numeric(global(stk_masked, fun = "sum", na.rm = TRUE)[,1])

# Area 
cell_km2 <- cellSize(template, unit = "km")

total_area_km2 <- as.numeric(global(stk        * cell_km2, "sum", na.rm = TRUE)[,1])
hkh_area_km2   <- as.numeric(global(stk_masked * cell_km2, "sum", na.rm = TRUE)[,1])

# Combine into one dataframe 
res <- tibble(
  species = names(stk),
  total_cells    = total_cells,
  hkh_cells      = hkh_cells,
  pct_in_HKH_cells = if_else(total_cells > 0, 100 * hkh_cells / total_cells, NA_real_),
  total_area_km2 = total_area_km2,
  hkh_area_km2   = hkh_area_km2,
  pct_in_HKH_area = if_else(total_area_km2 > 0, 100 * hkh_area_km2 / total_area_km2, NA_real_)
)

# Sort by species with highest % in HKH (optional)
res <- res |>
  arrange(desc(pct_in_HKH_area))

out_path <- file.path(data_storage_path, "Datasets/species_list/species_endemism/species_endemism_401_497.csv")

# Save as CSV
write_csv(res, out_path)
