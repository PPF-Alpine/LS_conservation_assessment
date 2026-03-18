library(sf)
library(terra)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(ggplot2)
library(units)
library(lwgeom)

# cut borders in 50 km segments
source(here::here("R/00_Config_file_HKH.R"))

library(stringr)
#---------------------------------------------#
# get border segments
#---------------------------------------------#

border_segments <- sf::st_read(paste0(data_storage_path, "Output/transboundary/borders_segments_100_countrypairs.shp"))

#---------------------------------------------#
# get threathened species raster stack 
#---------------------------------------------#
total_endemism_join <- read.csv(paste0(data_storage_path,"Datasets/species_list/species_endemism/total_species_endemism.csv"))|>
  filter(!is.na(pct_in_HKH_area), pct_in_HKH_area > 0)

species_list <- readxl::read_excel(paste0(data_storage_path,"Datasets/species_list/assessment_hkh_mammals_09082025_LS.xlsx"))|>
  rename(species = sciname)

total_endemism_join_test <- total_endemism_join |>
  left_join(
    species_list |>
      select(species, status_summary_global,status_summary_national) |>
      distinct(),
    by = "species"
  )


threatened <- total_endemism_join_test|> filter(status_summary_global=="threatened")

# threatened <- total_endemism_join_test|> filter(status_summary_national=="threatened")

# Load DEM for HKH; used as both grid template and mask
dem_crop <- rast(paste0(data_storage_path, "Datasets/DEM_HKH/DEM_HKH.tif"))
template <- dem_crop
template <- terra::project(dem_crop, "EPSG:8857")

#---------------------------------------------#
# select rasters for species and allign them
#---------------------------------------------#


# terra performance settings:
#  - memfrac: fraction of available RAM terra can use internally
#  - progress: show progress updates
terraOptions(
  memfrac = 0.6,            # allow terra to manage RAM
  progress = 1
)

# Raster write options:
#  - wopt_in: aligned per-species rasters written as compact unsigned bytes
#  - wopt_out: richness output written as compact unsigned integers
wopt_in  <- list(datatype="INT1U", gdal=c("TILED=YES","COMPRESS=LZW","ZLEVEL=6"))
wopt_out <- list(datatype="INT2U", gdal=c("TILED=YES","COMPRESS=LZW","PREDICTOR=2","ZLEVEL=9"))

# Directory containing all species rasters
in_dir  <- file.path(data_storage_path, "Datasets", "species_list", "rasterfiles")
files   <- list.files(in_dir, pattern="\\.tif$", full.names=TRUE)

# Filter by species names without a giant regex
#   - Species names in the table are "Genus species"
#   - Filenames are expected to begin with "Genus_species..."

# ‼️‼️replace target keys
target_keys <- str_replace_all(threatened$species, " ", "_")
stems       <- tools::file_path_sans_ext(basename(files))

# Select files whose basenames start with one of the target keys
files_subset <- files[str_detect(basename(files), paste0("^(", paste(target_keys, collapse="|"), ")"))]

message("Found ", length(files_subset), " rasters for target species.")

#file_subset_test <- files_subset[1:5]

#---------------------------------------------#
# Function: align one raster to template, write to disk
# (crop early -> project if needed -> resample)
#---------------------------------------------#
align_write <- function(infile, outdir) {
  
  r <- rast(infile)
  
  # 1. Reproject first if CRS differs
  if (!terra::same.crs(r, template)) {
    r <- terra::project(r, template, method = "near")
  }
  
  # 2. Crop after CRS matches
  r <- terra::crop(r, terra::ext(template), snap = "out")
  
  # 3. Resample to exact template grid
  r <- terra::resample(r, template, method = "near")
  
  # 4. Write aligned raster
  out <- file.path(
    outdir,
    paste0(tools::file_path_sans_ext(basename(infile)), "_al.tif")
  )
  
  terra::writeRaster(r, out, overwrite = TRUE, wopt = wopt_in)
  
  out
}

#---------------------------------------------#
# Step 1: align all rasters TO DISK (no big stack in RAM)
#---------------------------------------------#
# Use a temp directory to store aligned rasters (safe + fast for large batches)
tmp_aligned_dir <- file.path(tempdir(), "aligned_bin")
dir.create(tmp_aligned_dir, showWarnings = FALSE)

# Align each species raster and return aligned file paths
aligned_files <- vapply(
  files_subset,
  align_write,
  character(1),
  outdir = tmp_aligned_dir
)


##############TEST

r <- rast(aligned_files[2])

crs(r)
crs(template)

plot(r)
plot(border_segments$geometry, add = TRUE)
