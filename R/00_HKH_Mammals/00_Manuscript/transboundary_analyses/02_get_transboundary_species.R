

library(stringr)
#---------------------------------------------#
# get border segments
#---------------------------------------------#

border_segments <- sf::st_read(paste0(data_storage_path, "Datasets/protected_areas/borders_segments_100.shp"))

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


#threatened <- total_endemism_join_test|> filter(status_summary_global=="threatened")

threatened <- total_endemism_join_test|>
  filter(status_summary_national=="threatened")

# Load DEM for HKH; used as both grid template and mask
dem_crop <- rast(paste0(data_storage_path, "Datasets/DEM_HKH/DEM_HKH.tif"))
template <- dem_crop

#---------------------------------------------#
# select rasters for species and allign them
#---------------------------------------------#
# Template grid for alignment operations
template <- dem_crop  # your aligned DEM / target grid

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


#---------------------------------------------#
# Function: align one raster to template, write to disk
# (crop early -> project if needed -> resample)
#---------------------------------------------#
# For each input raster:
#   1) Read from disk
#   2) Crop to template extent (reduces size early)
#   3) Reproject to template CRS if needed (nearest neighbor for 0/1 data)
#   4) Resample to template grid (nearest neighbor)
#   5) Write aligned raster to a temporary aligned directory
align_write <- function(infile, outdir) {
  r <- rast(infile)
  # Early crop to template extent to reduce size
  r <- crop(r, ext(template), snap="out")
  # Reproject if needed
  if (!identical(crs(r), crs(template))) {
    r <- project(r, template, method="near")  # 0/1 data, so nearest neighbor
  }
  # Resample to template grid
  r <- resample(r, template, method="near")
  # Write aligned single-layer raster to disk
  out <- file.path(outdir, paste0(tools::file_path_sans_ext(basename(infile)), "_al.tif"))
  writeRaster(r, out, overwrite=TRUE, wopt=wopt_in)
  out
}

#---------------------------------------------#
# Step 1: align all rasters TO DISK (no big stack in RAM)
#---------------------------------------------#
# Use a temp directory to store aligned rasters (safe + fast for large batches)
tmp_aligned_dir <- file.path(tempdir(), "aligned_bin")
dir.create(tmp_aligned_dir, showWarnings = FALSE)

# Align each species raster and return aligned file paths
aligned_files <- vapply(files_subset, align_write, character(1), outdir=tmp_aligned_dir)
