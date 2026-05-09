
transboundary_species <- readxl::read_xlsx(
  file.path(
    data_storage_path,
    "Datasets/transboundary/transb_species/GLOBAL/transboundary_species.xlsx"
  )
)

#---------------------------------------------#
# select rasters for species and allign them
#---------------------------------------------#
terraOptions(
  memfrac = 0.6,            # allow terra to manage RAM
  progress = 1
)

# Raster write options:
wopt_in  <- list(datatype="INT1U", gdal=c("TILED=YES","COMPRESS=LZW","ZLEVEL=6"))
wopt_out <- list(datatype="INT2U", gdal=c("TILED=YES","COMPRESS=LZW","PREDICTOR=2","ZLEVEL=9"))

# Directory containing all species rasters
in_dir  <- file.path(data_storage_path, "Datasets", "species_list", "rasterfiles")
files   <- list.files(in_dir, pattern="\\.tif$", full.names=TRUE)

# Filter by species names without a giant regex
# ‼️‼️replace target keys
target_keys <- str_replace_all(transboundary_species$species, " ", "_")
stems       <- tools::file_path_sans_ext(basename(files))

# Select files whose basenames start with one of the target keys
files_subset <- files[str_detect(basename(files), paste0("^(", paste(target_keys, collapse="|"), ")"))]

message("Found ", length(files_subset), " rasters for target species.")

#file_subset_test <- files_subset[1:5]

#---------------------------------------------#
# Function: align one raster to template, write to disk
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
# align to raster 
#---------------------------------------------#
# Use a temp directory to store aligned rasters (safe + fast for large batches)
tmp_aligned_dir <- file.path(tempdir(), "aligned_bin")
dir.create(tmp_aligned_dir, showWarnings = FALSE)

# Align each species raster and return aligned file paths
aligned_files_transboundary <- vapply(
  files_subset,
  align_write,
  character(1),
  outdir = tmp_aligned_dir
)


#---------------------------------------------#
# save the raster with the species names 
#---------------------------------------------#
# make species names from file names
species_names <- tools::file_path_sans_ext(basename(aligned_files_transboundary))

species_names <- gsub("_al$", "", species_names)
species_names <- gsub("_elev_masked$", "", species_names)

# create multilayer raster
richness <- rast(aligned_files_transboundary)

# assign species names as layer names
names(richness) <- species_names

plot(richness)

# save multilayer raster
out_file <- file.path(
  data_storage_path,
  "Datasets/transboundary/transb_species/GLOBAL/aligned_files_transboundary.tif"
)

writeRaster(
  richness,
  out_file,
  overwrite = TRUE,
  datatype = "INT2U",
  gdal = c("TILED=YES", "COMPRESS=LZW", "PREDICTOR=2", "ZLEVEL=9")
)
