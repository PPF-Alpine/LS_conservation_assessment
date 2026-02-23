library(terra)
library(stringr)
library(sf)


#===============================================================================#
# HKH Mammal Range-Raster Workflow
#===============================================================================#
# Purpose
#   This script:
#     1) Loads a precomputed species "endemism" table (range area + % in HKH).
#     2) Selects subsets of species based on range size, HKH containment, and
#        elevational range.
#     3) Locates corresponding species range rasters on disk.
#     4) Aligns those rasters to a DEM template grid (CRS/resolution/extent).
#     5) Produces and writes a species richness raster (sum of binary layers).
#
# Key assumptions
#   - Species rasters are binary (0/1) presence maps (or can be treated as such).
#   - The DEM "template" provides the target grid and HKH mask (NA outside HKH).
#   - Filenames begin with "Genus_species" (underscored), so species can be matched
#     via the first two underscore-delimited tokens.
#
# Outputs
#   - A richness GeoTIFF at:
#       Datasets/species_list/species_richness/smallest_elev_0_4.tif
#     and a second copy written to:
#       Datasets/species_list/species_richness/biodiv_dimensions_0918/
#       smallest_elev_0_4_new.tif
#
# Notes on performance
#   - The main workflow aligns rasters to disk first (avoids large in-memory stacks),
#     then sums them with terra::app() directly to a file.
#   - A "BACKUP" section at the end shows an alternative in-memory stacking approach
#     that may fail with std::bad_alloc on large batches.
#===============================================================================#


#---------------------------------------------#
# Step 1: list files
#---------------------------------------------#
# Load configuration (e.g., data_storage_path)
source(here::here("R/00_Config_file_HKH.R"))

# Load full endemism table and keep species with valid, >0 HKH containment
total_endemism_join <- read.csv(
  paste0(data_storage_path, "Datasets/species_list/species_endemism/total_species_endemism.csv")
) |>
  filter(!is.na(pct_in_HKH_area), pct_in_HKH_area > 0)

# Load DEM for HKH; used as both grid template and mask
dem_crop <- rast(paste0(data_storage_path, "Datasets/DEM_HKH/DEM_HKH.tif"))
template <- dem_crop


#---------------------------------------------#
# select species with small area and elev range
#---------------------------------------------#
# Subset definitions (here using 40% thresholds; adjust as needed)
#  - smallest_range: species with smallest total mapped area
#  - most_hkh: species with highest % of mapped area inside HKH
#  - smallest_elev: species with smallest elevational range
smallest_range <- total_endemism_join |>
  slice_min(total_area_km2, prop = 0.40)

# 20% highest % HKH (comment says 20%, prop currently 0.40)
most_hkh <- total_endemism_join |>
  slice_max(pct_in_HKH_area, prop = 0.40)

smallest_elev <- total_endemism_join |>
  slice_min(elev_range, prop = 0.40)



# check out relationships 
# Scatterplot: log10(range area) vs elevational range, colored by % in HKH
# (Requires ggplot2 + viridis; not loaded above)
x11()
ggplot(total_endemism_join, 
       aes(x = log10(total_area_km2), 
           y = elev_range, 
           color = pct_in_HKH_area)) +
  geom_point(alpha = 0.7, size = 3) +
  scale_color_viridis_c() 

# Overlap across selection criteria:
#  - First, overlap of smallest elevation range and smallest total range area
overlap_species <- intersect(smallest_elev$species, smallest_range$species)

#  - Then, overlap of all three sets (smallest elev + smallest range + most HKH)
overlap_species <- list(
  smallest_elev$species,
  smallest_range$species,
  most_hkh$species
) |> 
  reduce(intersect)

# How many?
length(overlap_species)
length(smallest_10pct_elev$species)

# Share of overlap relative to smallest_elev set
length(overlap_species) / nrow(smallest_elev) * 100

# Which ones?
overlap_species


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
target_keys <- str_replace_all(smallest_elev$species, " ", "_")
stems       <- file_path_sans_ext(basename(files))

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
  out <- file.path(outdir, paste0(file_path_sans_ext(basename(infile)), "_al.tif"))
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

#---------------------------------------------#
# Step 2: sum chunk-wise directly to disk (no RAM stack)
#---------------------------------------------#
# Richness = sum of binary layers per cell.
# Note: NAs are handled by na.rm=TRUE in app(); we do not explicitly fill NAs here.
richness_out <- file.path(
  data_storage_path, "Datasets", "species_list", "species_richness", "smallest_elev_0_4.tif"
)

if (file.exists(richness_out)) file.remove(richness_out)

# Sum aligned rasters to create richness surface; written directly to disk
richness <- app(rast(aligned_files), fun = sum, na.rm = TRUE,
                filename = richness_out, overwrite = TRUE, wopt = wopt_out)

# Turn NA -> 0 **only where the template has data**, keep outside as NA
#  - 0*template yields 0 inside HKH (where template has data) and NA outside
richness0 <- cover(richness, 0 * template)  # 0*template is 0 inside, NA outside

# (Optional) enforce the outside mask explicitly
richness_mask <- mask(richness0, template)

plot(richness_mask, colNA = "grey90")  

# Avoid plotting huge rasters in interactive sessions:
# plot(richness)  # only if you must, preferably on a small sample/aggregate
out_file <- file.path(data_storage_path, "Datasets/species_list/species_richness/biodiv_dimensions_0918/smallest_elev_0_4_new.tif")
writeRaster(richness_mask, out_file, overwrite = TRUE, datatype = "INT2U",
            gdal = c("TILED=YES", "COMPRESS=LZW", "PREDICTOR=2", "ZLEVEL=9"))














################################### BACKUP ######################################
# Alternative in-memory approach (may fail with std::bad_alloc on large batches).
# This section:
#   - Filters input rasters by target species
#   - Aligns all rasters in-memory
#   - Stacks them and computes richness as sum of layers
# Use only when the number/size of rasters is small enough for available RAM.

# sometimes gives std bad alloc 

#---------------------------------------------#
# Step 1: list files
#---------------------------------------------#
in_dir <- file.path(data_storage_path, "Datasets", "species_list", "rasterfiles")
files  <- list.files(in_dir, pattern = "\\.tif$", full.names = TRUE)
message("Found ", length(files), " rasters.")

#---------------------------------------------#
# NEW: filter only files that match target species
#---------------------------------------------#
# Example target species vector (with spaces, as in your data)
target_species <- smallest_range$species
#target_species <-overlap_species

# Convert target species to filename keys (underscores)
target_keys <- str_replace_all(target_species, " ", "_")

# Keep only rasters where filename contains one of the keys (anchored at start)
files_subset <- files[str_detect(basename(files), paste0("^(", paste(target_keys, collapse="|"), ")"))]

message("Found ", length(files_subset), " rasters for target species.")

#---------------------------------------------#
# Step 2: align each raster to DEM (CRS/res/extent)
#---------------------------------------------#
# Helper: derive a safe layer name (Genus_species) from a raster filename
species_from_file <- function(path, with_space = FALSE) {
  bn <- file_path_sans_ext(basename(path))
  parts <- strsplit(bn, "_", fixed = TRUE)[[1]]
  sp <- parts[1:min(2, length(parts))]              # first two tokens
  if (with_space) paste(sp, collapse = " ") else paste(sp, collapse = "_")
}

# Helper: align raster to template grid so layers can be stacked
#  - Reproject if CRS differs
#  - Resample to template resolution
#  - Extend to template extent
#  - Convert NA to 0 (treat NA as absence) for downstream sums
align_to_dem <- function(r, template) {
  if (!identical(crs(r), crs(template))) r <- project(r, template, method = "near")
  r <- resample(r, template, method = "near")
  r <- extend(r, template)
  r <- ifel(is.na(r), 0, r)
  r
}

# Align each raster and name its layer
ras_aligned <- lapply(files_subset, function(fp) {
  r <- rast(fp)
  r <- align_to_dem(r, dem_crop)
  names(r) <- species_from_file(fp, with_space = FALSE)  # keep safe names with "_"
  r
})

gc()

# Ensure unique names (avoids errors when stacking)
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

# Crop/mask to HKH based on template
stk <- crop(stk, template)
gc()
stk_masked <- mask(stk, template)
gc()

#---------------------------------------------#
# Step 5: richness = sum of 0/1 layers
#---------------------------------------------#
richness <- sum(stk_masked, na.rm = TRUE)

x11()
plot(richness)

out_file <- file.path(data_storage_path, "Datasets", "species_list","species_richness","smallest_range_0_4.tif")
writeRaster(richness, out_file, overwrite = TRUE, datatype = "INT2U",
            gdal = c("TILED=YES", "COMPRESS=LZW", "PREDICTOR=2", "ZLEVEL=9"))
