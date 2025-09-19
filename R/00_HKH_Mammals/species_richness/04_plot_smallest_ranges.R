library(terra)
library(stringr)
library(terra)
library(sf)
library(stringr)
library(purrr)
library(dplyr)
library(tibble)
library(tools)
library(stringr)

#---------------------------------------------#
# Step 1: list files
#---------------------------------------------#
source(here::here("R/00_Config_file_HKH.R"))

total_endemism_join <- read.csv(paste0(data_storage_path,"Datasets/species_list/species_endemism/total_species_endemism.csv"))|>
  filter(!is.na(pct_in_HKH_area), pct_in_HKH_area > 0)

dem_crop <- rast(paste0(data_storage_path, "Datasets/DEM_HKH/DEM_HKH.tif"))
template<-dem_crop

#---------------------------------------------#
# select species with small area and elev range
#---------------------------------------------#
smallest_range <- total_endemism_join %>%
  slice_min(total_area_km2, prop = 0.40)

# 20% highest % HKH
most_hkh <- total_endemism_join %>%
  slice_max(pct_in_HKH_area, prop = 0.40)

smallest_elev <- total_endemism_join %>%
  slice_min(elev_range, prop = 0.40)


x11()
ggplot(total_endemism_join, 
       aes(x = log10(total_area_km2), 
           y = elev_range, 
           color = pct_in_HKH_area)) +
  geom_point(alpha = 0.7, size = 3) +
  scale_color_viridis_c() 

overlap_species <- intersect(smallest_elev$species, smallest_range$species)

overlap_species <- list(
  smallest_elev$species,
  smallest_range$species,
  most_hkh$species
) %>% 
  reduce(intersect)

# How many?
length(overlap_species)
length(smallest_10pct_elev$species)

length(overlap_species) / nrow(smallest_elev) * 100

# Which ones?
overlap_species


#---------------------------------------------#
# Setup: template & options
#---------------------------------------------#
template <- dem_crop  # your aligned DEM / target grid

terraOptions(
  memfrac = 0.6,            # allow terra to manage RAM
  progress = 1
)

wopt_in  <- list(datatype="INT1U", gdal=c("TILED=YES","COMPRESS=LZW","ZLEVEL=6"))
wopt_out <- list(datatype="INT2U", gdal=c("TILED=YES","COMPRESS=LZW","PREDICTOR=2","ZLEVEL=9"))

in_dir  <- file.path(data_storage_path, "Datasets", "species_list", "rasterfiles")
files   <- list.files(in_dir, pattern="\\.tif$", full.names=TRUE)

# Filter by species names without a giant regex
target_keys <- str_replace_all(most_hkh$species, " ", "_")
stems       <- file_path_sans_ext(basename(files))
files_subset <- files[str_detect(basename(files), paste0("^(", paste(target_keys, collapse="|"), ")"))]

message("Found ", length(files_subset), " rasters for target species.")

#---------------------------------------------#
# Function: align one raster to template, write to disk
# (crop early -> project if needed -> resample)
#---------------------------------------------#
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
tmp_aligned_dir <- file.path(tempdir(), "aligned_bin")
dir.create(tmp_aligned_dir, showWarnings = FALSE)

aligned_files <- vapply(files_subset, align_write, character(1), outdir=tmp_aligned_dir)

#---------------------------------------------#
# Step 2: sum chunk-wise directly to disk (no RAM stack)
#---------------------------------------------#
# Note: we do not fill NAs; app(..., na.rm=TRUE) handles them.
richness_out <- file.path(
  data_storage_path, "Datasets", "species_list", "species_richness", "smallelev_0_4.tif"
)

if (file.exists(richness_out)) file.remove(richness_out)

# Sum as you do
richness <- app(rast(aligned_files), fun = sum, na.rm = TRUE,
                filename = richness_out, overwrite = TRUE, wopt = wopt_out)

# Turn NA -> 0 **only where the template has data**, keep outside as NA
richness0 <- cover(richness, 0 * template)  # 0*template is 0 inside, NA outside

# (Optional) enforce the outside mask explicitly
richness_mask <- mask(richness0, template)

plot(richness_mask, colNA = "grey90")  

# Avoid plotting huge rasters in interactive sessions:
# plot(richness)  # only if you must, preferably on a small sample/aggregate
out_file <- file.path(data_storage_path, "Datasets/species_list/species_richness/biodiv_dimensions_0918/smallelev_0_4.tif")
writeRaster(richness_mask, out_file, overwrite = TRUE, datatype = "INT2U",
            gdal = c("TILED=YES", "COMPRESS=LZW", "PREDICTOR=2", "ZLEVEL=9"))


################################### BACKUP ######################################

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

# Keep only rasters where filename contains one of the keys
files_subset <- files[str_detect(basename(files), paste0("^(", paste(target_keys, collapse="|"), ")"))]

message("Found ", length(files_subset), " rasters for target species.")

#---------------------------------------------#
# Step 2: align each raster to DEM (CRS/res/extent)
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

ras_aligned <- lapply(files_subset, function(fp) {
  r <- rast(fp)
  r <- align_to_dem(r, dem_crop)
  names(r) <- species_from_file(fp, with_space = FALSE)  # keep safe names with "_"
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