#----------------------------------------------------------#
# ♻️LOOP -----
#------------------------------------------------

# -------------------------
# 0) Utilities
# -------------------------
safe_name <- function(x) stringr::str_replace_all(x, "[^A-Za-z0-9_]+", "_")

build_outdir <- function(data_storage_path) {
  out_dir <- file.path(data_storage_path, "Datasets", "species_list", "rasterfiles")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_dir
}

# -------------------------
# 1) Get species geometry
# -------------------------
get_species_geom <- function(mammals_multi, sciname) {
  m <- mammals_multi %>% filter(sciname == !!sciname)
  if (nrow(m) == 0) stop("No geometry found for species: ", sciname)
  m
}

# -------------------------
# 2) Crop DEM to species extent
# -------------------------
crop_dem_to_species <- function(dem, species_geom) {
  terra::crop(dem, species_geom)
}

# -------------------------
# 3) Rasterize species polygon to presence/absence (1/0)
# -------------------------
rasterize_species <- function(species_geom, dem_crop) {
  species_geom$val <- 1
  r <- terra::rasterize(species_geom, dem_crop, field = "val",
                        background = 0, touches = TRUE)
  # keep only the species footprint (remove extra zeros)
  terra::mask(r, species_geom)
}

# -------------------------
# 4) Get species elevation limits
# -------------------------
get_elev_limits <- function(species_list, sciname) {
  row <- species_list %>%
    filter(sciname == !!sciname) %>%
    distinct(sciname, average_min_elevation, average_max_elevation) %>%
    slice(1)
  if (nrow(row) == 0) stop("No elevation limits found for species: ", sciname)
  
  list(
    min = as.numeric(row$average_min_elevation),
    max = as.numeric(row$average_max_elevation)
  )
}

# -------------------------
# 5) Align rasters (project/resample dem_crop to presence grid if needed)
# -------------------------
align_dem_to_reference <- function(dem_crop, ref_raster) {
  if (!terra::compareGeom(ref_raster, dem_crop, stopOnError = FALSE)) {
    if (!identical(terra::crs(ref_raster), terra::crs(dem_crop))) {
      dem_crop <- terra::project(dem_crop, ref_raster)
    }
    dem_crop <- terra::resample(dem_crop, ref_raster, method = "bilinear")
  }
  dem_crop
}

# -------------------------
# 6) Apply elevation mask to presence raster
# -------------------------
apply_elevation_mask <- function(presence_raster, dem_crop, elev_min, elev_max) {
  outside <- is.na(dem_crop) | dem_crop < elev_min | dem_crop > elev_max
  masked  <- terra::ifel((presence_raster > 0) & outside, 0, presence_raster)
  masked[masked != 1] <- NA   # keep only presence cells
  masked
}

# -------------------------
# 7) Save output raster
# -------------------------
save_species_raster <- function(r, out_dir, sciname) {
  fp <- file.path(out_dir, paste0(safe_name(sciname), "_elev_masked.tif"))
  terra::writeRaster(r, fp, overwrite = TRUE)
  fp
}

# -------------------------
# Driver: one species end-to-end
# -------------------------
process_species <- function(sciname, mammals_multi, species_list, dem, out_dir) {
  message("Processing: ", sciname)
  
  geom        <- get_species_geom(mammals_multi, sciname)
  dem_crop    <- crop_dem_to_species(dem, geom)
  pres_raster <- rasterize_species(geom, dem_crop)
  
  lims        <- get_elev_limits(species_list, sciname)
  dem_aligned <- align_dem_to_reference(dem_crop, pres_raster)
  
  out_raster  <- apply_elevation_mask(pres_raster, dem_aligned, lims$min, lims$max)
  save_species_raster(out_raster, out_dir, sciname)
}

# -------------------------
# Batch runner (with error log)
# -------------------------
run_all_species <- function(mammals_multi, species_list, dem, data_storage_path) {
  out_dir   <- build_outdir(data_storage_path)
  scinames  <- unique(species_list$sciname)
  outputs   <- vector("list", length(scinames))
  error_log <- tibble(sciname = character(), error_msg = character())
  
  for (i in seq_along(scinames)) {
    sci <- scinames[i]
    tryCatch({
      outputs[[i]] <- process_species(sci, mammals_multi, species_list, dem, out_dir)
      message("  ✔ Done: ", sci)
    }, error = function(e) {
      msg <- conditionMessage(e)
      message("  ✖ Error: ", sci, " — ", msg)
      error_log <<- bind_rows(error_log, tibble(sciname = sci, error_msg = msg))
      outputs[[i]] <<- NA_character_
    })
  }
  
  list(files = unlist(outputs), errors = error_log, out_dir = out_dir)
}


