
library(terra)
library(sf)
library(stringr)
library(purrr)
library(dplyr)
library(tibble)


# ❗ rewrite config file for both datastorage paths
source(here::here("R/00_Config_file.R"))
dem <- rast(paste0(data_storage_path, "Datasets/Mountains/DEM/GMTED2010_30.tiff"))

source(here::here("R/00_Config_file_HKH.R"))

#----------------------------------------------------------#
# ♻ RUN LOOP FOR ALL SPECIES -----
#------------------------------------------------

# functions in function folder (read in withconfig file)

species_list <- species_list |>
  filter(sciname %in% mammals_multi$sciname)

# run the functions for all species 
res <- run_all_species(mammals_multi, species_list, dem, data_storage_path)
res$out_dir   # folder where rasters were saved
res$errors    # any species that failed, with messages





#----------------------------------------------------------#
# ♻1️⃣FOR INDIVIDUAL SPECIES -----
#------------------------------------------------
#----------------------------------------------------------#
#  rasterize species distribution  -----
#----------------------------------------------------------#
all_sciname <- unique(species_list$sciname)

# mammals_multi: sf with columns sciname + MULTIPOLYGON
# dem: SpatRaster (template grid)

target_sciname <- "Anourosorex squamipes"

mammal <- mammals_multi|>
  filter(sciname==target_sciname)
plot(mammal$geometry)

# crop dem to mammal extent 
dem_sci_crop <- crop(dem,mammal)
plot(dem_sci_crop)

# set the value to 1
mammal$val <- 1

# rasterize
mammal_raster <- rasterize(mammal, dem_sci_crop, field = "val", background = 0, touches = TRUE)

# crop to remove extra 0s
mammal_raster <- mask(mammal_raster, mammal)
plot(mammal_raster)

#----------------------------------------------------------#
# elevation mask for mammal distribution  -----
#----------------------------------------------------------#
sciname_elev <- species_list|>
  filter(sciname == target_sciname)|>
  select(sciname, average_min_elevation, average_max_elevation)|>
  distinct()

elev_min <- as.numeric(sciname_elev$average_min_elevation[1])
elev_max <- as.numeric(sciname_elev$average_max_elevation[1])


if (!compareGeom(mammal_raster, dem_sci_crop, stopOnError = FALSE)) {
  # Reproject if CRS differs, then resample to the mammal grid
  if (!identical(crs(mammal_raster), crs(dem_sci_crop))) {
    dem_sci_crop <- project(dem_sci_crop, mammal_raster)
  }
  dem_sci_crop <- resample(dem_sci_crop, mammal_raster, method = "bilinear")
}

# build elevation "outside" mask 
outside <- is.na(dem_sci_crop) | dem_sci_crop < elev_min | dem_sci_crop > elev_max

# zero-out presence where outside the elevation range 
mammal_elev_masked <- ifel((mammal_raster > 0) & outside, 0, mammal_raster)

#
mammal_elev_masked[mammal_elev_masked != 1] <- NA

# view
plot(mammal_elev_masked)


#----------------------------------------------------------#
# elevation mask for mammal distribution  -----
#------------------------------------------------
# save file 
writeRaster(mammal_elev_masked, file.path(data_storage_path, "Datasets", "species_list", paste0(gsub(" ", "_", target_sciname), "_elev_masked.tif")), overwrite = TRUE)

