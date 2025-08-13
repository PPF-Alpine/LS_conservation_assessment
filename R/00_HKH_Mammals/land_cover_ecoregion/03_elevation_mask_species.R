#----------------------------------------------------------#
# 4. elevation mask for mammal distribution  -----
#----------------------------------------------------------#
sciname_elev <- species_list|>
  filter(sciname == target_sciname)|>
  select(sciname, average_min_elevation, average_max_elevation)|>
  distinct()

elev_min <- as.numeric(sciname_elev$average_min_elevation[1])
elev_max <- as.numeric(sciname_elev$average_max_elevation[1])


if (!compareGeom(mammal_raster, dem_crop, stopOnError = FALSE)) {
  # Reproject if CRS differs, then resample to the mammal grid
  if (!identical(crs(mammal_raster), crs(dem_crop))) {
    dem_crop <- project(dem_crop, mammal_raster)
  }
  dem_crop <- resample(dem_crop, mammal_raster, method = "bilinear")
}

# build elevation "outside" mask 
outside <- is.na(dem_crop) | dem_crop < elev_min | dem_crop > elev_max

# zero-out presence where outside the elevation range 
mammal_elev_masked <- ifel((mammal_raster > 0) & outside, 0, mammal_raster)

# view
plot(mammal_elev_masked)


mammal_elev_masked[mammal_elev_masked != 1] <- NA