
#----------------------------------------------------------#
# read in global PA
#----------------------------------------------------------#

gdb <- paste0(data_storage_path, "Datasets/protected_areas/WDPA_WDOECM.gdb")

# list layers
layers <- sf::st_layers(gdb)$name

# pick the polygon layer
poly_layer <- layers[grepl("_poly_", layers, ignore.case = TRUE)]

# read multipolygon WDPA features
wdpa_poly <- vect(gdb, layer = poly_layer)

wdpa_poly

#  WDPA polygons as shapefile
# out_path <- file.path(data_storage_path, "Datasets/protected_areas/wdpa_wdpoecm_global.shp")
# writeVector(wdpa_poly, out_path, filetype = "ESRI Shapefile", overwrite = TRUE)

#----------------------------------------------------------#
# build bbox around species range 
#----------------------------------------------------------#
# 1. Get bounding box (extent) of your mammal polygon
mammal_bbox <- ext(mammal_poly)

# 2. Crop WDPA polygons to that extent
wdpa_crop <- crop(wdpa_poly, mammal_bbox)

# 3. (Optional but usually needed) Keep only PAs that actually intersect the polygon
wdpa_in_range <- wdpa_crop[mammal_poly, ]

plot(wdpa_in_range)

#----------------------------------------------------------#
# calculate protected area within each mammal range complete 
#----------------------------------------------------------#

range_km2 <- expanse(mammal_poly, unit = "km") |> sum(na.rm = TRUE)

# OVERALL PA coverage 
wdpa_union <- aggregate(wdpa_in_range)                   # merge all polygons into one (multi)part
pa_km2_overall <- expanse(wdpa_union, unit = "km") |> sum(na.rm = TRUE)
pa_cov_pct_overall <- 100 * pa_km2_overall / range_km2

#
overall <- tibble(
  metric = c("range_km2", "pa_km2_overall", "pa_coverage_pct_overall"),
  value  = c(range_km2, pa_km2_overall, pa_cov_pct_overall)
)

#--------------------------------------------------------------#
# calculate protected area within each range for each category 
#----------------------------------------------------------#
# dissolve within category first 
wdpa_by_cat <- aggregate(wdpa_in_range, "IUCN_CAT")

cat_area <- tibble(
  IUCN_CAT = wdpa_by_cat$IUCN_CAT,
  area_km2 = expanse(wdpa_by_cat, unit = "km")
) |>
  mutate(coverage_pct = 100 * area_km2 / range_km2) |>
  arrange(desc(area_km2))

# add the total 
cat_area <- cat_area |>
  bind_rows(
    cat_area |>
      summarise(
        IUCN_CAT = "TOTAL",
        area_km2 = sum(area_km2, na.rm = TRUE),
        coverage_pct = 100 * area_km2 / range_km2   # computed on summed area
      )
  )



