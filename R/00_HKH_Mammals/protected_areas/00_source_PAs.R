
library(terra)
library(sf)
library(sf)
library(dplyr)
library(units)

# Load configuration file
source(here::here("R/00_Config_file_HKH.R"))

#----------------------------------------------------------#
# 0. load files  -----
#----------------------------------------------------------#

# load files 
# dem 
# lc 
# species list 
# ecoregions

hkh_boundary <- sf::st_read(paste0(data_storage_path,"Datasets/HKH_boundary/HKH_boundary.shp"))

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
# build bbox around HKH region 
#----------------------------------------------------------#
# 1. Get bounding box (extent) of your mammal polygon
hkh_bbox <- ext(hkh_boundary)

# 2. Crop WDPA polygons to that extent
wdpa_crop <- crop(wdpa_poly, hkh_bbox)
plot(wdpa_crop)

wdpa_sf <- sf::st_as_sf(wdpa_crop)                 # SpatVector -> sf
wdpa_sf <- sf::st_transform(wdpa_sf, sf::st_crs(hkh_boundary))


# 3. (Optional but usually needed) Keep only PAs that actually intersect the polygon
wdpa_in_range <- terra::intersect(wdpa_crop, hkh_boundary)


wdpa_crop_sf  <- st_crop(wdpa_sf, st_bbox(hkh_boundary))   # optional speed-up
plot(wdpa_crop_sf$geometry)

wdpa_crop_sf  <- make_shapes_valid(wdpa_crop_sf)

wdpa_in_range <- st_intersection(wdpa_crop_sf, hkh_boundary)
plot(wdpa_in_range$geometry)
library(sf)
out_gpkg <- file.path(data_storage_path, "Datasets/protected_areas/PA_HKH/pa_in_hkh.gpkg")

st_write(wdpa_in_range, out_gpkg,
         layer = "pa_in_hkh",
         delete_dsn = TRUE,             # overwrite file if it exists
         layer_options = c("SPATIAL_INDEX=YES"))


#----------------------------------------------------------#
# calculate coverage
#----------------------------------------------------------#

# )
ea_crs <- 6933  # World Cylindrical Equal Area (EPSG:6933)

hkh_sf  <- make_shapes_valid(hkh_boundary) |> 
  st_transform(ea_crs)
wdpa_sf <- make_shapes_valid(wdpa_in_range) |> 
  st_transform(ea_crs)

#HKH area in km^2
range_km2 <- st_area(hkh_sf) |> set_units("km^2") |> sum(na.rm = TRUE)

# 3) OVERALL PA coverage 
wdpa_union <- st_union(wdpa_sf)                    # single multipart geometry
pa_km2_overall <- st_area(wdpa_union) |> 
  set_units("km^2") |> as.numeric()

range_km2_num  <- as.numeric(range_km2)

pa_cov_pct_overall <- 100 * pa_km2_overall / range_km2_num

overall <- tibble::tibble(
  metric = c("range_km2", "pa_km2_overall", "pa_coverage_pct_overall"),
  value  = c(range_km2_num, pa_km2_overall, pa_cov_pct_overall)
)

#----------------------------------------------------------#
# calculate coverage
#----------------------------------------------------------#
# 
wdpa_by_cat <- wdpa_sf |>
  group_by(IUCN_CAT) |>
  summarise(geom = st_union(geometry), .groups = "drop") |>
  st_as_sf() 


cat_area <- wdpa_by_cat|>
  mutate(area_km2 = st_area(geom)|> 
           set_units("km^2")|> 
           drop_units())|>
  transmute(IUCN_CAT,
            area_km2,
            coverage_pct = 100 * area_km2 / range_km2_num)|>
  arrange(desc(area_km2))

cat_area <- cat_area|>
  bind_rows(
    cat_area|>
      summarise(
        IUCN_CAT = "TOTAL",
        area_km2 = sum(area_km2, na.rm = TRUE),
        coverage_pct = 100 * sum(area_km2, na.rm = TRUE) / range_km2_num
      )
  )


