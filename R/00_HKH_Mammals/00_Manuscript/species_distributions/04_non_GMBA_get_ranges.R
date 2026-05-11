#----------------------------------------------------------#
# 0. Set up  -----
#----------------------------------------------------------#
library(here)
#devtools::install_github("alrobles/mdd")
library(mdd)
library(terra) # for plot
library(purrr)
library(tidyverse)
library(sf)
library(units
        )

# Load configuration file
source(here::here("R/00_Config_file_HKH.R"))

#----------------------------------------------------------#
# get full distribution ranges for the missing species  -----
#----------------------------------------------------------#
species_list <- readxl::read_excel(paste0(data_storage_path,"Datasets/species_list/HKH_non_GMBA_species.xlsx"))


#----------------------------------------------------------#
# test function  -----
#----------------------------------------------------------#

all_sciname <- unique(species_list$sciname)

# test : all_sciname <- all_sciname[451:579]

# record potential errors
error_log <- tibble(sciname = character(), error_msg = character())

# fetch the shps for all species
fetch_one <- function(sci) {
  message("Processing species: ", sci)
  tryCatch({
    x <- get_mdd_map(sci)
    x$sciname <- sci
    message("  ✔ Successfully processed: ", sci)
    return(x)
  }, error = function(e) {
    msg <- conditionMessage(e)
    message("  ✖ Error for species: ", sci, " — ", msg)
    # --- NEW: record error ---
    error_log <<- bind_rows(error_log, tibble(sciname = sci, error_msg = msg))
    return(NULL)
  })
}

# combine 
mammals_raw <- map(all_sciname, ~tryCatch(fetch_one(.x), error = function(e) NULL)) |>
  purrr::compact() |>
  do.call(rbind, args = _)

# harmonize geometry & dissolve by species -> MULTIPOLYGON 
mammals_multi <- mammals_raw %>%
  terra::makeValid() %>%
  sf::st_as_sf() %>%
  sf::st_transform(4326) %>%
  sf::st_collection_extract("POLYGON", warn = FALSE) %>%
  dplyr::group_by(sciname) %>%
  dplyr::summarise(geometry = sf::st_make_valid(st_union(geometry)), .groups = "drop") %>%
  sf::st_cast("MULTIPOLYGON")

plot(mammals_multi)

#----------------------------------------------------------#
# write as gpkg  -----
#----------------------------------------------------------#
sf::st_write(
  mammals_multi,
  paste0(data_storage_path, "Datasets/species_list/hkh_mammals_missing.gpkg"),
  layer = "hkh_mammals",
  delete_layer = TRUE
)

#----------------------------------------------------------#
# calculate overlap with HKH 
#----------------------------------------------------------# 

library(sf)
library(dplyr)
library(units)

hkh_boundaries <- sf::st_read(
  "C:/Users/losch5089/OneDrive - University of Bergen/Desktop/ICIMOD_work/HKH_Boundary/HKH_Boundary.shp"
)

plot(hkh_boundaries)

sf_use_s2(FALSE)

# make CRS match
mammals_multi <- st_transform(mammals_multi, st_crs(hkh_boundaries))

# keep only species overlapping HKH
mammals_multi_hkh <- mammals_multi |>
  st_make_valid() |>
  filter(lengths(st_intersects(geometry, hkh_boundaries)) > 0)

# total range area of those species
mammals_multi_hkh$range_km2 <- set_units(
  st_area(mammals_multi_hkh),
  km^2
)

range_area <- mammals_multi_hkh |>
  st_drop_geometry() |>
  group_by(sciname) |>
  summarise(
    total_range_km2 = sum(as.numeric(range_km2)),
    .groups = "drop"
  )

# intersect ranges with HKH area
mammals_overlap <- st_intersection(
  mammals_multi_hkh,
  hkh_boundaries
)

# calculate overlap area in km2
mammals_overlap$overlap_km2 <- set_units(
  st_area(mammals_overlap),
  km^2
)

# summarise overlap area per species + percentage
species_overlap_area <- mammals_overlap |>
  st_drop_geometry() |>
  group_by(sciname) |>
  summarise(
    overlap_km2 = sum(as.numeric(overlap_km2)),
    .groups = "drop"
  ) |>
  left_join(range_area, by = "sciname") |>
  mutate(
    percent_overlap_hkh = (overlap_km2 / total_range_km2) * 100
  ) |>
  arrange(desc(overlap_km2))

species_overlap_filtered <- species_overlap_area|>
  filter(percent_overlap_hkh > 9)

#----------------------------------------------------------#
# calculate overlap with HKH 
#----------------------------------------------------------# 
mammals_multi_filter <- species_overlap_filtered |>
  left_join(
    mammals_multi,
    by = "sciname"
  )

sf::st_write(
  mammals_multi_filter,
  paste0(data_storage_path, "Datasets/species_list/hkh_mammals_missing_filtered.gpkg"),
  layer = "hkh_mammals_missing_filtered",
  delete_layer = TRUE
)

# save as excel
writexl::write_xlsx(
  mammals_multi_filter,
  path = file.path(
    data_storage_path,
    "Datasets/species_list/hkh_mammals_missing_filtered.xlsx"
  )
)
