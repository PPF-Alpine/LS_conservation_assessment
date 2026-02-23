
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


species_list <- readxl::read_excel(paste0(data_storage_path,"Datasets/species_list/assessment_hkh_mammals_09082025_LS.xlsx"))

#----------------------------------------------------------#
# Download mammal range from MDD  -----
#----------------------------------------------------------#

target_sciname <- "Herpestes javanicus"
mammal <- get_mdd_map(target_sciname)

plot(mammal)

#----------------------------------------------------------#
# test function  -----
#----------------------------------------------------------#

all_sciname <- unique(species_list$sciname)

# test
all_sciname <- all_sciname[451:579]

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

# 
error_log  # contains sciname + error messages



#----------------------------------------------------------#
# if there are errors: run this:   -----
#----------------------------------------------------------#

# Safe union function that returns NA geometry if both attempts fail
safe_union <- function(g) {
  tryCatch(
    {
      st_make_valid(st_union(g))
    },
    error = function(e1) {
      tryCatch(
        st_make_valid(st_buffer(g, 0)),
        error = function(e2) {
          # return NA geometry if both fail
          st_sfc(st_geometrycollection(), crs = st_crs(g))
        }
      )
    }
  )
}

# Run pipeline
mammals_multi <- mammals_raw %>%
  sf::st_as_sf() %>%
  sf::st_transform(4326) %>%
  sf::st_collection_extract("POLYGON", warn = FALSE) %>%
  group_by(sciname) %>%
  summarise(geometry = safe_union(geometry), .groups = "drop") %>%
  st_cast("MULTIPOLYGON")

# Identify failures
failed_species <- mammals_multi %>%
  filter(st_is_empty(geometry)) %>%
  pull(sciname)

if (length(failed_species) > 0) {
  message("⚠️ The following species failed during union and were skipped: ",
          paste(failed_species, collapse = ", "))
}

# Keep only successful geometries
mammals_multi <- mammals_multi %>%
  filter(!st_is_empty(geometry))



#----------------------------------------------------------#
# write as gpkg  -----
#----------------------------------------------------------#
sf::st_write(
  mammals_multi,
  paste0(data_storage_path, "Datasets/species_list/hkh_mammals.gpkg"),
  layer = "hkh_mammals",
  delete_layer = TRUE
)


