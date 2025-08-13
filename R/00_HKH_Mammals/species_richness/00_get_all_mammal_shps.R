
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

target_sciname <- "Marmota himalayana"
mammal <- get_mdd_map(target_sciname)

plot(mammal)

#----------------------------------------------------------#
# test function  -----
#----------------------------------------------------------#

all_sciname <- unique(species_list$sciname)

# test
all_sciname <- all_sciname[11:15]

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
  dplyr::summarise(geometry = sf::st_union(geometry), .groups = "drop") %>%
  sf::st_cast("MULTIPOLYGON")

plot(mammals_multi)

# 
error_log  # contains sciname + error messages

#----------------------------------------------------------#
# write as gpkg  -----
#----------------------------------------------------------#
sf::st_write(
  mammals_multi,
  paste0(data_storage_path, "Datasets/species_list/hkh_mammals.gpkg"),
  layer = "hkh_mammals",
  delete_layer = TRUE
)


