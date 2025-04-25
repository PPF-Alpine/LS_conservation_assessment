#----------------------------------------------------------#
# 1. Set up  -----
#----------------------------------------------------------#
library(here)
#install.packages("wdpar")
#webdriver::install_phantomjs()
library(wdpar)
library(elevatr)
library(tidyverse)
library(sf)
library(chirps)
library(leaflet)


if (!require(remotes)) install.packages("remotes")
remotes::install_github("prioritizr/prepr")

# Load configuration file
source(here::here("R/00_Config_file.R"))

#----------------------------------------------------------#
# 1. Set up  -----
#----------------------------------------------------------#

# get PAs for mountain ranges with alpine biome

#source the gmba mountains
mountain_shapes <- sf::st_read(paste(data_storage_path,"Datasets/Mountains/GMBA_Levels/Level_03/GMBA_Inventory_v2.0_Level_03.shp", sep = "/"))

# source alpine biome
alpine_shapes <- sf::st_read(paste(data_storage_path,"Datasets/Mountains/Alpine_Biome/alpine_biome.shp", 
                                   sep = "/"))

# filter mountains with alpine biome
mountain_shapes_selected <- mountain_shapes|>
  filter(MapName %in% alpine_shapes$Mntn_rn)


#----------------------------------------------------------#
# individual mountain ranges
#----------------------------------------------------------#

mountain_range <- mountain_shapes_selected|>
  filter(MapName == "North European Highlands")

# in wdpa functions PAs are downloaded via ISO

# define country codes
countries <- mountain_range$CountryISO
countries <- strsplit(countries, ",\\s*")[[1]]

  
# Function to fetch WDPA data for a given country and return as an sf object
fetch_wdpa_country <- function(country_iso) {
  message(paste("Downloading WDPA data for:", country_iso))
  wdpa_fetch(country_iso, wait = TRUE) %>%
    st_as_sf()
}

# Download and combine protected areas for all specified countries
wdpa_mountain_range <- lapply(countries, fetch_wdpa_country) %>%
  bind_rows()

#----------------------------------------------------------#
#     clean PAs
#----------------------------------------------------------#

clean_PAs <- wdpa_clean(wdpa_mountain_range) 

#----------------------------------------------------------#
#     overlap with mountain
#----------------------------------------------------------#
# Reproject 
if (!sf::st_crs(clean_PAs) == sf::st_crs(mountain_range)) {
  mountain_range <- sf::st_transform(mountain_range, sf::st_crs(clean_PAs))
}

# intersect the mountain range
intersected_geom <- clean_PAs |>
  dplyr::filter(sf::st_intersects(geometry, mountain_range, sparse = FALSE)) |>
  sf::st_union()

clean_PAs_himalaya <- sf::st_sf(geometry = intersected_geom)

x11()
plot(clean_PAs_himalaya)

clean_PAs_himalaya <- st_transform(clean_PAs_himalaya, 4326)
mountain_range <- st_transform(mountain_range, 4326)

#----------------------------------------------------------#
#    get the metadata
#----------------------------------------------------------#

PA_metadata <- clean_PAs |>
  select(WDPA_PID, WDPAID, NAME, DESIG_ENG, DESIG_TYPE, IUCN_CAT, GOV_TYPE, ISO3) |>
  sf::st_drop_geometry()

# create one shapefile with uncategorized IUCN PA and one without

#----------------------------------------------------------#
#    save as geopackage
#----------------------------------------------------------#
# Save as a GeoPackage
st_write(
  clean_PAs_himalaya,
  paste0(data_storage_path, "Datasets/protected_areas/all_mountains/clean_PAs_North_European_Highlands.shp"),
  delete_layer = TRUE,overwrite=FALSE
)


# Define file path
output_path <- paste0(data_storage_path, "Outputs/protected_areas/metadata_PA/PA_metadata_Central_Andes.csv")
write.csv(PA_metadata,output_path)







#❗ does not work yet.. 

#----------------------------------------------------------#
#   LOOP THROUGH ALL MOUNTAINS 
#----------------------------------------------------------#

# Loop through each mountain range
for (i in seq_len(nrow(mountain_shapes_selected))) {
  
  # Get single mountain range
  mountain_range <- mountain_shapes_selected[i, ]
  mountain_name <- mountain_range$MapName
  
  message(paste0("Processing: ", mountain_name, " (", i, "/", nrow(mountain_shapes_selected), ")"))
  
  # Extract and split ISO codes
  countries <- strsplit(mountain_range$CountryISO, ",\\s*")[[1]]
  
  # Function to fetch WDPA data
  fetch_wdpa_country <- function(country_iso) {
    message(paste("  Downloading WDPA data for:", country_iso))
    wdpa_fetch(country_iso, wait = TRUE) %>%
      st_as_sf()
  }
  
  # Download PAs
  wdpa_mountain_range <- tryCatch({
    lapply(countries, fetch_wdpa_country) %>%
      bind_rows()
  }, error = function(e) {
    message(paste("  ❌ Error downloading WDPA data for", mountain_name, ":", e$message))
    return(NULL)
  })
  
  # Skip if download failed
  if (is.null(wdpa_mountain_range) || nrow(wdpa_mountain_range) == 0) next
  
  # Clean
  clean_PAs <- tryCatch({
    wdpa_clean(wdpa_mountain_range)
  }, error = function(e) {
    message(paste("  ❌ Error cleaning PAs for", mountain_name, ":", e$message))
    return(NULL)
  })
  
  if (is.null(clean_PAs) || nrow(clean_PAs) == 0) next
  
  # Reproject if needed
  if (!sf::st_crs(clean_PAs) == sf::st_crs(mountain_range)) {
    mountain_range <- sf::st_transform(mountain_range, sf::st_crs(clean_PAs))
  }
  
  # Intersect with mountain range
  clean_PAs_overlap <- dplyr::filter(clean_PAs, st_intersects(geometry, mountain_range, sparse = FALSE))
  
  if (nrow(clean_PAs_overlap) == 0) {
    message(paste("  ⚠️ No overlapping PAs for", mountain_name))
    next
  }
  
  # Final transform to WGS84
  clean_PAs_overlap <- st_transform(clean_PAs_overlap, 4326)
  mountain_range <- st_transform(mountain_range, 4326)
  
  # Create safe filename
  file_name <- paste0("clean_PAs_", str_replace_all(tolower(mountain_name), "\\s+", "_"), ".gpkg")
  
  # Save
  tryCatch({
    st_write(
      clean_PAs_overlap,
      paste0(data_storage_path, "Datasets/protected_areas/all_mountains/", file_name),
      delete_layer = TRUE,
      quiet = TRUE
    )
    message(paste("  ✅ Saved:", file_name))
  }, error = function(e) {
    message(paste("  ❌ Error saving", file_name, ":", e$message))
  })
}
