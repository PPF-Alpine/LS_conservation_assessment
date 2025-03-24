
#----------------------------------------------------------#
# 1. Set up  -----
#----------------------------------------------------------#
library(here)
install.packages("wdpar")
webdriver::install_phantomjs()
library(wdpar)
library(elevatr)
library(tidyverse)
library(sf)
library(chirps)
library(leaflet)




devtools::install_github("GMBA-biodiversity/gmbaR")

# Load configuration file
source(here::here("R/00_Config_file.R"))

#----------------------------------------------------------#
#     Load protected areas data for certain countries
#----------------------------------------------------------#

Sys.setenv(CHROMOTE_CHROME = "C:/Program Files/Google/Chrome/Application/chrome.exe")

# For individual countries

wdpa_query <- wdpa_fetch("NPL")

wdpa_query

# For all countries for Himalaya region
# Define country ISO codes 
countries <- c("BTN", "NPL", "IND", "CHN", "AFG", "PAK")

# Function to fetch WDPA data for a given country and return as an sf object
fetch_wdpa_country <- function(country_iso) {
  message(paste("Downloading WDPA data for:", country_iso))
  wdpa_fetch(country_iso, wait = TRUE) %>%
    st_as_sf()
}

# Download and combine protected areas for all specified countries
wdpa_himalaya <- lapply(countries, fetch_wdpa_country) %>%
  bind_rows()



#source the gmba mountains
mountain_shapes <- sf::st_read(paste(data_storage_path,"Datasets/Mountains/GMBA_Levels/Level_03/GMBA_Inventory_v2.0_Level_03.shp", 
                                     sep = "/"))

mountain_range <- mountain_shapes|>
  filter(MapName=="Himalaya")

#----------------------------------------------------------#
#     clean PAs
#----------------------------------------------------------#

clean_PAs <- wdpa_clean(wdpa_himalaya) 

#----------------------------------------------------------#
#     overlap with mountain
#----------------------------------------------------------#

# Reproject 
if (!sf::st_crs(clean_PAs) == sf::st_crs(mountain_range)) {
  mountain_range <- sf::st_transform(mountain_range, sf::st_crs(clean_PAs))
}

# intersect the mountain range
clean_PAs_himalaya <- clean_PAs |> 
  dplyr::filter(sf::st_intersects(geometry, mountain_range, sparse = FALSE))

clean_PAs_himalaya <- st_transform(clean_PAs_himalaya, 4326)
mountain_range <- st_transform(mountain_range, 4326)

#----------------------------------------------------------#
#   visualize in leaflet 
#----------------------------------------------------------#

leaflet() |>
  addProviderTiles("CartoDB.Positron") |>  # clean basemap
  
  # Add Protected Areas (in green)
  addPolygons(data = clean_PAs_himalaya,
              color = "#228B22", weight = 1, opacity = 0.8, fillOpacity = 0.3,
              label = ~NAME, group = "Protected Areas") |>
  
  # Add Mountain Range (in red)
  addPolygons(data = mountain_range,
              color = "#FF4500", weight = 2, opacity = 0.9, fillOpacity = 0.2,
              label = ~MapName, group = "Mountain Range") |>
  
  # Add layers control to toggle visibility
  addLayersControl(
    overlayGroups = c("Protected Areas", "Mountain Range"),
    options = layersControlOptions(collapsed = FALSE)
  )


# Save as a GeoPackage
st_write(
  wdpa_himalaya,
  paste0(data_storage_path, "Datasets/protected_areas/Himalaya/wdpa_himalaya.gpkg"),
  delete_layer = TRUE
)

