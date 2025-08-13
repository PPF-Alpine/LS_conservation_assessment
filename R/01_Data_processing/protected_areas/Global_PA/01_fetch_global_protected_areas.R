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
library(prepr)

#if (!require(remotes)) install.packages("remotes")

temp_lib <- "C:/Users/losch5089/Rlibs"

dir.create(temp_lib, recursive = TRUE, showWarnings = FALSE)
remotes::install_github("prioritizr/prepr", lib = temp_lib)

library(prepr, lib.loc = temp_lib)


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
  filter(MapName == "Pacific Coast Ranges")

mountain_name <- unique(mountain_range$MapName)

# in wdpa functions PAs are downloaded via ISO

# define country codes
countries <- mountain_range$CountryISO
countries <- strsplit(countries, ",\\s*")[[1]]

#countries <- c("ARM","AZE","GEO","RUS","TUR")
  
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

# intersect with mountain range for each category
intersected_PAs <- clean_PAs |>
  filter(sf::st_intersects(geometry, mountain_range, sparse = FALSE))

# previous gave error with intersection. this works..
intersected_PAs <- clean_PAs |>
  filter(apply(sf::st_intersects(geometry, mountain_range, sparse = FALSE), 1, any))

#----------------------------------------------------------#
#    get all the metadata
#----------------------------------------------------------#

PA_metadata <- intersected_PAs |>
  select(WDPA_PID, WDPAID, NAME, DESIG_ENG, DESIG_TYPE, IUCN_CAT, GOV_TYPE, ISO3) |>
  sf::st_drop_geometry()


#----------------------------------------------------------#
#     divide into IUCN categories
#----------------------------------------------------------#

# divide shapes 
IUCN_I_IV <- intersected_PAs |>
  filter(IUCN_CAT %in% c("I", "Ia", "Ib", "II", "III", "IV")) |>
  mutate(IUCN_category = "I_IV")

IUCN_V_VI <- intersected_PAs |>
  filter(IUCN_CAT %in% c("V", "VI")) |>
  mutate(IUCN_category = "V_VI")

IUCN_NA <- intersected_PAs |>
  filter(IUCN_CAT %in% c("Not Applicable", "Not Reported")) |>
  mutate(IUCN_category = "NA")

# Combine all categories into one data frame
combined_PAs <- bind_rows(IUCN_I_IV, IUCN_V_VI, IUCN_NA)


# dissolve geometries within each category
final_multipolygons <- combined_PAs |>
  group_by(IUCN_category) |>
  summarize(geometry = st_union(geometry), .groups = "drop")

x11()
ggplot(final_multipolygons) +
  geom_sf(aes(fill = IUCN_category), color = "black", size = 0.2) +
  scale_fill_viridis_d(option = "C") + # optional, nice discrete color scale
  theme_minimal() +
  labs(fill = "IUCN Category",
       title = "Protected Areas by IUCN Category",
       x = "Longitude",
       y = "Latitude")


final_multipolygons <- st_transform(final_multipolygons, 4326)
mountain_range <- st_transform(mountain_range, 4326)

#----------------------------------------------------------#
#    save as shp
#----------------------------------------------------------#
# Save as a GeoPackage
# Replace spaces with underscores
mountain_name_clean <- gsub(" ", "_", mountain_name)
#mountain_name_clean <- gsub(" ", "_", mountain_name[1])

# Then use the cleaned name in your st_write
st_write(
  final_multipolygons,
  paste0(data_storage_path, "Datasets/protected_areas/all_mountains/clean_PAs_", mountain_name_clean, ".shp"),
  delete_layer = TRUE,
  overwrite = FALSE
)


# Define file path
output_path <- paste0(data_storage_path, "Outputs/protected_areas/metadata_PA/PA_metadata_",mountain_name_clean,".csv")
write.csv(PA_metadata,output_path)


#----------------------------------------------------------#
#   LOOP THROUGH ALL MOUNTAINS 
#----------------------------------------------------------#

# Your mountain_selection vector
mountain_selection <- c(#"Himalaya", ✅
  #"Northern Andes", ✅
  #"Central Andes", ✅
  #"Central European Highlands", ✅
  #"Intermountain West", ✅
  #"Hindu Kush", ✅
  #"Ethiopian Highlands", ✅
  #"Albertine Rift Mountains", # ✅
  #"South Island", ✅
  #"Tibetan Plateau",❗check separately 
  #"Great Escarpment",✅
  #"Malay Archipelago",✅
  #"Caucasus Mountains", # ✅ done separately because ISO XUZ didnt work
  #"East European Highlands",✅
  #"Rocky Mountains",# ✅ 
  #"Pacific Coast Ranges",# ❗check separately 
  #"Eastern Rift mountains",✅
  #"Mexican Highlands"✅
  )

#  loop
for (mountain in mountain_selection) {
  cat("\nProcessing:", mountain, "\n")
  
  # Filter mountain shape
  mountain_range <- mountain_shapes_selected %>%
    filter(MapName == mountain)
  
  # Skip if mountain not found
  if (nrow(mountain_range) == 0) {
    warning(paste("No shape found for mountain:", mountain))
    next
  }
  
  mountain_name <- mountain_range$MapName
  countries <- mountain_range$CountryISO
  countries <- strsplit(countries, ",\\s*")[[1]]
  
  # Fetch WDPA data for each country
  wdpa_mountain_range <- lapply(countries, fetch_wdpa_country) %>%
    bind_rows()
  
  # Clean PAs
  clean_PAs <- wdpa_clean(wdpa_mountain_range)
  
  # Reproject if necessary
  if (!st_crs(clean_PAs) == st_crs(mountain_range)) {
    mountain_range <- st_transform(mountain_range, st_crs(clean_PAs))
  }
  
  # Intersect
  intersected_PAs <- clean_PAs %>%
    filter(st_intersects(geometry, mountain_range, sparse = FALSE))
  
  # Extract metadata
  PA_metadata <- intersected_PAs %>%
    select(WDPA_PID, WDPAID, NAME, DESIG_ENG, DESIG_TYPE, IUCN_CAT, GOV_TYPE, ISO3) %>%
    st_drop_geometry()
  
  # Split by IUCN category
  IUCN_I_IV <- intersected_PAs %>%
    filter(IUCN_CAT %in% c("I", "Ia", "Ib", "II", "III", "IV")) %>%
    mutate(IUCN_category = "I_IV")
  
  IUCN_V_VI <- intersected_PAs %>%
    filter(IUCN_CAT %in% c("V", "VI")) %>%
    mutate(IUCN_category = "V_VI")
  
  IUCN_NA <- intersected_PAs %>%
    filter(IUCN_CAT %in% c("Not Applicable", "Not Reported")) %>%
    mutate(IUCN_category = "NA")
  
  combined_PAs <- bind_rows(IUCN_I_IV, IUCN_V_VI, IUCN_NA)
  
  final_multipolygons <- combined_PAs %>%
    group_by(IUCN_category) %>%
    summarize(geometry = st_union(geometry), .groups = "drop")
  
  
  # Transform to WGS84
  final_multipolygons <- st_transform(final_multipolygons, 4326)
  mountain_range <- st_transform(mountain_range, 4326)
  
  # Clean mountain name for filenames
  mountain_name_clean <- gsub(" ", "_", mountain_name)
  
  # Save shapefile
  st_write(
    final_multipolygons,
    paste0(data_storage_path, "Datasets/protected_areas/all_mountains/clean_PAs_", mountain_name_clean, ".shp"),
    delete_layer = TRUE,
    overwrite = FALSE
  )
  
  # Save metadata CSV
  output_path <- paste0(data_storage_path, "Outputs/protected_areas/metadata_PA/PA_metadata_", mountain_name_clean, ".csv")
  write.csv(PA_metadata, output_path, row.names = FALSE)
}
