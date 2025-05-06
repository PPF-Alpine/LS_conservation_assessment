# calculate area coverage of PAs

# terrain adjusted surface area coverage 

#----------------------------------------------------------#
# 1. Set up  -----
#----------------------------------------------------------#
library(here)
#install.packages("wdpar")
#webdriver::install_phantomjs()
library(terra)
library(elevatr)
library(tidyverse)
library(sf)
library(lwgeom)
library(leaflet)


source(here::here("R/00_Config_file.R"))

#----------------------------------------------------------#
# Load data  -----
#----------------------------------------------------------#


# DEFINE MOUNTAIN RANGE
mountain_name <- "Himalaya"

# get PAs for mountain ranges with alpine biome

#source the gmba mountains
mountain_shapes <- sf::st_read(paste(data_storage_path,"Datasets/Mountains/GMBA_Levels/Level_03/GMBA_Inventory_v2.0_Level_03.shp", sep = "/"))

# source alpine biome
alpine_shapes <- sf::st_read(paste(data_storage_path,"Datasets/Mountains/Alpine_Biome/alpine_biome.shp", 
                                   sep = "/"))

# filter mountains with alpine biome
mountain_shapes_selected <- mountain_shapes|>
  filter(MapName %in% alpine_shapes$Mntn_rn)


PA_mountain <- sf::st_read(
  paste0(data_storage_path, "/Datasets/protected_areas/all_mountains/clean_PAs_", mountain_name, ".shp")
)


#----------------------------------------------------------#
# select and reproject crs ---
#----------------------------------------------------------#

mountain_name <- "Himalaya"

mountain_range <- mountain_shapes_selected|>
  filter(MapName == mountain_name)

alpine_range <- alpine_shapes |>
  filter(Mntn_rn == mountain_name)

# define crs
target_crs <- 4326

# Make sure everything has the same CRS
PA_mountain <- st_transform(PA_mountain, st_crs(target_crs))
alpine_range <- st_transform(alpine_range, st_crs(target_crs))
mountain_range <- st_transform(mountain_range,st_crs(target_crs))

PA_mountain <- st_make_valid(PA_mountain)
alpine_range <- st_make_valid(alpine_range)
mountain_range <- st_make_valid(mountain_range)


### Important
sf_use_s2(FALSE)  # disable s2 for geometry ops

# ----------------------------
# Calculate total protected area coverage
# ----------------------------

# Intersections
alpine_pa_cov <- st_intersection(alpine_range, PA_mountain)
mountain_pa_cov <- st_intersection(mountain_range, PA_mountain)

# Total geodetic areas (m²) - corrected for earth curvature 
alpine_area_total <- st_geod_area(alpine_range) |> sum()
mountain_area_total <- st_geod_area(mountain_range) |> sum()

# Total protected areas (m²) - earth curvature
alpine_area_protected <- st_geod_area(alpine_pa_cov) |> sum()
mountain_area_protected <- st_geod_area(mountain_pa_cov) |> sum()

# ----------------------------
# Total protection summary (no IUCN cat)
# ----------------------------
total_rows <- tibble(
  Range = c("Alpine", "Mountain"),
  IUCN_ct = "ALL",
  total_area_km2 = c(as.numeric(alpine_area_total), as.numeric(mountain_area_total)) / 1e6,
  protected_area_km2 = c(as.numeric(alpine_area_protected), as.numeric(mountain_area_protected)) / 1e6,
  percent_protected = c(
    as.numeric(alpine_area_protected) / as.numeric(alpine_area_total) * 100,
    as.numeric(mountain_area_protected) / as.numeric(mountain_area_total) * 100
  )
)

# ----------------------------
# Protection by IUCN category
# ----------------------------

# Alpine
alpine_iucn <- alpine_pa_cov |>
  mutate(area_m2 = st_geod_area(alpine_pa_cov)) |>
  group_by(IUCN_ct) |>
  summarise(protected_area_m2 = sum(area_m2), .groups = "drop") |>
  mutate(
    Range = "Alpine",
    total_area_m2 = as.numeric(alpine_area_total),
    percent_protected = 100 * as.numeric(protected_area_m2) / total_area_m2,
    total_area_km2 = total_area_m2 / 1e6,
    protected_area_km2 = as.numeric(protected_area_m2) / 1e6
  ) |>
  select(Range, IUCN_ct, total_area_km2, protected_area_km2, percent_protected)

# Mountain
mountain_iucn <- mountain_pa_cov |>
  mutate(area_m2 = st_geod_area(mountain_pa_cov)) |>
  group_by(IUCN_ct) |>
  summarise(protected_area_m2 = sum(area_m2), .groups = "drop") |>
  mutate(
    Range = "Mountain",
    total_area_m2 = as.numeric(mountain_area_total),
    percent_protected = 100 * as.numeric(protected_area_m2) / total_area_m2,
    total_area_km2 = total_area_m2 / 1e6,
    protected_area_km2 = as.numeric(protected_area_m2) / 1e6
  ) |>
  select(Range, IUCN_ct, total_area_km2, protected_area_km2, percent_protected)

# ----------------------------
# Final: Combine all results
# ----------------------------

final_coverage_df <- bind_rows(total_rows, alpine_iucn, mountain_iucn) |> 
  arrange(Range, desc(IUCN_ct)) 

final_coverage<- final_coverage_df|> 
  mutate(Mountain_range = mountain_name)|>
 select(Mountain_range,Range,IUCN_ct,total_area_km2,protected_area_km2,percent_protected)

# View
final_coverage

# ----------------------------
# write results
# ----------------------------

# DEFINE MOUNTAIN RANGE
mountain_name <- "Himalaya"

# Define file path
output_path <- paste0(data_storage_path, "Outputs/protected_areas/PA_coverage/PA_coverage_",mountain_name,".csv")
write.csv(final_coverage,output_path)




