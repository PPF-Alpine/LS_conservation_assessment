library(sf)
library(terra)
library(rnaturalearth)
library(sf)
library(terra)
library(dplyr)
library(ggplot2)
library(viridis)

#---------------------------------------------#
# get border segments, countries and cons priority
#---------------------------------------------#

transb_species <- sf::st_read(paste0(data_storage_path, "Output/transboundary/transb_glob_threat_new.shp"))
hkh_boundary <- sf::st_read(paste0(data_storage_path, "Datasets/HKH_boundary/HKH_boundary.shp"))
plot(hkh_boundary)

# read HKH countries again
countries <- ne_countries(scale = "medium", returnclass = "sf")

hkh_countries <- c("Afghanistan", "Pakistan", "India", "Nepal",
                   "Bhutan", "China", "Myanmar", "Bangladesh")

countries_hkh <- countries |>
  filter(name %in% hkh_countries) |>
  st_make_valid() |>
  st_transform(st_crs(transb_species))

#---------------------------------------------#
# transform and cut 
#---------------------------------------------#

# transform vector layers to HKH boundary CRS
transb_species_r <- st_transform(transb_species, st_crs(hkh_boundary))
countries_hkh_r  <- st_transform(countries_hkh,  st_crs(hkh_boundary))

# make sure boundary is valid
hkh_boundary <- st_make_valid(hkh_boundary)

# clip to HKH boundary
countries_crop      <- st_intersection(countries_hkh_r,  hkh_boundary)

#---------------------------------------------#
# plot 
#---------------------------------------------#

p_transb <- ggplot() +
  geom_sf(data = countries_crop,
          fill = "grey92",
          color = "grey45",
          linewidth = 0.25) +
  
  geom_sf(data = transb_species_r,
          aes(color = n_shard, linewidth = n_shard),
          lineend = "round") +
  
  scale_color_viridis_c(
    option = "viridis",
    name = "Shared threatened\nspecies"
  ) +
  
  scale_linewidth(
    range = c(0.4, 2.5),   # adjust this!
    guide = "none"         # avoid double legend clutter
  ) +
  
  coord_sf(expand = FALSE) +
  
  theme_void() +
  theme(
    legend.position = "right"
  )

p_transb

ggsave(
  filename = paste0(
    data_storage_path,
    "Output/transboundary/transboundary_thr_species.jpeg"
  ),
  plot = p_transb,
  width = 14,
  height = 9,
  dpi = 300
)

