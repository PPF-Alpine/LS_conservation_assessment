
# pkgs
library(terra)        # you already use this
library(tidyterra)    # ggplot geoms for terra rasters
library(ggplot2)
library(patchwork)    # to arrange plots
library(sf)
#---------------------------------------------#
# Step 1: list files
#---------------------------------------------#
source(here::here("R/00_Config_file_HKH.R"))

total_endemism_join <- read.csv(paste0(data_storage_path,"Datasets/species_list/species_endemism/total_species_endemism.csv"))|>
  filter(!is.na(pct_in_HKH_area), pct_in_HKH_area > 0)

species_list <- readxl::read_excel(paste0(data_storage_path,"Datasets/species_list/assessment_hkh_mammals_09082025_LS.xlsx"))


species_richness_total<-rast(paste0(data_storage_path, "Datasets/species_list/species_richness/HKH_species_richness_TOTAL.tif"))
smallest_range<-rast(paste0(data_storage_path, "Datasets/species_list/species_richness/biodiversity_dimensions/smallest_range_median.tif"))
elev_range <-rast(paste0(data_storage_path, "Datasets/species_list/species_richness/biodiversity_dimensions/smallest_elev_median.tif"))
HKH_only<-rast(paste0(data_storage_path, "Datasets/species_list/species_richness/biodiversity_dimensions/mosthkh_median.tif"))
threatened<-rast(paste0(data_storage_path, "Datasets/species_list/species_richness/biodiversity_dimensions/globally_threathened.tif"))
threatened_nat<-rast(paste0(data_storage_path, "Datasets/species_list/species_richness/biodiversity_dimensions/nationally_threathened.tif"))
data_deficient<-rast(paste0(data_storage_path, "Datasets/species_list/species_richness/biodiversity_dimensions/dd_and_NA.tif"))
uniqueness <- rast(paste0(data_storage_path, "Output/uniqueness/r_lcbd.tif"))
phylogenetic <- rast(paste0(data_storage_path, "Output/phylogenetic_diversity/PD_raster.tif"))

#uniqueness[is.na(uniqueness)] <- 0
phylogenetic[is.na(phylogenetic)] <- 0
#uniqueness <- mask(uniqueness, species_richness_total)
plot(uniqueness)
phylogenetic <- resample(phylogenetic, species_richness_total)
phylogenetic <- mask(phylogenetic, species_richness_total)

#---------------------------------------------#
# Step 1: list files
#---------------------------------------------#

# border segments and create buffer around each segment
border_segments <- sf::st_read(paste0(data_storage_path, "Output/transboundary/borders_segments_100_countrypairs.shp"))
plot(border_segments$geom)

# create 10km or 100 km ? buffer around each segment
buffer_dist <- 100000 

segment_buffers <- st_buffer(border_segments, dist = buffer_dist)
#============================================================#
# Prepare biodiversity dimension rasters
#============================================================#

biodiv_dims <- c(
  species_richness_total,
  smallest_range,
  elev_range,
  HKH_only,
  threatened,
  threatened_nat,
  data_deficient,
  uniqueness,
  phylogenetic
)

names(biodiv_dims) <- c(
  "Species richness",
  "Smallest range",
  "Smallest elev. range",
  "HKH-only species",
  "Globally threatened",
  "Nationally threatened",
  "Data deficient",
  "Uniqueness",
  "Phylogenetic diversity"
)


#============================================================#
# Resample all biodiversity dimensions to same grid
#============================================================#

template <- species_richness_total

biodiv_dims_rs <- resample(biodiv_dims, template, method = "bilinear")
biodiv_dims_rs <- mask(biodiv_dims_rs, template)


#============================================================#
# Scale each biodiversity dimension to 0–1
#============================================================#

scale01 <- function(x) {
  (x - global(x, "min", na.rm = TRUE)[1,1]) /
    (global(x, "max", na.rm = TRUE)[1,1] -
       global(x, "min", na.rm = TRUE)[1,1])
}

biodiv_dims_01 <- biodiv_dims_rs

for (i in 1:nlyr(biodiv_dims_rs)) {
  r <- biodiv_dims_rs[[i]]
  
  r_min <- global(r, "min", na.rm = TRUE)[1,1]
  r_max <- global(r, "max", na.rm = TRUE)[1,1]
  
  biodiv_dims_01[[i]] <- (r - r_min) / (r_max - r_min)
}

#============================================================#
# Extract biodiversity values within border buffers
#============================================================#

border_segments_v <- terra::vect(segment_buffers)
border_segments_v <- terra::project(border_segments_v, terra::crs(biodiv_dims_01))

biodiv_seg_q90 <- terra::extract(
  biodiv_dims_01,
  border_segments_v,
  fun = function(x, na.rm = TRUE)
    quantile(x, 0.9, na.rm = na.rm),
  na.rm = TRUE
)

biodiv_seg_mean <- terra::extract(
  biodiv_dims_01,
  border_segments_v,
  fun = mean,
  na.rm = TRUE
)

biodiv_seg <- terra::extract(
  biodiv_dims_01,
  border_segments_v,
  na.rm = TRUE
)

#============================================================#
# Join segment metadata
#============================================================#

seg_meta <- border_segments |>
  st_drop_geometry() |>
  mutate(ID = row_number()) |>
  select(ID, pair_id, seg_id, len_km)

biodiv_seg_q90_long <- biodiv_seg_q90 |>
  left_join(seg_meta, by = "ID") |>
  pivot_longer(
    cols = all_of(names(biodiv_dims_01)),
    names_to = "dimension",
    values_to = "value"
  )

biodiv_seg_mean_long <- biodiv_seg_mean |>
  left_join(seg_meta, by = "ID") |>
  pivot_longer(
    cols = all_of(names(biodiv_dims_01)),
    names_to = "dimension",
    values_to = "value"
  )


biodiv_seg_long <- biodiv_seg |>
  left_join(seg_meta, by = "ID") |>
  pivot_longer(
    cols = all_of(names(biodiv_dims_01)),
    names_to = "dimension",
    values_to = "value"
  )
#============================================================#
# Plot 1: heatmap by country pair
#============================================================#

biodiv_pair_q90 <- biodiv_seg_q90_long |>
  group_by(pair_id, dimension) |>
  summarise(
    value = mean(value, na.rm = TRUE),
    .groups = "drop"
  )

biodiv_pair <- biodiv_seg_long |>
  group_by(pair_id, dimension) |>
  summarise(
    value = mean(value, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(biodiv_pair,
       aes(x = dimension,
           y = pair_id,
           fill = value)) +
  geom_tile(color = "white") +
  
  scale_fill_viridis_c(
    option = "magma",
    direction = -1
  ) +
  
  labs(
    x = NULL,
    y = "Country pair",
    fill = "Scaled biodiv value"
  ) +
  
  theme_minimal() +
  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

#============================================================#
# Plot 2: heatmap by individual border segment
#============================================================#

biodiv_seg_long <- biodiv_seg_long |>
  mutate(segment_label = paste0(pair_id, "_seg", seg_id))

ggplot(biodiv_seg_long,
       aes(x = dimension,
           y = segment_label,
           fill = value)) +
  geom_tile(color = "white") +
  labs(
    x = NULL,
    y = "Border segment",
    fill = "Scaled biodiv value"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )
