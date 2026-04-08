#---------------------------------------------#
# Packages
#---------------------------------------------#
library(terra)
library(sf)
library(tidyterra)
library(ggplot2)
library(dplyr)

source(here::here("R/00_Config_file_HKH.R"))

#---------------------------------------------#
# Input data
#---------------------------------------------#
biodiv_imp <- rast(file.path(data_storage_path, "Output/biodiv_dimensions/biodiv_imp.tif"))
climate_distance <- rast(file.path(data_storage_path, "Datasets/climate_shift/clim_shift_eucl.tif"))
pa_raster <- rast(file.path(data_storage_path, "Datasets/protected_areas/protected_raster.tif"))
borders_hkh_sf <- st_read(file.path(data_storage_path, "Datasets/protected_areas/borders_HKH.shp"))

#---------------------------------------------#
# Align rasters to biodiversity raster
#---------------------------------------------#
climate_distance <- climate_distance |>
  resample(biodiv_imp, method = "near") |>
  crop(biodiv_imp) |>
  mask(biodiv_imp)

pa_raster <- pa_raster |>
  resample(biodiv_imp, method = "near") |>
  crop(biodiv_imp) |>
  mask(biodiv_imp)

#---------------------------------------------#
# Quantile thresholds
#---------------------------------------------#
q_clim_25 <- quantile(values(climate_distance), probs = 0.25, na.rm = TRUE)
q_clim_75 <- quantile(values(climate_distance), probs = 0.75, na.rm = TRUE)

q_bio_25 <- quantile(values(biodiv_imp), probs = 0.25, na.rm = TRUE)
q_bio_75 <- quantile(values(biodiv_imp), probs = 0.75, na.rm = TRUE)

#---------------------------------------------#
# Classify climate, biodiversity, protection
#---------------------------------------------#
# climate: 1 = low, 2 = medium, 3 = high
climate_class <- ifel(climate_distance <= q_clim_25, 1,
                      ifel(climate_distance >= q_clim_75, 3, 2))

# biodiversity: 1 = low, 2 = medium, 3 = high
biodiv_class <- ifel(biodiv_imp <= q_bio_25, 1,
                     ifel(biodiv_imp >= q_bio_75, 3, 2))

# protection: 1 = protected, 0 = unprotected
prot_class <- ifel(pa_raster == 1, 1, 0)

#---------------------------------------------#
# Raster 1: all combinations
# code = biodiversity * 100 + climate * 10 + protection
# e.g. 331 = high biodiv, high climate, protected
#      330 = high biodiv, high climate, unprotected
#---------------------------------------------#
combo_all <- biodiv_class * 100 + climate_class * 10 + prot_class

writeRaster(
  combo_all,
  file.path(data_storage_path, "Output/priority_indices/priority_map_all_combinations.tif"),
  overwrite = TRUE
)

# Lookup table for labels
levels_df <- data.frame(
  value = c(111,110,121,120,131,130,
            211,210,221,220,231,230,
            311,310,321,320,331,330),
  biodiv = rep(c("low", "medium", "high"), each = 6),
  climate = rep(c("low", "low", "medium", "medium", "high", "high"), times = 3),
  protection = rep(c("protected", "unprotected"), 9)
)

levels_df$class_name <- paste(
  "Biodiv:", levels_df$biodiv,
  "| Climate:", levels_df$climate,
  "|", levels_df$protection
)

combo_all_cat <- as.factor(combo_all)
levels(combo_all_cat) <- levels_df

writeRaster(
  combo_all_cat,
  file.path(data_storage_path, "Output/priority_indices/priority_map_all_combinations_cat.tif"),
  overwrite = TRUE
)

# Plot all combinations
all_combo_colors <- c(
  "Biodiv: low | Climate: low | protected" = "#1b9e77",
  "Biodiv: low | Climate: low | unprotected" = "#d95f02",
  "Biodiv: low | Climate: medium | protected" = "#7570b3",
  "Biodiv: low | Climate: medium | unprotected" = "#e7298a",
  "Biodiv: low | Climate: high | protected" = "#66a61e",
  "Biodiv: low | Climate: high | unprotected" = "#e6ab02",
  "Biodiv: medium | Climate: low | protected" = "#a6761d",
  "Biodiv: medium | Climate: low | unprotected" = "#666666",
  "Biodiv: medium | Climate: medium | protected" = "#1f78b4",
  "Biodiv: medium | Climate: medium | unprotected" = "#b2df8a",
  "Biodiv: medium | Climate: high | protected" = "#fb9a99",
  "Biodiv: medium | Climate: high | unprotected" = "#fdbf6f",
  "Biodiv: high | Climate: low | protected" = "#cab2d6",
  "Biodiv: high | Climate: low | unprotected" = "#6a3d9a",
  "Biodiv: high | Climate: medium | protected" = "#ffff99",
  "Biodiv: high | Climate: medium | unprotected" = "#b15928",
  "Biodiv: high | Climate: high | protected" = "#8dd3c7",
  "Biodiv: high | Climate: high | unprotected" = "#fb8072"
)

p_all <- ggplot() +
  geom_spatraster(data = combo_all_cat) +
  geom_sf(data = borders_hkh_sf, fill = NA, color = "grey40", linewidth = 0.25) +
  scale_fill_manual(
    values = all_combo_colors,
    na.value = "white",
    drop = FALSE,
    name = "Priority class"
  ) +
  coord_sf() +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right"
  )

plot(p_all)
ggsave(
  filename = file.path(data_storage_path, "Output/priority_indices/priority_map_all_combinations.png"),
  plot = p_all,
  width = 14,
  height = 9,
  dpi = 300
)

#---------------------------------------------#
# Raster 2: high-biodiversity combinations only
#---------------------------------------------#
combo_high_bio <- ifel(
  biodiv_class == 3 & climate_class == 3 & prot_class == 1, 1,
  ifel(
    biodiv_class == 3 & climate_class == 3 & prot_class == 0, 2,
    ifel(
      biodiv_class == 3 & climate_class == 2 & prot_class == 1, 3,
      ifel(
        biodiv_class == 3 & climate_class == 2 & prot_class == 0, 4,
        ifel(
          biodiv_class == 3 & climate_class == 1 & prot_class == 1, 5,
          ifel(
            biodiv_class == 3 & climate_class == 1 & prot_class == 0, 6,
            0
          )
        )
      )
    )
  )
)

writeRaster(
  combo_high_bio,
  file.path(data_storage_path, "Output/priority_indices/priority_map_high_biodiv_only.tif"),
  overwrite = TRUE
)

highbio_df <- as.data.frame(combo_high_bio, xy = TRUE, na.rm = FALSE)
colnames(highbio_df) <- c("x", "y", "class")

highbio_df$class <- factor(
  highbio_df$class,
  levels = c(0, 1, 2, 3, 4, 5, 6),
  labels = c(
    "Other",
    "High biodiv + high climate + protected",
    "High biodiv + high climate + unprotected",
    "High biodiv + medium climate + protected",
    "High biodiv + medium climate + unprotected",
    "High biodiv + low climate + protected",
    "High biodiv + low climate + unprotected"
  )
)

highbio_colors <- c(
  "Other" = "grey90",
  "High biodiv + high climate + protected" = "darkgreen",
  "High biodiv + high climate + unprotected" = "red",
  "High biodiv + medium climate + protected" = "forestgreen",
  "High biodiv + medium climate + unprotected" = "gold",
  "High biodiv + low climate + protected" = "green3",
  "High biodiv + low climate + unprotected" = "orange"
)

p_highbio <- ggplot(highbio_df, aes(x = x, y = y, fill = class)) +
  geom_raster() +
  geom_sf(data = borders_hkh_sf, fill = NA, color = "grey40", linewidth = 0.25, inherit.aes = FALSE) +
  coord_sf() +
  scale_fill_manual(
    values = highbio_colors,
    na.value = "white",
    drop = FALSE
  ) +
  theme_void() +
  labs(fill = "Priority class")

plot(p_highbio)

ggsave(
  filename = file.path(data_storage_path, "Output/priority_indices/priority_map_high_biodiv_only.jpeg"),
  plot = p_highbio,
  width = 14,
  height = 9,
  dpi = 300
)

#---------------------------------------------#
# Optional quick inspection
#---------------------------------------------#
plot(combo_all)
plot(combo_high_bio)