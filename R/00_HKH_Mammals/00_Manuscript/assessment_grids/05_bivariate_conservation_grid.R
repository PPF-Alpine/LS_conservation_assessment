#---------------------------------------------#
# Packages
#---------------------------------------------#
library(terra)
library(sf)
library(tidyterra)
library(ggplot2)
library(dplyr)
library(patchwork)

source(here::here("R/00_Config_file_HKH.R"))

#---------------------------------------------#
# Input data
#---------------------------------------------#
biodiv_imp <- rast(file.path(data_storage_path, "Output/biodiv_dimensions/biodiv_imp.tif"))
climate_distance <- rast(file.path(data_storage_path, "Datasets/climate_shift/clim_shift_eucl.tif"))
pa_raster <- rast(file.path(data_storage_path, "Datasets/protected_areas/protected_raster.tif"))
borders_hkh_sf <- st_read(file.path(data_storage_path, "Datasets/protected_areas/borders_HKH.shp"))
pa_hkh <- st_read(file.path(data_storage_path, "Datasets/protected_areas/PA_HKH/pa_in_HKH.shp"))
pa_hkh<- pa_hkh$geometry
plot(pa_hkh$geometry)
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
plot(combo_all)

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



#---------------------------------------------#
# Bivariate plot 
#---------------------------------------------#
bivar <- biodiv_class * 10 + climate_class

bivar_df <- as.data.frame(bivar, xy = TRUE, na.rm = TRUE)
names(bivar_df)[3] <- "class"

bivar_df$class <- factor(
  bivar_df$class,
  levels = c(11,12,13,21,22,23,31,32,33)
)

bivar_cols <- c(
  "11" = "#e8e8e8",
  "12" = "#b8d6be",
  "13" = "#73ae80",
  "21" = "#dfb0d6",
  "22" = "#a5add3",
  "23" = "#5698b9",
  "31" = "#be64ac",
  "32" = "#8c62aa",
  "33" = "#3b4994"
)

library(ggplot2)

p_map <- ggplot() +
  geom_tile(
    data = bivar_df,
    aes(x = x, y = y, fill = class)
  ) +
  geom_sf(
    data = pa_hkh,
    fill = NA,           # no fill
    color = "black",
    linewidth = 0.2
  ) +
  scale_fill_manual(values = bivar_cols) +
  coord_sf() +          # use coord_sf for sf layers
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    legend.position = "none"
  )
x11()
plot(p_map)


## add the legend 
legend_df <- expand.grid(
  x = 1:3,
  y = 1:3
)

legend_df$class <- factor(
  c("11","12","13",
    "21","22","23",
    "31","32","33"),
  levels = names(bivar_cols)
)

p_legend <- ggplot(legend_df, aes(x = x, y = y, fill = class)) +
  geom_tile() +
  scale_fill_manual(values = bivar_cols, guide = "none") +
  scale_x_continuous(
    breaks = 1:3,
    labels = c("Low","Medium","High"),
    expand = c(0,0)
  ) +
  scale_y_continuous(
    breaks = 1:3,
    labels = c("Low","Medium","High"),
    expand = c(0,0)
  ) +
  labs(
    x = "Predicted climate change",
    y = "Biodiversity importance"
  ) +
  coord_fixed() +
  theme_minimal() +
  theme(panel.grid = element_blank())



library(patchwork)

comb <- p_map +
  inset_element(
    p_legend,
    left = 0.05,
    bottom = 0.05,
    right = 0.32,
    top = 0.30
  )

comb
comb


ggsave(
  filename = file.path(data_storage_path, "Output/priority_indices/bivariat_cons_grid.png"),
  plot = comb,
  width = 14,
  height = 9,
  dpi = 300
)
