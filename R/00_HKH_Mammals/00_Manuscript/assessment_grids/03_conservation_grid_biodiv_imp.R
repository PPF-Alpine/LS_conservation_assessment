# pkgs
library(terra)        # you already use this
library(tidyterra)    # ggplot geoms for terra rasters
library(ggplot2)
library(patchwork)    # to arrange plots

#---------------------------------------------#
# Step 1: list files
#---------------------------------------------#
source(here::here("R/00_Config_file_HKH.R"))

biodiv_imp<-rast(paste0(data_storage_path, "Output/biodiv_dimensions/biodiv_imp.tif"))
plot(biodiv_imp)

climate_distance<-rast(paste0(data_storage_path, "Datasets/climate_shift/clim_shift_eucl.tif"))

pa_raster<-rast(paste0(data_storage_path, "Datasets/protected_areas/protected_raster.tif"))

borders_hkh_sf <- sf::st_read(paste0(data_storage_path,"Datasets/protected_areas/borders_HKH.shp"))

climate_distance     <- resample(climate_distance, biodiv_imp, method = "near")
climate_distance     <- mask(crop(climate_distance, biodiv_imp), biodiv_imp)

pa_raster     <- resample(pa_raster, biodiv_imp, method = "near")
pa_raster        <- mask(crop(pa_raster, biodiv_imp), biodiv_imp)

protected <- ifel(pa_raster == 1, 1, 0)
unprotected <- ifel(pa_raster == 0, 1, 0)

#---------------------------------------------#
# select high medium low values
#---------------------------------------------#

# select high values
thresh_clim <- quantile(values(climate_distance), probs = 0.75, na.rm = TRUE)
climate_high <- ifel(climate_distance >= thresh_clim, 1, 0)

thresh_clim_low <- quantile(values(climate_distance), probs = 0.25, na.rm = TRUE)
climate_low <- ifel(climate_distance <= thresh_clim_low, 1, 0)

thresh_biodiv <- quantile(values(biodiv_imp), probs = 0.75, na.rm = TRUE)
biodiv_high <- ifel(biodiv_imp >= thresh_biodiv, 1, 0)
plot(biodiv_high)

#---------------------------------------------#
# select high medium low values
#---------------------------------------------#
combo4 <- ifel(biodiv_high == 1 & climate_high == 1 & protected == 1, 1,
               ifel(biodiv_high == 1 & climate_high == 1 & unprotected == 1, 2,
                    ifel(biodiv_high == 1 & climate_low  == 1 & protected == 1, 3,
                         ifel(biodiv_high == 1 & climate_low  == 1 & unprotected == 1, 4, 0))))

plot(combo4)

summary_df <- as.data.frame(combo4, xy = TRUE, na.rm = FALSE)
names(summary_df)
colnames(summary_df) <- c("x", "y", "class")

summary_df$class <- factor(
  summary_df$class,
  levels = c(0, 1, 2, 3, 4),
  labels = c(
    "Other",
    "High biodiv + high climate + protected",
    "High biodiv + high climate + unprotected",
    "High biodiv + low climate + protected",
    "High biodiv + low climate + unprotected"
  )
)

sum_1 <- ggplot(summary_df, aes(x = x, y = y, fill = class)) +
  geom_raster() +
  geom_sf(data = borders_hkh_sf, fill = NA, color = "grey40", linewidth = 0.25, inherit.aes = FALSE) +
  coord_sf() +
  scale_fill_manual(values = c(
    "Other" = "grey90",
    "High biodiv + high climate + protected" = "darkgreen",
    "High biodiv + low climate + protected" = "green3",
    "High biodiv + high climate + unprotected" = "red",
    "High biodiv + low climate + unprotected" = "orange"
  ), na.value = "white") +
  theme_void() +
  labs(title = NULL)

print(sum_1)


ggsave(
  filename = paste0(data_storage_path, "Output/priority_indices/biodiv_imp.png"),
  plot = sum_1,
  width = 14,
  height = 9,
  dpi = 300
)

################### all combinations 

library(terra)

# --- thresholds ---
q_clim_25 <- quantile(values(climate_distance), probs = 0.25, na.rm = TRUE)
q_clim_75 <- quantile(values(climate_distance), probs = 0.75, na.rm = TRUE)

q_bio_25  <- quantile(values(biodiv_imp), probs = 0.25, na.rm = TRUE)
q_bio_75  <- quantile(values(biodiv_imp), probs = 0.75, na.rm = TRUE)

# --- classify climate: 1 = low, 2 = medium, 3 = high ---
climate_class <- ifel(climate_distance <= q_clim_25, 1,
                      ifel(climate_distance >= q_clim_75, 3, 2))

# --- classify biodiversity: 1 = low, 2 = medium, 3 = high ---
biodiv_class <- ifel(biodiv_imp <= q_bio_25, 1,
                     ifel(biodiv_imp >= q_bio_75, 3, 2))

plot(climate_class)
plot(biodiv_class)


# 1 = protected, 0 = unprotected
prot_class <- ifel(protected == 1, 1, 0)

combo <- biodiv_class * 100 + climate_class * 10 + prot_class
plot(combo)
freq(combo)


writeRaster(combo,paste0(data_storage_path, "Output/priority_indices/priority_mapp_all_combo.tif"), overwrite = TRUE)


# ggplot with all combinations
levels_df <- data.frame(
  value = c(111,110,121,120,131,130,
            211,210,221,220,231,230,
            311,310,321,320,331,330),
  biodiv = rep(c("low","medium","high"), each = 6),
  climate = rep(c("low","low","medium","medium","high","high"), times = 3),
  protection = rep(c("protected","unprotected"), 9)
)

levels_df$class_name <- paste(
  "Biodiv:", levels_df$biodiv,
  "| Climate:", levels_df$climate,
  "|", levels_df$protection
)

levels_df

combo_cat <- as.factor(combo)

# attach full table
levels(combo_cat) <- levels_df

levels(combo_cat)
cats(combo_cat)

writeRaster(combo_cat,paste0(data_storage_path, "Output/priority_indices/priority_mapp_all_combo_cat.tif"), overwrite = TRUE)

allcombo <- ggplot() +
  geom_spatraster(data = combo_cat) +
  scale_fill_manual(
    values = c(
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
    ),na.value = "white",
    drop = FALSE,
    name = "Priority class"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.title = element_blank()
  )


#---------------------------------------------#
# save
#---------------------------------------------#
# 
ggsave(
  filename = paste0(data_storage_path, "Output/priority_indices/allcombo_plot.png"),
  plot = allcombo,
  width = 14,
  height = 9,
  dpi = 300
)
