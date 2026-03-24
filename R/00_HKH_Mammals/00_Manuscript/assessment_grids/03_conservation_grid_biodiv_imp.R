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
  labs(title = "High priority combined", fill = NULL)

print(sum_1)
