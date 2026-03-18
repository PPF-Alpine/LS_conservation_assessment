#---------------------------------------------#
# Prep input data 
#---------------------------------------------#


# pkgs
library(terra)        # you already use this
library(tidyterra)    # ggplot geoms for terra rasters
library(ggplot2)
library(patchwork)    # to arrange plots
library(rnaturalearth)
library(sf)

#---------------------------------------------#
# Step 1: list files
#---------------------------------------------#
source(here::here("R/00_Config_file_HKH.R"))


# species rasters
species_richness_total<-rast(paste0(data_storage_path, "Datasets/species_list/species_richness/HKH_species_richness_TOTAL.tif"))
threatened<-rast(paste0(data_storage_path, "Datasets/species_list/species_richness/biodiversity_dimensions/globally_threathened.tif"))
threatened_nat<-rast(paste0(data_storage_path, "Datasets/species_list/species_richness/biodiversity_dimensions/nationally_threathened.tif"))
HKH_only<-rast(paste0(data_storage_path, "Datasets/species_list/species_richness/biodiversity_dimensions/mosthkh_0_4.tif"))

# read in climate exposure 
climate_exposure<-rast(paste0(data_storage_path, "Datasets/climate_shift/climate_exposure.tif"))
plot(climate_exposure)

# protected area (shp)
#pa_path <- paste0(data_storage_path, "Datasets/protected_areas/PA_HKH/pa_in_hkh.gpkg")
#pa <- st_read(pa_path)
#plot(pa$geom)

# match projection
#pa_raster <- rasterize(pa, species_richness_total, touches=TRUE, field=1, background=0)
#plot(pa_raster)

#writeRaster(pa_raster,paste0(data_storage_path, "Datasets/protected_areas/protected_raster.tif"), overwrite = TRUE)
# read in protected areas

pa_raster<-rast(paste0(data_storage_path, "Datasets/protected_areas/protected_raster.tif"))


climate_exposure     <- resample(climate_exposure, species_richness_total, method = "near")
climate_exposure     <- mask(crop(climate_exposure, species_richness_total), species_richness_total)

pa_raster     <- resample(pa_raster, species_richness_total, method = "near")
pa_raster        <- mask(crop(pa_raster, species_richness_total), species_richness_total)



# get the hkh national borders for plots
#borders <- ne_countries(scale = "medium", returnclass = "sf")
#hkh_countries <- c("Afghanistan", "Pakistan", "India", "Nepal", "Bhutan", "China", "Myanmar", "Bangladesh")

#borders <- borders[borders$name %in% hkh_countries, ]
#borders_geom <- borders$geometry
#borders_vect <- vect(borders_geom)

# crop borders to raster extent
#borders_hkh <- crop(borders_vect, species_richness_total)
#borders_hkh_sf <- sf::st_as_sf(borders_hkh)
#sf::write_sf(borders_hkh_sf,paste0(data_storage_path, "Datasets/protected_areas/borders_HKH.shp"), overwrite = TRUE)

borders_hkh_sf <- sf::st_read(paste0(data_storage_path,"Datasets/protected_areas/borders_HKH.shp"))

#---------------------------------------------#
# scale to same. 0-1 
#---------------------------------------------#
# 
climate_scaled <- (climate_exposure - global(climate_exposure, "min", na.rm=TRUE)[1,1]) /
  (global(climate_exposure, "max", na.rm=TRUE)[1,1] - global(climate_exposure, "min", na.rm=TRUE)[1,1])

plot(climate_scaled)

richness_scaled <- (species_richness_total - global(species_richness_total, "min", na.rm=TRUE)[1,1]) /
  (global(species_richness_total, "max", na.rm=TRUE)[1,1] - global(species_richness_total, "min", na.rm=TRUE)[1,1])

plot(richness_scaled)

threatened_scaled <- (threatened - global(threatened, "min", na.rm=TRUE)[1,1]) /
  (global(threatened, "max", na.rm=TRUE)[1,1] - global(threatened, "min", na.rm=TRUE)[1,1])

plot(threatened_scaled)

#---------------------------------------------#
# combine and plot priority continuos
#---------------------------------------------#
priority_cont <- (richness_scaled + threatened_scaled + climate_scaled) / 3

# convert raster to dataframe
priority_cont_df <- as.data.frame(priority_cont, xy = TRUE, na.rm = TRUE)
colnames(priority_cont_df) <- c("x", "y", "value")

# ggplot object
p_cont <- ggplot(priority_cont_df, aes(x = x, y = y, fill = value)) +
  geom_raster() +
  geom_sf(data = borders_hkh_sf,
          fill = NA,
          color = "grey40",
          linewidth = 0.25,
          inherit.aes = FALSE) +
  coord_sf() +
  scale_fill_viridis_c(option = "viridis") +
  theme_void() +
  labs(fill = "Priority")

p_cont

#---------------------------------------------#
# show quartile priority classes 
#---------------------------------------------#
q50 <- quantile(values(priority_cont), 0.50, na.rm = TRUE)
q75 <- quantile(values(priority_cont), 0.75, na.rm = TRUE)

priority_class <- ifel(priority_cont >= q75, 3,
                       ifel(priority_cont >= q50, 2, 1))


plot(priority_class)

# convert raster to dataframe
priority_df <- as.data.frame(priority_class, xy = TRUE, na.rm = TRUE)
colnames(priority_df) <- c("x", "y", "class")

# make class a factor
priority_df$class <- factor(priority_df$class,
                            levels = c(1, 2, 3),
                            labels = c("Low", "Medium", "High"))

prio_class <- ggplot(priority_df, aes(x = x, y = y, fill = class)) +
  geom_raster() +
  geom_sf(data = borders_hkh_sf, fill = NA, color = "grey40", linewidth = 0.25, inherit.aes = FALSE) +
  coord_sf() +
  scale_fill_manual(values = c(
    "Low" = "grey90",
    "Medium" = "khaki",
    "High" = "red"
  )) +
  theme_void() +
  labs(fill = "Priority class")

prio_class

#---------------------------------------------#
# show dominant variables
#---------------------------------------------#
vars <- c(richness_scaled, threatened_scaled, climate_scaled)
names(vars) <- c("Richness", "Threat", "Climate")


dominant <- which.max(vars)
plot(dominant,
     col = c("forestgreen", "orange", "steelblue"),
     type = "classes",
     levels = c("Richness", "Threat", "Climate"))

dominant_df <- as.data.frame(dominant, xy = TRUE, na.rm = TRUE)
colnames(dominant_df) <- c("x", "y", "class")

dominant_df$class <- factor(dominant_df$class,
                            levels = c(1, 2, 3),
                            labels = c("Richness", "Threat", "Climate"))

p_dom <- ggplot(dominant_df, aes(x = x, y = y, fill = class)) +
  geom_raster() +
  geom_sf(data = borders_hkh_sf,
          fill = NA,
          color = "grey40",
          linewidth = 0.25,
          inherit.aes = FALSE) +
  coord_sf() +
  scale_fill_manual(values = c(
    "Richness" = "darkseagreen",
    "Threat" = "burlywood1",
    "Climate" = "lightsteelblue1"
  )) +
  theme_void() +
  labs(fill = "Dominant driver")

p_dom


combined_plot <- p_cont /
  (prio_class + p_dom) +
  plot_layout(heights = c(0.9, 1)) +
  plot_annotation(tag_levels = "A")

x11()
plot(combined_plot)

#---------------------------------------------#
# save
#---------------------------------------------#
# 
ggsave(
  filename = paste0(data_storage_path, "Output/priority_indices/continuos_combined_plot.png"),
  plot = combined_plot,
  width = 14,
  height = 9,
  dpi = 300
)

ggsave(
  filename = paste0(data_storage_path, "Output/priority_indices/prio_cont_classes.png"),
  plot = prio_class,
  width = 14,
  height = 9,
  dpi = 300
)


ggsave(
  filename = paste0(data_storage_path, "Output/priority_indices/prio_continuos.png"),
  plot = p_cont,
  width = 14,
  height = 9,
  dpi = 300
)


ggsave(
  filename = paste0(data_storage_path, "Output/priority_indices/dominant_drivers.png"),
  plot = p_dom,
  width = 14,
  height = 9,
  dpi = 300
)
