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
# assign priority classes for each cell
#---------------------------------------------#

# top 25% as high 
# 50 - 75 medium 
# <50 as low 

thresh_clim <- quantile(values(climate_exposure), probs = 0.75, na.rm = TRUE)
climate_high <- ifel(climate_exposure >= thresh_clim, 1, 0)
plot(climate_high)

thresh_richness <- quantile(values(species_richness_total), probs = 0.75, na.rm = TRUE)
richness_high <- ifel(species_richness_total >= thresh_richness, 1, 0)
plot(richness_high)

thresh_threathened <- quantile(values(threatened), probs = 0.75, na.rm = TRUE)
threathened_high <- ifel(threatened >= thresh_threathened, 1, 0)
plot(threathened_high)

thresh_nat_threathened <- quantile(values(threatened_nat), probs = 0.75, na.rm = TRUE)
threathened_nat_high <- ifel(threatened_nat >= thresh_nat_threathened, 1, 0)
plot(threathened_nat_high)


thresh_hkhonly <- quantile(values(HKH_only), probs = 0.75, na.rm = TRUE)
hkhonly_high <- ifel(HKH_only >= thresh_hkhonly, 1, 0)
plot(hkhonly_high)

# pa raster is already binary 


plot_indiv <- function(r, title) {
  ggplot() +
    geom_spatraster(data = as.factor(r)) +
    
    scale_fill_manual(
      values = c("0" = "grey90", "1" = "cadetblue4"),
      breaks = c("0", "1"),
      labels = c("0", "1"),
      na.value = "transparent",
      name = NULL
    ) +
    
    theme_minimal(base_size = 10) +
    labs(title = title) +
    
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background = element_rect(fill = "white", colour = NA),
      
      plot.title = element_text(hjust = 0.5),
      legend.position = "right"
    )
}

p_total <- plot_indiv(richness_high, "Highest species richness")
plot(p_total)
p_threat <- plot_indiv(threathened_high, "Highest threatened species richness")
p_clim <- plot_indiv(climate_high, "Highest projected climatic difference")
p_hkhonly <- plot_indiv(hkhonly_high, "Highest HKH endemics")
#---------------------------------------------#
# priority index with threathened
#---------------------------------------------#

# priority = richness + threathened + climate + protection 
# priority_gap = richness + threathened + climate - protection 

bio_priority <- richness_high + threathened_high + climate_high 


summary_map <- ifel(bio_priority == 0, 0,ifel(bio_priority >= 1 & pa_raster == 1, bio_priority, bio_priority + 3))

#writeRaster(summary_map,paste0(data_storage_path, "Output/priority_indices/priority_summary_threathend.tif"), overwrite = TRUE)



#---------------------------------------------#
# Nicer map
#---------------------------------------------#

# nice map
summary_df <- as.data.frame(summary_map, xy = TRUE, na.rm = TRUE)

colnames(summary_df) <- c("x", "y", "class")
summary_df$class <- factor(summary_df$class,
                           levels = 0:6,
                           labels = c("Lower priority",
                                      "1 priority protected",
                                      "2 priority protected",
                                      "3 priority protected",
                                      "1 priority unprotected",
                                      "2 priority unprotected",
                                      "3 priority unprotected"))

sum_1 <- ggplot(summary_df, aes(x = x, y = y, fill = class)) +
  geom_raster() +
  geom_sf(data = borders_hkh_sf, fill = NA, color = "grey40", linewidth = 0.25, inherit.aes = FALSE) +
  coord_sf() +
  scale_fill_manual(values = c(
    "grey90",
    "lightgreen",
    "green3",
    "darkgreen",
    "khaki",
    "orange",
    "red"
  )) +
  theme_void() +
  labs(fill = NULL)

x11()
plot(sum_1)


# ---->   add the individual maps 
common_theme <- theme(
  plot.title = element_text(size = 12, hjust = 0.5),
  legend.position = "right",
  plot.margin = margin(2, 2, 2, 2)
)

p_total2  <- p_total  + common_theme
p_threat2 <- p_threat + common_theme
p_clim2   <- p_clim   + common_theme

# bottom row with  shared legend
bottom_row <- (p_total2 | p_threat2 | p_clim2) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "right",
    legend.title = element_blank()
  )

# top plot
top_row <- sum_1 +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),
    plot.margin = margin(2, 2, 2, 2)
  )

# combine: smaller top, bigger bottom, less vertical gap
final_plot_thr <- top_row / bottom_row +
  plot_layout(heights = c(1.5, 1))   # adjust to taste

plot(final_plot_thr)



#---------------------------------------------#
# priority index with HKH only
#---------------------------------------------#

# priority = richness + threathened + climate + protection 
# priority_gap = richness + threathened + climate - protection 

bio_priority <- richness_high + hkhonly_high+ climate_high 

summary_map <- ifel(bio_priority == 0, 0,ifel(bio_priority >= 1 & pa_raster == 1, bio_priority, bio_priority + 3))

#writeRaster(summary_map,paste0(data_storage_path, "Output/priority_indices/priority_summary_hkh_end.tif"), overwrite = TRUE)

# nice map
summary_df <- as.data.frame(summary_map, xy = TRUE, na.rm = TRUE)

colnames(summary_df) <- c("x", "y", "class")
summary_df$class <- factor(summary_df$class,
                           levels = 0:6,
                           labels = c("Lower priority",
                                      "1 priority protected",
                                      "2 priority protected",
                                      "3 priority protected",
                                      "1 priority unprotected",
                                      "2 priority unprotected",
                                      "3 priority unprotected"))




sum_2 <- ggplot(summary_df, aes(x = x, y = y, fill = class)) +
  geom_raster() +
  geom_sf(data = borders_hkh_sf, fill = NA, color = "grey40", linewidth = 0.25, inherit.aes = FALSE) +
  coord_sf() +
  scale_fill_manual(values = c(
    "grey90",
    "lightgreen",
    "green3",
    "darkgreen",
    "khaki",
    "orange",
    "red"
  )) +
  theme_void() +
  labs(fill = NULL)

x11()
plot(sum_2)



# ---->   add the individual maps 
common_theme <- theme(
  plot.title = element_text(size = 12, hjust = 0.5),
  legend.position = "right",
  plot.margin = margin(2, 2, 2, 2)
)

p_total2  <- p_total  + common_theme
p_hkh2 <- p_hkhonly + common_theme
p_clim2   <- p_clim   + common_theme

# bottom row with  shared legend
bottom_row <- (p_total2 | p_hkh2 | p_clim2) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "right",
    legend.title = element_blank()
  )

# top plot
top_row <- sum_2 +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),
    plot.margin = margin(2, 2, 2, 2)
  )

# combine: smaller top, bigger bottom, less vertical gap
final_plot_hkhonly <- top_row / bottom_row +
  plot_layout(heights = c(1.5, 1))   # adjust to taste

plot(final_plot_hkhonly)




#---------------------------------------------#
# priority index with HKH only
#---------------------------------------------#

# priority = richness + threathened + climate + protection 
# priority_gap = richness + threathened + climate - protection 

bio_priority <- richness_high + threathened_nat_high+ climate_high 
plot(bio_priority)

summary_map <- ifel(bio_priority == 0, 0,ifel(bio_priority >= 1 & pa_raster == 1, bio_priority, bio_priority + 3))
plot(summary_map)
#writeRaster(summary_map,paste0(data_storage_path, "Output/priority_indices/priority_summary_hkh_end.tif"), overwrite = TRUE)

# nice map
summary_df <- as.data.frame(summary_map, xy = TRUE, na.rm = TRUE)

colnames(summary_df) <- c("x", "y", "class")
summary_df$class <- factor(summary_df$class,
                           levels = 0:6,
                           labels = c("Lower priority",
                                      "1 priority protected",
                                      "2 priority protected",
                                      "3 priority protected",
                                      "1 priority unprotected",
                                      "2 priority unprotected",
                                      "3 priority unprotected"))




sum_3 <- ggplot(summary_df, aes(x = x, y = y, fill = class)) +
  geom_raster() +
  geom_sf(data = borders_hkh_sf, fill = NA, color = "grey40", linewidth = 0.25, inherit.aes = FALSE) +
  coord_sf() +
  scale_fill_manual(values = c(
    "grey90",
    "lightgreen",
    "green3",
    "darkgreen",
    "khaki",
    "orange",
    "red"
  )) +
  theme_void() +
  labs(fill = "Priority class")

x11()
plot(sum_3)




#---------------------------------------------#
# save
#---------------------------------------------#
# 
ggsave(
  filename = paste0(data_storage_path, "Output/priority_indices/priority_thr.png"),
  plot = final_plot_thr,
  width = 14,
  height = 9,
  dpi = 300
)


ggsave(
  filename = paste0(data_storage_path, "Output/priority_indices/priority_hkhonly.png"),
  plot = final_plot_hkhonly,
  width = 14,
  height = 9,
  dpi = 300
)
