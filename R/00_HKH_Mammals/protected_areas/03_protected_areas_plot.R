

library(sf)
library(stringr)
library(purrr)
library(dplyr)
library(tibble)
library(tools)
library(stringr)
# pkgs
library(terra)        # you already use this
library(tidyterra)    # ggplot geoms for terra rasters
library(ggplot2)
library(patchwork)    # to arrange plots

#----------------------------------------------------------#
# 0. load files  -----
#----------------------------------------------------------#

# what proportion of richness is covered by PA
# how well are the top 30% of values represented in PA
# which unprotected areas have high richness


richness<-rast(paste0(data_storage_path, "Datasets/species_list/species_richness/HKH_species_richness_TOTAL.tif"))
threatened<-rast(paste0(data_storage_path, "Datasets/species_list/species_richness/biodiv_dimensions_0918/globally_threathened.tif"))
pa_path <- paste0(data_storage_path, "Datasets/protected_areas/PA_HKH/pa_in_hkh.gpkg")
pa <- st_read(pa_path)


prop_threathened <- ifel(richness == 0, 0, threatened / richness)
plot(prop_threathened)
#----------------------------------------------------------#
# plot richness and protected areas  ----
#----------------------------------------------------------#

ggplot() +
  geom_spatraster(data = richness) +
  geom_sf(data = pa, color = "red", fill = NA) +
  scale_fill_viridis_c(na.value = NA, guide = guide_colorbar(title = NULL)) +
  theme_minimal(base_size = 10) +
  theme(
    axis.title = element_blank(),
    axis.text  = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5)
  ) +
  theme(legend.position = "right")

#---------------------------------------------#
# plot all
#---------------------------------------------#

# helper: one plot per raster (each keeps its own legend/scale)
plot_r <- function(r, title) {
  ggplot() +
    geom_spatraster(data = r) +
    geom_sf(data = pa, color = "red", fill = NA) +
    scale_fill_viridis_c(na.value = NA, guide = guide_colorbar(title = NULL)) +
    theme_minimal(base_size = 10) +
    labs(title = title, fill = NULL) +
    theme(
      axis.title = element_blank(),
      axis.text  = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(size=14)
    ) +
    theme(legend.position = "right")
}

# make the individual maps
p_total   <- plot_r(richness, "Total species richness")
p_threat_prop  <- plot_r(prop_threathened,"Prop. globally threatened")

#---------------------------------------------#
# arrange plots in patchwork
#---------------------------------------------#

# 
final_plot <- (p_total | p_threat_prop) +
  theme(plot.title = element_text(size = 14))

# show it
final_plot

# optional: save a high-res imageggsave
ggsave(
  filename = paste0(data_storage_path, "Datasets/species_list/species_richness/threathened_richness_PA coverage.png"),
  plot = final_plot,
  width = 15,
  height = 4,
  dpi = 300
)
