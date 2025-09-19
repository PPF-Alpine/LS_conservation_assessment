library(terra)
library(stringr)
library(terra)
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

#---------------------------------------------#
# Step 1: list files
#---------------------------------------------#
source(here::here("R/00_Config_file_HKH.R"))

total_endemism_join <- read.csv(paste0(data_storage_path,"Datasets/species_list/species_endemism/total_species_endemism.csv"))|>
  filter(!is.na(pct_in_HKH_area), pct_in_HKH_area > 0)

species_list <- readxl::read_excel(paste0(data_storage_path,"Datasets/species_list/assessment_hkh_mammals_09082025_LS.xlsx"))


species_richness_total<-rast(paste0(data_storage_path, "Datasets/species_list/species_richness/HKH_species_richness_TOTAL.tif"))
smallest_range<-rast(paste0(data_storage_path, "Datasets/species_list/species_richness/smallest_range.tif"))
elev_range <-rast(paste0(data_storage_path, "Datasets/species_list/species_richness/smallest_elev_range.tif"))
HKH_only<-rast(paste0(data_storage_path, "Datasets/species_list/species_richness/HKH_only.tif"))
threatened<-rast(paste0(data_storage_path, "Datasets/species_list/species_richness/globally_threatened_species.tif"))
data_deficient<-rast(paste0(data_storage_path, "Datasets/species_list/species_richness/globally_dd_species.tif"))


#---------------------------------------------#
# plot single category
#---------------------------------------------#
ggplot() +
  geom_spatraster(data = elev_range) +
  scale_fill_viridis_c(na.value = NA, guide = guide_colorbar(title = NULL)) +
  theme_minimal(base_size = 10) +
  theme(
    axis.title = element_blank(),
    axis.text  = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )+
  theme(legend.position = "right")

#---------------------------------------------#
# plot all
#---------------------------------------------#

# helper: one plot per raster (each keeps its own legend/scale)
plot_r <- function(r, title) {
  ggplot() +
    geom_spatraster(data = r) +
    scale_fill_viridis_c(na.value = NA, guide = guide_colorbar(title = NULL)) +
    theme_minimal(base_size = 10) +
    labs(title = title, fill = NULL) +
    theme(
      axis.title = element_blank(),
      axis.text  = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(face = "bold", hjust = 0.5)
    )+
    theme(legend.position = "right")
}

# make the individual maps
p_total   <- plot_r(species_richness_total, "Total species richness")
p_small   <- plot_r(smallest_range,         "Smallest range")
p_elev    <- plot_r(elev_range,             "Smallest elev. range")
p_hkh     <- plot_r(HKH_only,               "HKH-only species")
p_threat  <- plot_r(threatened,             "Globally threatened")
p_dd      <- plot_r(data_deficient,         "Data deficient")

# arrange: 3 columns x 2 rows (change ncol/nrow as you like)
# guides = "keep" ensures each plot keeps its own legend
final_plot <- (p_total | p_small | p_elev) /
  (p_hkh  | p_threat | p_dd) +
  plot_annotation(title = "HKH biodiversity maps") &
  theme(plot.title = element_text(size = 14, face = "bold"))

# show it
final_plot



#---------------------------------------------#
# account for fraction of total species richness per cell
#---------------------------------------------#

tot <- species_richness_total

# stack the category rasters in the same order you care about
cats <- c(smallest_range, elev_range, HKH_only, threatened, data_deficient)
names(cats) <- c("smallest_range","smallest_elev_range","HKH_only","threatened","data_deficient")

# avoid divide-by-zero: set proportion to NA where total == 0
prop_layers <- ifel(tot == 0, 0, cats / tot)

smallest_range <- prop_layers$smallest_range
elev_range <- prop_layers$smallest_elev_range
HKH_only <- prop_layers$HKH_only
threatened <- prop_layers$threatened
data_deficient <- prop_layers$data_deficient

# make the individual maps
p_total   <- plot_r(species_richness_total, "Total species richness")
p_small   <- plot_r(smallest_range,         "Smallest range")
p_elev    <- plot_r(elev_range,             "Smallest elev. range")
p_hkh     <- plot_r(HKH_only,               "HKH-only species")
p_threat  <- plot_r(threatened,             "Globally threatened")
p_dd      <- plot_r(data_deficient,         "Data deficient")

# arrange: 3 columns x 2 rows (change ncol/nrow as you like)
# guides = "keep" ensures each plot keeps its own legend
final_plot_norm <- (p_total | p_small | p_elev) /
  (p_hkh  | p_threat | p_dd) +
  plot_annotation(title = "HKH biodiversity maps normalized by total richness") &
  theme(plot.title = element_text(size = 14, face = "bold"))

# show it
final_plot_norm


# Now plot prop_layers (all share the same 0–1 scale)
plot(prop_layers, main = names(prop_layers))

smallest_range <- prop_layers$smallest_range
plot(smallest_range)
elev_range <- prop_layers$smallest_elev_range
HKH_only <- prop_layers$HKH_only
threatened <- prop_layers$threatened
data_deficient <- prop_layers$data_deficient

out_file <- file.path(data_storage_path, "Datasets", "species_list","species_richness","elev_range_norm.tif")
writeRaster(elev_range, out_file, overwrite = TRUE, datatype = "INT2U",
            gdal = c("TILED=YES", "COMPRESS=LZW", "PREDICTOR=2", "ZLEVEL=9"))

out_file <- file.path(data_storage_path, "Datasets", "species_list","species_richness","normalized_richness_maps.tif")
writeRaster(prop_layers, out_file, overwrite = TRUE, datatype = "FLT4S",
            gdal = c("TILED=YES", "COMPRESS=LZW", "PREDICTOR=2", "ZLEVEL=9"))

#---------------------------------------------#
# save
#---------------------------------------------#
# optional: save a high-res imageggsave
ggsave(
  filename = paste0(data_storage_path, "Datasets/species_list/species_richness/HKH_biodiversity_maps.png"),
  plot = final_plot,
  width = 14,
  height = 9,
  dpi = 300
)

# optional: save a high-res imageggsave
ggsave(
  filename = paste0(data_storage_path, "Datasets/species_list/species_richness/HKH_biodiversity_maps_norm.png"),
  plot = final_plot_norm,
  width = 14,
  height = 9,
  dpi = 300
)
