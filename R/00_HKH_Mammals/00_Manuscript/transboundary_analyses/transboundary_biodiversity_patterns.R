
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

#============================================================#
# Load border segments and create buffers
#============================================================#

# border segments and create buffer around each segment
border_segments <- sf::st_read(paste0(data_storage_path, "Output/transboundary/borders_segments_100_countrypairs.shp"))
plot(border_segments$geom)

# create 10km or 100 km ? buffer around each segment
buffer_dist <- 100000 

segment_buffers <- st_buffer(border_segments, dist = buffer_dist)

plot(segment_buffers)
#---------------------------------------------#
# plot single category
#---------------------------------------------#

#============================================================#
# Extract biodiversity values within buffer areas
#============================================================#

biodiv_layers <- c(
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

names(biodiv_layers) <- c(
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

# make sure buffers have an ID
segment_buffers <- segment_buffers |>
  mutate(ID = row_number())

# convert buffers to terra vector
segment_buffers_v <- terra::vect(segment_buffers)

# extract mean values per buffer
biodiv_buffer_mean <- terra::extract(
  biodiv_layers,
  segment_buffers_v,
  fun = mean,
  na.rm = TRUE
)

# extract 90th percentile values per buffer
biodiv_buffer_q90 <- terra::extract(
  biodiv_layers,
  segment_buffers_v,
  fun = function(x, na.rm = TRUE)
    quantile(x, 0.9, na.rm = na.rm),
  na.rm = TRUE
)

# join extracted values back to buffer polygons
segment_buffers_mean <- segment_buffers |>
  left_join(biodiv_buffer_mean, by = "ID")

segment_buffers_q90 <- segment_buffers |>
  left_join(biodiv_buffer_q90, by = "ID")

border_segments_q90 <- border_segments |>
  mutate(ID = row_number()) |>
  left_join(
    st_drop_geometry(segment_buffers_q90),
    by = "ID"
  )

#============================================================#
# Function to plot border lines by one variable
#============================================================#

plot_buffer_var <- function(line_sf, var_name, title) {
  
  ggplot() +
    
    geom_sf(
      data = line_sf,
      aes(color = .data[[var_name]]),
      linewidth = 2
    ) +
    
    scale_color_viridis_c(
      na.value = NA,
      
      guide = guide_colorbar(
        barheight = unit(2.5, "cm"),
        barwidth  = unit(0.25, "cm")
      )
    ) +
    
    labs(
      title = title,
      color = NULL
    ) +
    
    theme_minimal(base_size = 9) +
    
    theme(
      axis.title = element_blank(),
      axis.text  = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      
      plot.title = element_text(
        face = "bold",
        hjust = 0.5,
        size = 11
      ),
      
      legend.position = "right",
      
      legend.text = element_text(size = 7),
      
      plot.margin = margin(2,2,2,2)
    )
}

p_total <- plot_buffer_var(border_segments_q90, "Species richness", "Species richness")
p_small <- plot_buffer_var(border_segments_q90, "Smallest range", "Smallest range")
p_elev  <- plot_buffer_var(border_segments_q90, "Smallest elev. range", "Smallest elev. range")
p_hkh   <- plot_buffer_var(border_segments_q90, "HKH-only species", "HKH-only species")
p_glob  <- plot_buffer_var(border_segments_q90, "Globally threatened", "Globally threatened")
p_nat   <- plot_buffer_var(border_segments_q90, "Nationally threatened", "Nationally threatened")
p_dd    <- plot_buffer_var(border_segments_q90, "Data deficient", "Data deficient")
p_uniq  <- plot_buffer_var(border_segments_q90, "Uniqueness", "Uniqueness")
p_pd    <- plot_buffer_var(border_segments_q90, "Phylogenetic diversity", "Phylogenetic diversity")

final_buffer_plot <- 
  (p_total | p_small | p_elev) /
  (p_hkh | p_glob | p_nat) /
  (p_dd | p_uniq | p_pd) +
  
  plot_layout(guides = "keep")
  
  theme(
    panel.spacing = unit(0.2, "lines")
  )


final_buffer_plot
ggsave(
  filename = paste0(data_storage_path, "Output/biodiv_dimensions/HKH_biodiversity_maps_borders.png"),
  plot = final_buffer_plot,
  width = 12,
  height = 10,
  dpi = 300
)
