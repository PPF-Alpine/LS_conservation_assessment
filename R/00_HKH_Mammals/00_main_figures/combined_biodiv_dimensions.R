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
library(ggplot2)
library(patchwork)
library(scales)

#---------------------------------------------#
# Step 1: list files
#---------------------------------------------#
source(here::here("R/00_Config_file_HKH.R"))

total_endemism_join <- read.csv(paste0(data_storage_path,"Datasets/species_list/species_endemism/total_species_endemism.csv"))|>
  filter(!is.na(pct_in_HKH_area), pct_in_HKH_area > 0)

species_list <- readxl::read_excel(paste0(data_storage_path,"Datasets/species_list/assessment_hkh_mammals_09082025_LS.xlsx"))


species_richness_total<-rast(paste0(data_storage_path, "Datasets/species_list/species_richness/HKH_species_richness_TOTAL.tif"))
smallest_range<-rast(paste0(data_storage_path, "Datasets/species_list/species_richness/biodiv_dimensions_0918/smallest_range_0_4.tif"))
elev_range <-rast(paste0(data_storage_path, "Datasets/species_list/species_richness/biodiv_dimensions_0918/smallest_elev_0_4_new.tif"))
HKH_only<-rast(paste0(data_storage_path, "Datasets/species_list/species_richness/biodiv_dimensions_0918/mosthkh_0_4_new.tif"))
threatened<-rast(paste0(data_storage_path, "Datasets/species_list/species_richness/biodiv_dimensions_0918/globally_threathened.tif"))
data_deficient<-rast(paste0(data_storage_path, "Datasets/species_list/species_richness/biodiv_dimensions_0918/dd_and_NA.tif"))
nat_threatened<-rast(paste0(data_storage_path, "Datasets/species_list/species_richness/biodiv_dimensions_0918/nationally_threathened.tif"))


#---------------------------------------------#
# plot single category
#---------------------------------------------#
elev_range_masked <- elev_range
elev_range_masked[elev_range_masked == 0] <- NA

ggplot() +
  geom_spatraster(data = elev_range_masked) +
  scale_fill_viridis_c(
    na.value = NA,  # <— NA (and now 0) shown in grey
    guide = guide_colorbar(title = NULL)
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5),
    legend.position = "right"
  )


#---------------------------------------------#
# plot all ABSOLUTE numbers
#---------------------------------------------#

# helper: one plot per raster (each keeps its own legend/scale)
plot_r <- function(r, title) {
  ggplot() +
    geom_spatraster(
      data = r
    ) +
    scale_fill_viridis_c(na.value = NA, guide = guide_colorbar(title = NULL)) +
    
    theme_minimal(base_size = 10) +
    labs(title = title, fill = NULL) +
    theme(
      axis.title = element_blank(),
      axis.text  = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text( hjust = 0.5)
    )+
    theme(legend.position = "right")
}

#---------------------------------------------#
# plot all ABSOLUTE numbers --> 0 as grey
#---------------------------------------------#

plot_r <- function(r, title) {
  maxv <- as.numeric(terra::global(r, "max", na.rm = TRUE)[1, 1])
  
  cols <- c("grey80", viridis(256))                     # 0 -> grey, then dark blue → yellow
  vals <- c(0, seq(1e-6, 1, length.out = 256))          # length(vals) == length(cols)
  
  ggplot() +
    geom_spatraster(data = r) +
    scale_fill_gradientn(
      colours = cols,
      values  = vals,
      limits  = c(0, maxv),           # still covers 0–max (so grey shows on map)
      na.value = NA,
      oob = scales::squish,
      guide = guide_colorbar(
        title = NULL,
        barwidth = 0.35,              # narrower colorbar
        barheight = ,
        frame.colour = NA,
        # 👇 these breaks control what is shown on the legend
        ticks = TRUE
      ),
      breaks = pretty(c(3, maxv),n=5),  # 👈 legend starts at 1!
      labels = scales::number_format(accuracy = 2)
    ) +
    theme_minimal(base_size = 10) +
    labs(title = title, fill = NULL) +
    theme(
      axis.title = element_blank(),
      axis.text  = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(hjust = 0.5),
      legend.position = "right"
    )
}


# make the individual maps
p_total   <- plot_r(species_richness_total, "Total species richness")
p_small   <- plot_r(smallest_range,         "Smallest range")
p_elev    <- plot_r(elev_range,             "Smallest elev. range")
p_hkh     <- plot_r(HKH_only,               "HKH-only species")
p_threat  <- plot_r(threatened,             "Globally threatened")
p_dd      <- plot_r(data_deficient,         "Data deficient")
p_natthreat      <- plot_r(nat_threatened,         "Nationally threatened")

# arrange: 3 columns x 2 rows (change ncol/nrow as you like)
# guides = "keep" ensures each plot keeps its own legend
final_plot <- (p_total | p_threat | p_dd) /
  (p_natthreat  | p_small |p_elev| p_hkh) 

# show it
final_plot
ggsave(
  filename = paste0(data_storage_path, "Datasets/species_list/species_richness/biodiv_dimensions_0918/HKH_biodiversity_maps_absolute numbers.png"),
  plot = final_plot,
  width = 14,
  height = 9,
  dpi = 300
)


#---------------------------------------------#
# plot all NORMALIZED numbers
#---------------------------------------------#

tot <- species_richness_total

# stack the category rasters as a SpatRaster
cats <- rast(list(
  smallest_range,
  elev_range,
  HKH_only,
  threatened,
  data_deficient,
  nat_threatened
))
names(cats) <- c("small_geog", "small_elev", "hkh_only",
                 "threat_glob", "dd", "threat_nat")

# normalize each layer by total richness per cell
# avoid divide-by-zero
prop_layers <- ifel(tot == 0, 0, cats / tot)
prop_layers <- ifel(is.na(tot), NA, ifel(tot == 0, 0, cats / rep(tot, nlyr(cats))))
plot(prop_layers)

#out_file <- file.path(data_storage_path, "Datasets", "species_list","species_richness","biodiv_dimensions_0918","prop_layers.tif")
#writeRaster(prop_layers, out_file, overwrite = TRUE)

std_stack <- prop_layers
names(std_stack) <- names(cats)

# Composite priority as the mean of the proportions (0–1)
sum_stack <- app(std_stack, mean, na.rm = TRUE)  # same as sum()/6 but safer

# Common fill for all maps 
common_fill <- scale_fill_viridis_c(
  limits = c(0, 1),
  breaks = c(0, .25, .5, .75, 1),
  labels = label_percent(accuracy = 1),
  na.value = NA,
  guide = guide_colorbar(title = NULL)
)

plot_r <- function(r, title) {
  maxv <- as.numeric(terra::global(r, "max", na.rm = TRUE)[1, 1])
  
  cols <- c("grey80", viridis(256))                     # 0 -> grey, then dark blue → yellow
  vals <- c(0, seq(1e-6, 1, length.out = 256))          # length(vals) == length(cols)
  
  ggplot() +
    geom_spatraster(data = r) +
    scale_fill_gradientn(
      colours = cols,
      values  = vals,
      limits  = c(0, maxv),           # still covers 0–max (so grey shows on map)
      na.value = NA,
      oob = scales::squish,
      guide = guide_colorbar(
        title = NULL,
        barwidth = 0.35,              # narrower colorbar
        barheight = ,
        frame.colour = NA,
        # 👇 these breaks control what is shown on the legend
        ticks = TRUE
      ),
      breaks = pretty(c(3, maxv),n=5),  # 👈 legend starts at 1!
      labels = scales::number_format(accuracy = 2)
    ) +
    theme_minimal(base_size = 10) +
    labs(title = title, fill = NULL) +
    theme(
      axis.title = element_blank(),
      axis.text  = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(hjust = 0.5),
      legend.position = "right"
    )
}
# Panels (all are proportions 0–1)
p_smallg  <- plot_r(std_stack$small_geog,  "Small geographical range (prop)")
p_smalle  <- plot_r(std_stack$small_elev,  "Small elevational range (prop)")
p_hkh     <- plot_r(std_stack$hkh_only,    "HKH-only spp (prop)")
p_threatg <- plot_r(std_stack$threat_glob, "Threatened (global, prop)")
p_dd      <- plot_r(std_stack$dd,          "Data deficient (prop)")
p_threatn <- plot_r(std_stack$threat_nat,  "Threatened (national, prop)")




library(scales)

legend_0_100 <- scale_fill_viridis_c(
  limits = c(0, 1),                          # data are proportions
  breaks = seq(0, 1, by = 0.2),              # 0, .2, .4, .6, .8, 1
  labels = label_number(accuracy = 1, scale = 100),  # 0–100
  na.value = NA,
  guide = guide_colorbar(title = NULL, barwidth = 0.25, barheight = 5)
)

wrapped <- wrap_plots(
  p_threatg, p_dd, p_threatn,
  p_smallg, p_smalle, p_hkh,
  ncol = 3,   
  nrow = 2    
) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

wrapped

# show it

ggsave(
  filename = paste0(data_storage_path, "Datasets/species_list/species_richness/biodiv_dimensions_0918/HKH_biodiversity_maps_relativenumbers.png"),
  plot = wrapped,
  width = 14,
  height = 9,
  dpi = 300
)

#---------------------------------------------#
# plot PRIORITY areas (on absolute numbers)
#---------------------------------------------#
layers <- list(
  total_richness       = species_richness_total,  # 0..max
  small_geogr_range       = smallest_range,          # e.g. count of small-range spp
  small_elev_range        = elev_range,              # e.g. small elev. range metric
  hkh_only    = HKH_only,                # count or proportion
  threat_glob = threatened,              # threatened spp count
  data_deficient          = data_deficient,          # DD spp count
  threat_national  = nat_threatened           # nationally threatened spp count
)

std_minmax <- function(r){
  mm <- global(r, c("min","max"), na.rm=TRUE)
  (r - mm[1,1]) / (mm[1,2] - mm[1,1])
}


std_layers <- lapply(layers, std_minmax)  # or std_minmax
std_stack  <- rast(std_layers)                # multilayer SpatRaster
names(std_stack) <- names(layers)

plot(std_stack)


# priority area summary stack
sum_stack <- sum(std_stack)/7

x11()
plot(sum_stack)


# 0–1 scale, common breaks/labels
common_fill <- scale_fill_viridis_c(
  limits = c(0, 1),
  breaks = c(0, .25, .5, .75, 1),
  labels = label_percent(accuracy = 1),
  na.value = NA,
  guide = guide_colorbar(title = NULL)
)

plot_r <- function(r, title) {
  ggplot() +
    geom_spatraster(data = r) +
    common_fill +
    theme_minimal(base_size = 9) +
    labs(title = title) +
    theme(
      axis.title = element_blank(),
      axis.text  = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 10)
    )
}



# build plots
p_total   <- plot_r(std_stack$total_richness,     "Total richness")
p_smallg  <- plot_r(std_stack$small_geogr_range,  "Small geographical range")
p_smalle  <- plot_r(std_stack$small_elev_range,   "Small elevational range")
p_hkh     <- plot_r(std_stack$hkh_only,           "HKH only")
p_threatg <- plot_r(std_stack$threat_glob,        "Threatened (global)")
p_dd      <- plot_r(std_stack$data_deficient,     "Data deficient")
p_threatn <- plot_r(std_stack$threat_national,    "Threatened (national)")
p_sum     <- plot_r(sum_stack,                    "Summed areas (equal weight)")

# 3x3 layout, bigger center, single legend
layout <- "
ABC
D E
FGH
"

wrapped <- wrap_plots(
  A = p_sum, B = p_smallg, C = p_smalle,
  D = p_hkh,   E = p_total,    F = p_threatg,
  G = p_dd,    H = p_threatn,
  design = layout
) +
  plot_layout(widths = c(1, 1.6, 1), heights = c(1, 1.6, 1), guides = "collect") &
  theme(legend.position = "right")


# optional: save a high-res imageggsave
ggsave(
  filename = paste0(data_storage_path, "Datasets/species_list/species_richness/biodiv_dimensions_0918/HKH_biodiversity_maps_norm.png"),
  plot = wrapped,
  width = 14,
  height = 9,
  dpi = 300
)







plot_r <- function(r, title) {
  ggplot() +
    geom_spatraster(data = r) +
    common_fill +
    theme_minimal(base_size = 9) +
    labs(title = title) +
    theme(
      axis.title = element_blank(),
      axis.text  = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 10)
    )
}


# Panels (all are proportions 0–1)
p_smallg  <- plot_r(std_stack$small_geog,  "Small geographical range (prop)")
p_smalle  <- plot_r(std_stack$small_elev,  "Small elevational range (prop)")
p_hkh     <- plot_r(std_stack$hkh_only,    "HKH-only spp (prop)")
p_threatg <- plot_r(std_stack$threat_glob, "Threatened (global, prop)")
p_dd      <- plot_r(std_stack$dd,          "Data deficient (prop)")
p_threatn <- plot_r(std_stack$threat_nat,  "Threatened (national, prop)")


# --- Layout: big center, six around, two spacers ---
layout <- "
ABC
DEF
"

wrapped <- wrap_plots(
  A = p_smallg,  B = p_smalle,  
  C = p_hkh, D = p_threatg,
  E = p_dd, F = p_threatn, 
  design = layout
) +
  plot_layout(
    widths  = c(1, 1, 1),   # center column bigger
    heights = c(1, 1, 1),   # center row bigger
    guides  = "collect"       # single shared legend
  ) &
  theme(legend.position = "right")

wrapped

# save
ggsave(
  filename = file.path(
    data_storage_path,
    "Datasets/species_list/species_richness/biodiv_dimensions_0918/HKH_biodiversity_maps_norm_by_richness.png"
  ),
  plot = wrapped,
  width = 14, height = 9, dpi = 300
)








