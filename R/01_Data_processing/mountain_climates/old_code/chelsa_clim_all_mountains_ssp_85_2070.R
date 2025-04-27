
#----------------------------------------------------------#
# 1. Set up  -----
#----------------------------------------------------------#
library(here)
library(terra)
library(ggside)
library(terra)
library(ggplot2)
library(purrr)
library(patchwork)

# Load configuration file
source(here::here("R/00_Config_file.R"))


#----------------------------------------------------------#
#         load chelsa dataand mountain range
#----------------------------------------------------------#

annual_temp <- rast(paste0(data_storage_path,"Datasets/Mountains/Chelsa/CHELSA_bio1_1981-2010_V.2.1.tif"))

annual_prec <- rast(paste0(data_storage_path,"Datasets/Mountains/Chelsa/CHELSA_bio12_1981-2010_V.2.1.tif"))


#######❗ choose one scenario

# this is for ssp85 2070-2100
annual_temp_ssp85 <- rast(paste0(data_storage_path,"Datasets/Mountains/Chelsa/CHELSA_bio1_2071-2100_mpi-esm1-2-hr_ssp585_V.2.1.tif"))

annual_prec_ssp85 <- rast(paste0(data_storage_path,"Datasets/Mountains/Chelsa/CHELSA_bio12_2071-2100_mpi-esm1-2-hr_ssp585_V.2.1.tif"))

# this is for ssp85 2040-2070
annual_temp_ssp85 <- rast(paste0(data_storage_path,"Datasets/Mountains/Chelsa/CHELSA_bio1_2041-2070_mpi-esm1-2-hr_ssp585_V.2.1.tif"))

annual_prec_ssp85 <- rast(paste0(data_storage_path,"Datasets/Mountains/Chelsa/CHELSA_bio12_2041-2070_mpi-esm1-2-hr_ssp585_V.2.1.tif"))


#source the gmba mountains
mountain_shapes <- sf::st_read(paste(data_storage_path,"Datasets/Mountains/GMBA_Levels/Level_03/GMBA_Inventory_v2.0_Level_03.shp", 
                                     sep = "/"))

mountain_range <- mountain_shapes|>
  filter(MapName=="Northern Andes")

# source alpine biome
alpine_shapes <- sf::st_read(paste(data_storage_path,"Datasets/Mountains/Alpine_Biome/alpine_biome.shp", 
                                   sep = "/"))

head(alpine_shapes)



# Define mountain selection
mountain_selection <- c("Himalaya", 
                        "Northern Andes", 
                        "Central Andes", 
                        "Central European Highlands", 
                        "Intermountain West",
                        #"Hindu Kush", 
                        "Ethiopian Highlands", 
                        #"Albertine Rift Mountain",
                        "South Island",
                        "North European Highlands",
                        "Tibetan Plateau",
                        #"Great Escarpment",
                        #"Malay Archipelago",
                        "Caucasus Mountains",
                        "East European Highlands",
                        "Rocky Mountains",
                        #"Pacific Coast Ranges",
                        "Eastern Rift mountains",
                        "Mexican Highlands")


#------------------------------------------------
# loop through mountain ranges
#----------------------------------------------------------#
# Store all plots
climate_plots <- list()
climate_shifts <- data.frame()

# Loop over each mountain range
for (mnt in mountain_selection) {
  cat("Processing:", mnt, "\n")
  
  # Subset polygon
  mountain_range <- alpine_shapes |>
    filter(Mntn_rn == mnt)
  
  # Crop and mask rasters
  temp_mountain <- mask(crop(annual_temp, mountain_range), mountain_range)
  prec_mountain <- mask(crop(annual_prec, mountain_range), mountain_range)
  temp_mountain_future <- mask(crop(annual_temp_ssp85, mountain_range), mountain_range)
  prec_mountain_future <- mask(crop(annual_prec_ssp85, mountain_range), mountain_range)
  
  # Skip if no data
  if (is.null(temp_mountain) || is.null(prec_mountain) ||
      is.null(temp_mountain_future) || is.null(prec_mountain_future)) {
    next
  }
  
  # Present stack
  climate_stack <- c(temp_mountain, prec_mountain)
  names(climate_stack) <- c("temperature", "precipitation")
  climate_df <- as.data.frame(climate_stack, na.rm = TRUE)
  if (nrow(climate_df) < 100) next  # skip if not enough data
  climate_df <- climate_df[sample(nrow(climate_df), min(10000, nrow(climate_df))), ]
  climate_df$period <- "Present"
  
  # Future stack
  climate_stack_future <- c(temp_mountain_future, prec_mountain_future)
  names(climate_stack_future) <- c("temperature", "precipitation")
  climate_df_future <- as.data.frame(climate_stack_future, na.rm = TRUE)
  if (nrow(climate_df_future) < 100) next
  climate_df_future <- climate_df_future[sample(nrow(climate_df_future), min(10000, nrow(climate_df_future))), ]
  climate_df_future$period <- "Future"
  
  # Combine
  climate_combined <- rbind(climate_df, climate_df_future)
  
  # Centroids
  centroids <- climate_combined |>
    group_by(period) |>
    summarise(
      mean_temp = mean(temperature, na.rm = TRUE),
      mean_prec = mean(precipitation, na.rm = TRUE)
    )
  
  # Arrow df
  arrow_df <- data.frame(
    x = centroids$mean_temp[centroids$period == "Present"],
    y = centroids$mean_prec[centroids$period == "Present"],
    xend = centroids$mean_temp[centroids$period == "Future"],
    yend = centroids$mean_prec[centroids$period == "Future"]
  )
  
  # Euclidean shift
  D <- sqrt((arrow_df$xend - arrow_df$x)^2 + (arrow_df$yend - arrow_df$y)^2)
  
  # Store distance
  climate_shifts <- rbind(climate_shifts, data.frame(Mountain = mnt, ClimateShift = D))
  
  # Plot
  p <- ggplot(climate_combined, aes(x = temperature, y = precipitation)) +
    stat_density_2d(aes(fill = period), geom = "polygon", contour = TRUE, bins = 5,
                    alpha = 0.4, color = NA) +
    scale_fill_manual(values = c("Present" = "#1f78b4", "Future" = "#e31a1c")) +
    
    geom_segment(data = arrow_df,
                 aes(x = x, y = y, xend = xend, yend = yend),
                 arrow = arrow(length = unit(0.3, "cm")), color = "black", size = 1) +
    
    geom_point(data = centroids, aes(x = mean_temp, y = mean_prec, color = period),
               size = 1, shape = 21, fill = "white", stroke = 1) +
    scale_color_manual(values = c("Present" = "#1f78b4", "Future" = "#e31a1c")) +
    
    #  marginal densities 
    geom_xsidedensity(aes(y = ..density.., fill = period), alpha = 0.5, position = "identity") +
    geom_ysidedensity(aes(x = ..density.., fill = period), alpha = 0.5, position = "identity") +
    
    scale_xsidey_continuous(breaks = NULL) +
    scale_ysidex_continuous(breaks = NULL) +
    
    labs(title = mnt,
         x = "Temperature (C)",
         y = "Precipitation (mm)",
         fill = "Period", color = "Centroid") +
    scale_x_continuous(limits = c(-22, 35)) +
    scale_y_continuous(limits = c(0, 7000)) +
    coord_fixed(ratio = 1/200) +
    theme_minimal() +
    theme(ggside.panel.scale = 0.2)  # controls the side plot size
  
  climate_plots[[mnt]] <- p
}

#----------------------------------------------------------#
#         safe plots 
#----------------------------------------------------------#
wrap_plots(climate_plots, guides = "collect") +
  plot_annotation(
    title = "Climate space shift in alpine areas: Present vs SSP8.5 (2070–2100)"
  ) &
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5)
  )

today <- format(Sys.Date(), "%Y%m%d")

# Define file path
output_path <- paste0(data_storage_path, "Outputs/Figures/mountain_climates/climate_space_shift_ssp85_2070_", today, ".jpg")

# Save the plot
ggsave(output_path,
       plot = last_plot(),      # or assign your full plot to a variable and use it here
       width = 14, height = 10, # adjust size as needed
       dpi = 300,               # high-quality output
       device = "jpeg")


# improvements:
# - add densities to top of the plots
# - add anotations of the climate shifts D value to each plot