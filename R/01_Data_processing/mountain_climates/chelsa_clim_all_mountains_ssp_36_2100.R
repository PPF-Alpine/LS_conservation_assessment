
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


# ❗ choose one scenario

# this is for ssp85 2070-2100
annual_temp_ssp37 <- rast(paste0(data_storage_path,"Datasets/Mountains/Chelsa/CHELSA_bio1_2071-2100_mpi-esm1-2-hr_ssp370_V.2.1.tif"))

annual_prec_ssp37 <- rast(paste0(data_storage_path,"Datasets/Mountains/Chelsa/CHELSA_bio12_2071-2100_mpi-esm1-2-hr_ssp370_V.2.1.tif"))


#------------------------------------------------
# source mountains and alpine
#----------------------------------------------------------#
#source the gmba mountains
mountain_shapes <- sf::st_read(paste(data_storage_path,"Datasets/Mountains/GMBA_Levels/Level_03/GMBA_Inventory_v2.0_Level_03.shp", 
                                     sep = "/"))


# source alpine biome
alpine_shapes <- sf::st_read(paste(data_storage_path,"Datasets/Mountains/Alpine_Biome/alpine_biome.shp", 
                                   sep = "/"))

head(alpine_shapes)



# Define mountain selection
mountain_selection <- alpine_shapes$Mntn_rn |> unique() |> sort()

#------------------------------------------------
# loop through mountain ranges
#----------------------------------------------------------#
# Store all plots
climate_plots <- list()
climate_shifts <- data.frame()
climate_combined_all <- data.frame()

for (mnt in mountain_selection) {
  cat("Processing:", mnt, "\n")
  
  mountain_range <- alpine_shapes |>
    filter(Mntn_rn == mnt)
  
  temp_mountain <- mask(crop(annual_temp, mountain_range), mountain_range)
  prec_mountain <- mask(crop(annual_prec, mountain_range), mountain_range)
  temp_mountain_future <- mask(crop(annual_temp_ssp37, mountain_range), mountain_range)
  prec_mountain_future <- mask(crop(annual_prec_ssp37, mountain_range), mountain_range)
  
  if (is.null(temp_mountain) || is.null(prec_mountain) ||
      is.null(temp_mountain_future) || is.null(prec_mountain_future)) {
    next
  }
  
  climate_stack <- c(temp_mountain, prec_mountain)
  names(climate_stack) <- c("temperature", "precipitation")
  climate_df <- as.data.frame(climate_stack, na.rm = TRUE)
  if (nrow(climate_df) < 100) next
  climate_df <- climate_df[sample(nrow(climate_df), min(10000, nrow(climate_df))), ]
  climate_df$period <- "Present"
  
  climate_stack_future <- c(temp_mountain_future, prec_mountain_future)
  names(climate_stack_future) <- c("temperature", "precipitation")
  climate_df_future <- as.data.frame(climate_stack_future, na.rm = TRUE)
  if (nrow(climate_df_future) < 100) next
  climate_df_future <- climate_df_future[sample(nrow(climate_df_future), min(10000, nrow(climate_df_future))), ]
  climate_df_future$period <- "Future"
  
  # Combine and add mountain name
  climate_combined <- bind_rows(climate_df, climate_df_future) |>
    mutate(
      Mountain = mnt,
      temperature_z = scale(temperature)[, 1],
      precipitation_z = scale(precipitation)[, 1]
    )
  
  # Save full combined df with z-scores
  climate_combined_all <- bind_rows(climate_combined_all, climate_combined)
  
  # Centroids (original scale)
  centroids <- climate_combined |>
    group_by(period) |>
    summarise(
      mean_temp = mean(temperature, na.rm = TRUE),
      mean_prec = mean(precipitation, na.rm = TRUE)
    )
  
  # Centroids (z-score scale)
  centroids_z <- climate_combined |>
    group_by(period) |>
    summarise(
      mean_temp_z = mean(temperature_z, na.rm = TRUE),
      mean_prec_z = mean(precipitation_z, na.rm = TRUE)
    )
  
  # Euclidean distance in original space
  D <- sqrt((centroids$mean_temp[2] - centroids$mean_temp[1])^2 +
              (centroids$mean_prec[2] - centroids$mean_prec[1])^2)
  
  # Euclidean distance in standardized (z-score) space
  D_z <- sqrt((centroids_z$mean_temp_z[2] - centroids_z$mean_temp_z[1])^2 +
                (centroids_z$mean_prec_z[2] - centroids_z$mean_prec_z[1])^2)
  
  # Store shift metrics
  climate_shifts <- bind_rows(climate_shifts, tibble(
    Mountain = mnt,
    ClimateShift = D,
    ClimateShift_z = D_z
  ))
  
  # Plot
  p <- ggplot(climate_combined, aes(x = temperature, y = precipitation)) +
    stat_density_2d(aes(fill = period), geom = "polygon", contour = TRUE, bins = 5,
                    alpha = 0.4, color = NA) +
    scale_fill_manual(values = c("Present" = "#1f78b4", "Future" = "#e31a1c")) +
    
    geom_segment(data = centroids,
                 aes(x = mean_temp[1], y = mean_prec[1],
                     xend = mean_temp[2], yend = mean_prec[2]),
                 arrow = arrow(length = unit(0.3, "cm")), color = "black", size = 1) +
    
    geom_point(data = centroids, aes(x = mean_temp, y = mean_prec, color = period),
               size = 1, shape = 21, fill = "white", stroke = 1) +
    scale_color_manual(values = c("Present" = "#1f78b4", "Future" = "#e31a1c")) +
    
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
    theme(ggside.panel.scale = 0.2)
  
  climate_plots[[mnt]] <- p
}



#----------------------------------------------------------#
#         safe plots 
#----------------------------------------------------------#
# Split into chunks of 16 plots
plots_per_page <- 16
plot_chunks <- split(climate_plots, ceiling(seq_along(climate_plots) / plots_per_page))

# Create folder if not exists
output_folder <- file.path(data_storage_path, "Outputs/Figures/mountain_climates")
dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)

# Date for filename
today <- format(Sys.Date(), "%Y%m%d")

# Loop and save each chunk
for (i in seq_along(plot_chunks)) {
  page_plots <- plot_chunks[[i]]
  
  # Remove legend from each individual plot
  page_plots_nolegend <- lapply(page_plots, function(p) p + theme(legend.position = "none"))
  
  # Combine and annotate
  p <- wrap_plots(page_plots_nolegend, ncol = 4) +
    plot_annotation(
      title = paste("Climate space shift in alpine areas (Page", i, ")"),
      theme = theme(
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5)
      )
    )
  
  # Save
  ggsave(
    filename = file.path(output_folder, paste0("climate_space_shift_ssp370_page_", i, "_", today, ".jpg")),
    plot = p,
    width = 14, height = 10,
    dpi = 300, device = "jpeg"
  )
}

#---------------------------------------------------------------#
# write to csv --
#----------------------------------------------------------------#

today <- format(Sys.Date(), "%Y%m%d")

# Define file path
output_path <- paste0(data_storage_path, "Outputs/mountain_climates/all_mountains_climate_shifts_ssp370_2100_", today, ".csv")

write.csv(climate_shifts,output_path)

# Define file path
output_path <- paste0(data_storage_path, "Outputs/mountain_climates/climate_combined_all_ssp370_2100_", today, ".csv")

write.csv(climate_combined_all,output_path)
