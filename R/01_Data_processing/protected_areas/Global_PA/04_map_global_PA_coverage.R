#----------------------------------------------------------#
# 1. Set up  -----
#----------------------------------------------------------#
library(here)
library(tidyverse)
library(sf)
library(ggplot2)
library(viridis)


source(here::here("R/00_Config_file.R"))

#------------------------------------#
# Load PA coverage data 
#-------------------------------------#

# load all csvs in the folder for PA coverage
PA_coverage_data_path <- paste0(data_storage_path, "Outputs/protected_areas/PA_coverage/")

# List all .csv files in the folder
csv_files <- list.files(path = PA_coverage_data_path, pattern = "\\.csv$", full.names = TRUE)

# Read and bind all CSVs into one dataframe
PA_coverage_df <- csv_files |>
  lapply(read.csv) |>         
  bind_rows()     |>            
  replace_na(list(IUCN_ct = "NA_NR")) # recode NAs as NA _ NR (not applicable by IUCN or not reported)

#------------------------------------#
# barplot the data 
#-------------------------------------#


plot <- ggplot(PA_coverage_df, aes(x = Mountain_range, y = percent_protected, fill = IUCN_ct)) +
  geom_col(position = position_dodge(width = 0.6)) +
  scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.05))) +
  scale_fill_viridis_d(option = "inferno", name = "IUCN category",direction=-1) +
  facet_grid(. ~ Range, scales = "free_x", space = "free_x") + 
  labs(
    x = NULL,
    y = "% area protected"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )+ 
  geom_hline(yintercept = 30, linetype = "dashed", color = "grey30") +
  annotate("text", x = Inf, y = 30, label = "CBD target 30% by 2030", 
           hjust = 1.5, vjust = -0.5, size = 3, color = "grey30")

x11()
plot(plot)

#----------------------------------------------------------#
#   save the plot
#----------------------------------------------------------#
today <- format(Sys.Date(), "%Y%m%d")

# Define file path for PNG
plot_path <- paste0(data_storage_path, "Outputs/protected_areas/PA_coverage", today, ".jpeg")

# Save last plot as PNG
ggsave(filename = plot_path, plot=plot,width = 12, height = 6, dpi = 300)





