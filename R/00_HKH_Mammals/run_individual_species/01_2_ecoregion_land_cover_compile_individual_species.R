library(terra)
library(dplyr)
library(stringr)
library(readxl)

# Load configuration file
source(here::here("R/00_Config_file_HKH.R"))

#----------------------------------------------------------#
# 0. load files  -----
#----------------------------------------------------------#
lc_data_descr <- readxl::read_excel(paste0(data_storage_path,"Datasets/land_cover/lc_data_description.xlsx"))
# Define the folder path
folder_path <- paste0(data_storage_path, "Datasets/species_list/rasterfiles/00_mammal_summaries/")

# List all CSV files in the folder
csv_files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

# Read all CSVs into a list of data frames
csv_list <- lapply(csv_files, read.csv)

# Optionally combine them all into a single data frame (if they have the same columns)
all_data <- do.call(rbind, csv_list)

#----------------------------------------------------------#
# plot land cover data   -----
#----------------------------------------------------------#
# 9 974F6B = grassland
# 8 6151E5 = bare rock
# 7 F3FA69 = bare soil
# 6 7E8A40 = cropland
# 5 FB63D4 = riverbed
# 4 8CF0F1 = built up area
# 3 D6604C = forest
# 2 64E954 = snow and glacier
# 1 545FA7 = waterbody


# Filter only land cover data
df_land <- all_data %>% 
  filter(source == "land_cover")|>
  filter(category!= "NA")

# Violin plot with jittered points
plot<-ggplot(df_land, aes(x = category, y = proportion)) +
  geom_boxplot(fill = "lightblue", alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.4, color = "black", size = 1) +
  labs(
    x = NULL,
    y = "Proportion of range covered by LC type (%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.y = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    panel.grid.minor = element_blank()
  )

# Define your colors in the correct order
lc_colors <- c(
  "water body"      = "#545FA7",
  "snow and glacier"= "#64E954",
  "forest"          = "#D6604C",
  "built-up area"   = "#8CF0F1",
  "riverbed"        = "#FB63D4",
  "cropland"        = "#7E8A40",
  "bare soil"       = "#E6E600",
  "bare rock"       = "#6151E5",
  "grassland"       = "#974F6B"
)

# Boxplot with your custom palette
# Make sure factor levels match the palette order
df_land$category <- factor(df_land$category, levels = names(lc_colors))

# Plot
plot <- ggplot(df_land, aes(x = category, y = proportion, fill = category)) +
  geom_boxplot(alpha = 0.7, color = "black", outlier.shape = NA) +
  
  # 2️⃣ Main colored points on top
  geom_jitter(aes(color = category),
              width = 0.2, size = 1, alpha = 0.8) +
  
  scale_fill_manual(values = lc_colors) +
  scale_color_manual(values = lc_colors) +
  
  labs(
    x = NULL,
    y = "Proportion of range covered by LC type (%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x  = element_text(size = 14, angle = 60, hjust = 1),
    axis.text.y  = element_text(size = 14),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )

plot


plot

ggsave(
  filename = paste0(data_storage_path, "Datasets/land_cover/land_cover_individ_species.png"),
  plot = plot,
  width = 14,
  height = 9,
  dpi = 300
)

# Filter only land cover data
df_eco <- all_data %>% 
  filter(source == "ecoregion")|>
  filter(category!= "NA")

#
ggplot(df_eco, aes(x = category, y = proportion)) +
  geom_boxplot(fill = "lightblue", alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.4, color = "black", size = 1) +
  labs(
    x = "WWF ecoregion",
    y = "Proportion of range covered by ecoregion (%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, size = 9),
    panel.grid.minor = element_blank()
  )

