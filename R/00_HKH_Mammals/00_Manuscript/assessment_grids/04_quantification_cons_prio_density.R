#---------------------------------------------#
# Packages
#---------------------------------------------#
library(terra)
library(sf)
library(tidyterra)
library(ggplot2)
library(dplyr)

source(here::here("R/00_Config_file_HKH.R"))

#---------------------------------------------#
# Input data
#---------------------------------------------#
biodiv_imp <- rast(file.path(data_storage_path, "Output/biodiv_dimensions/biodiv_imp.tif"))
uniqueness <- rast(paste0(data_storage_path, "Output/uniqueness/r_lcbd.tif"))
phylo_div <- rast(paste0(data_storage_path, "Output/phylogenetic_diversity/PD_raster.tif"))
climate_distance <- rast(file.path(data_storage_path, "Datasets/climate_shift/clim_shift_eucl.tif"))
pa_raster <- rast(file.path(data_storage_path, "Datasets/protected_areas/protected_raster.tif"))
smallest_range<-rast(paste0(data_storage_path, "Datasets/species_list/species_richness/biodiversity_dimensions/smallest_range_median.tif"))

library(terra)
library(dplyr)
library(tidyr)
library(ggplot2)

# -----------------------------
# 1. Load rasters
# -----------------------------
biodiv_imp <- rast(file.path(data_storage_path, "Output/biodiv_dimensions/biodiv_imp.tif"))
uniqueness <- rast(file.path(data_storage_path, "Output/uniqueness/r_lcbd.tif"))
phylo_div <- rast(file.path(data_storage_path, "Output/phylogenetic_diversity/PD_raster.tif"))
climate_distance <- rast(file.path(data_storage_path, "Datasets/climate_shift/clim_shift_eucl.tif"))
pa_raster <- rast(file.path(data_storage_path, "Datasets/protected_areas/protected_raster.tif"))
smallest_range <- rast(file.path(data_storage_path, "Datasets/species_list/species_richness/biodiversity_dimensions/smallest_range_median.tif"))
threatened<-rast(paste0(data_storage_path, "Datasets/species_list/species_richness/biodiversity_dimensions/globally_threathened.tif"))


# -----------------------------
# 2. Align all rasters to template
# -----------------------------
template <- biodiv_imp

uniqueness <- resample(uniqueness, template, method = "bilinear")
phylo_div <- resample(phylo_div, template, method = "bilinear")
climate_distance <- resample(climate_distance, template, method = "bilinear")
smallest_range <- resample(smallest_range, template, method = "bilinear")
threatened <- resample(threatened, template, method = "bilinear")
pa_raster <- resample(pa_raster, template, method = "near")



# -----------------------------
# 3. Build dataframe
# -----------------------------
df <- data.frame(
  biodiv_imp = values(biodiv_imp, mat = FALSE),
  uniqueness = values(uniqueness, mat = FALSE),
  phylo_div = values(phylo_div, mat = FALSE),
  climate_distance = values(climate_distance, mat = FALSE),
  smallest_range = values(smallest_range, mat = FALSE),
  threatened = values(threatened, mat = FALSE),
  pa = values(pa_raster, mat = FALSE)
) %>%
  filter(
    !is.na(biodiv_imp),
    !is.na(uniqueness),
    !is.na(phylo_div),
    !is.na(climate_distance),
    !is.na(smallest_range),
    !is.na(threatened),
    !is.na(pa)
  ) %>%
  mutate(
    pa = factor(pa, levels = c(0, 1), labels = c("Outside PA", "Inside PA"))
  )

# -----------------------------
# 4. Convert to long format
# -----------------------------
df_long <- df %>%
  select(pa, biodiv_imp, uniqueness, phylo_div, climate_distance, smallest_range, threatened) %>%
  pivot_longer(
    cols = c(biodiv_imp, uniqueness, phylo_div, climate_distance, smallest_range, threatened),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric = recode(
      metric,
      biodiv_imp = "Biodiversity importance",
      uniqueness = "Uniqueness",
      phylo_div = "Phylogenetic diversity",
      climate_distance = "Climate distance",
      smallest_range = "Small-ranged richness",
      threatened = "Threatened richness"
    )
  )

# optional: control panel order
df_long$metric <- factor(
  df_long$metric,
  levels = c(
    "Biodiversity importance",
    "Uniqueness",
    "Phylogenetic diversity",
    "Small-ranged richness",
    "Threatened richness",
    "Climate distance"
  )
)

# -----------------------------
# 5. Median lines
# -----------------------------
medians <- df_long %>%
  group_by(metric, pa) %>%
  summarise(
    med = median(value, na.rm = TRUE),
    .groups = "drop"
  )

# -----------------------------
# 6. Distribution shape plot
# -----------------------------
p_density <- ggplot(df_long, aes(x = value, fill = pa, color = pa)) +
  geom_density(alpha = 0.5, adjust = 1) +
  geom_vline(
    data = medians,
    aes(xintercept = med, color = pa),
    linewidth = 0.6,
    show.legend = FALSE
  ) +
  facet_wrap(~ metric, scales = "free", nrow = 2) +
  scale_fill_manual(values = c("Outside PA" = "#E69F00", "Inside PA" = "#C44E52")) +
  scale_color_manual(values = c("Outside PA" = "#E69F00", "Inside PA" = "#C44E52")) +
  labs(
    x = NULL,
    y = "Density",
    fill = NULL,
    color = NULL
  ) +
  theme_classic() +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "right"
  )

p_density

# -----------------------------
# 7. histogram plots 
# -----------------------------
p_count <- ggplot(df_long, aes(x = value, fill = pa, color = pa)) +
  geom_histogram(aes(y = after_stat(count)),
                 position = "identity", alpha = 0.3, bins = 40) +
  facet_wrap(~ metric, scales = "free", nrow = 2) +
  scale_fill_manual(values = c("Outside PA" = "#C2B280", "Inside PA" = "#2F5D62")) +
  scale_color_manual(values = c("Outside PA" = "#C2B280", "Inside PA" = "#2F5D62")) +
  labs(
    x = NULL,
    y = "Number of cells",
    fill = NULL,
    color = NULL
  ) +
  theme_classic() +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "right"
  )

p_count

x11()
plot(p_count)

ggsave(
  filename = file.path(data_storage_path, "Output/priority_indices/counts_pa.png"),
  plot = p_count,
  width = 14,
  height = 9,
  dpi = 300
)

ggsave(
  filename = file.path(data_storage_path, "Output/priority_indices/counts_pa.png"),
  plot = p_count,
  width = 14,
  height = 9,
  dpi = 300
)
