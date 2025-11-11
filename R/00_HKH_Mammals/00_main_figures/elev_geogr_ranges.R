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

#---------------------------------------------#
# Step 1: list files
#---------------------------------------------#
source(here::here("R/00_Config_file_HKH.R"))

total_endemism_join <- read.csv(paste0(data_storage_path,"Datasets/species_list/species_endemism/total_species_endemism.csv"))|>
  filter(!is.na(pct_in_HKH_area), pct_in_HKH_area > 0)

species_list <- readxl::read_excel(paste0(data_storage_path,"Datasets/species_list/assessment_hkh_mammals_09082025_LS.xlsx"))|>
  rename("species"="sciname")


total_endemism_join_2 <- total_endemism_join|>
  left_join(species_list|>
              select(species,status_code_global,status_summary_global)|>
              distinct(),by="species")|>
  distinct()
#---------------------------------------------#
# select species with small area and elev range
#---------------------------------------------#
smallest_range <- total_endemism_join %>%
  slice_min(total_area_km2, prop = 0.40)

# 20% highest % HKH
most_hkh <- total_endemism_join %>%
  slice_max(pct_in_HKH_area, prop = 0.40)

smallest_elev <- total_endemism_join %>%
  slice_min(elev_range, prop = 0.40)

total_endemism_join_2$status_code_global <- factor(
  total_endemism_join_2$status_code_global,
  levels = c("CR", "EN", "VU", "NT", "LC", "NA")
)


# (Optional) ensure your levels are ordered as you want in the legend
total_endemism_join_2$status_summary_global <- factor(
  total_endemism_join_2$status_summary_global,
  levels = c("threatened", "not threatened", "data deficient", "NA")
)

x11()
elevareaplot <- ggplot(total_endemism_join_2, 
                       aes(x = log10(total_area_km2), 
                           y = elev_range, 
                           color = status_summary_global)) +
  geom_jitter(width = 0.03, height = 20, alpha = 0.7, size = 3) +
  scale_color_manual(
    name = "IUCN Status Summary (Global)",
    values = c(
      "threatened" = "darkred",
      "not threatened" = "yellow",
      "data deficient" = "black",
      "NA" = "darkgrey"
    ),
    breaks = c("threatened", "not threatened", "data deficient", "NA"),
    labels = c(
      "threatened" = "Threatened",
      "not threatened" = "Not Threatened",
      "data deficient" = "Data Deficient",
      "NA" = "Not Assessed by IUCN"
    )
  ) +
  labs(
    y = "Elevational range",
    x = "log10 (area size)"
  ) +
  theme_minimal(base_size = 16) +         # sets base font size
  theme(
    axis.title = element_text(size = 18), # axis titles
    axis.text = element_text(size = 14),  # axis tick labels
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  )
print(elevareaplot)


ggsave(
  filename = paste0(data_storage_path, "Datasets/species_list/species_richness/biodiv_dimensions_0918/elev_area_plot.png"),
  plot = elevareaplot,
  width = 10,
  height = 9,
  dpi = 300
)



overlap_species <- intersect(smallest_elev$species, smallest_range$species)

overlap_species <- list(
  smallest_elev$species,
  smallest_range$species,
  most_hkh$species
) %>% 
  reduce(intersect)

# How many?
length(overlap_species)
length(smallest_10pct_elev$species)

length(overlap_species) / nrow(smallest_elev) * 100

# Which ones?
overlap_species
