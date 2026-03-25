#---------------------------------------------#
# Step 1: list files
#---------------------------------------------#
source(here::here("R/00_Config_file_HKH.R"))

library(dplyr)
library(sf)
library(plotly)
library(RColorBrewer)


#---------------------------------------------#
# get border segments, countries and cons priority
#---------------------------------------------#

border_segments_cons_prio <- sf::st_read(paste0(data_storage_path, "Output/transboundary/border_segments_consprio_biodivimp.gpkg"))

transb_species <- sf::st_read(paste0(data_storage_path, "Output/transboundary/transb_glob_threat_new.shp"))



highbio_weights <- data.frame(
  value = c(311, 310, 321, 320, 331, 330),
  score = c(1, 2, 2, 4, 3, 6)
)

prio_scored <- border_segments_cons_prio %>%
  left_join(highbio_weights, by = c("ID" = "value")) %>%
  filter(!is.na(score))



seg_side_score <- prio_scored %>%
  group_by(seg_id, country, side) %>%
  summarise(
    mean_score = mean(score, na.rm = TRUE),
    .groups = "drop"
  )

seg_diff <- seg_side_score %>%
  pivot_wider(
    names_from = side,
    values_from = c(country, mean_score)
  ) %>%
  mutate(
    diff_score = abs(mean_score_A - mean_score_B)
  )

seg_diff
