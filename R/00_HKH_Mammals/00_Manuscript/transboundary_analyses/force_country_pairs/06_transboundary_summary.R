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

fullborder_sum <- sf::st_read(paste0(data_storage_path, "Output/transboundary/full_borderlenght_countrypairs.gpkg"))

fullborder <- sf::st_read(paste0(data_storage_path, "Output/transboundary/fullborderlength_consprio_biodivimp.gpkg"))

transb_species <- sf::st_read(paste0(data_storage_path, "Output/transboundary/transb_glob_threat_new.shp"))|>
  select(seg_id,n_shard)|>
  rename(n_shared=n_shard
  )|>
  sf::st_drop_geometry()

#---------------------------------------------#
# how much border protection overall
#---------------------------------------------#
fullborder_summary <- fullborder |>
 filter(!is.na(value)) |>
  mutate(
    protection= if_else(
      value %in% c(111,121,131,211,221,231,311,321,331),
      "protected",
      "unprotected"
    )
  ) |>
  summarise(
    total_cells_n = n(),
    protected_cells_n = sum(protection == "protected"),
    unprotected_cells_n = sum(protection == "unprotected"),
    prop_protected = protected_cells_n / total_cells_n * 100,
    prop_unprotected = unprotected_cells_n / total_cells_n * 100
  )

# 15 % protected
# 85 % unprotected 


#---------------------------------------------#
# how much of each side is protected unprotected in whole hkh
#---------------------------------------------#
library(dplyr)
library(tidyr)

# 1. classify protection and summarise per segment side
side_summary <- fullborder |>
  filter(!is.na(value)) |>
  mutate(
    protected = value %% 10 == 1
  ) |>
  group_by(seg_id, side, country) |>
  summarise(
    n_cells = n(),
    n_protected = sum(protected),
    prop_protected = n_protected / n_cells * 100,
    .groups = "drop"
  )

# 2. put side A and B on the same row
side_compare <- side_summary |>
  pivot_wider(
    names_from = side,
    values_from = c(country, n_cells, n_protected, prop_protected),
    names_sep = "_"
  ) |>
  mutate(
    pair_id = paste(country_A, country_B, sep = "_"),
    border_status = case_when(
      prop_protected_A > 0 & prop_protected_B > 0 ~ "both sides protected",
      prop_protected_A > 0 & prop_protected_B == 0 ~ "only side A protected",
      prop_protected_A == 0 & prop_protected_B > 0 ~ "only side B protected",
      TRUE ~ "neither side protected"
    )
  )


border_status_hkh <- side_compare |>
  count(border_status) |>
  mutate(
    prop = n / sum(n) * 100
  )
