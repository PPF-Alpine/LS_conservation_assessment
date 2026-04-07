# pkgs
library(terra)        # you already use this
library(tidyterra)    # ggplot geoms for terra rasters
library(ggplot2)
library(patchwork)    # to arrange plots

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

transb_species <- sf::st_read(paste0(data_storage_path, "Output/transboundary/transb_glob_threat_new.shp"))|>
  select(seg_id,n_shard)|>
  rename(n_shared=n_shard
  )|>
  sf::st_drop_geometry()

# -----------------------------
# filter and summarise cells 
# -----------------------------

prio_extract_filtered <- border_segments_cons_prio|>
  filter(!is.na(sum)) |>
  group_by(seg_id) |>
  mutate(total_cells_n = n()) |>
  ungroup()


# summarise values for border segments 
value_summary <- prio_extract_filtered |>
  filter(!is.na(sum)) |>
  group_by(seg_id, side, sum,country,pair_id) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(seg_id, side) |>
  mutate(
    total_cells_n = sum(n),
    prop = round(n / total_cells_n * 100, 4)
  ) |>
  ungroup()|>
  mutate(
    protection = if_else(
      sum %in% c(111,121,131,211,221,231,311,321,331),
      "protected",
      "unprotected"
    )
  )|>
  group_by(seg_id, side, country) |>
  mutate(
    prop_protected = sum(prop[protection == "protected"]),
    prop_unprotected = sum(prop[protection == "unprotected"])
  ) |>
  ungroup()


# -----------------------------
# plot border segments comparison data prep
# -----------------------------

plot_data <- value_summary |>
  distinct(seg_id, side, country, pair_id, prop_protected, prop_unprotected) |>
  pivot_longer(
    cols = c(prop_protected, prop_unprotected),
    names_to = "protection",
    values_to = "prop"
  ) |>
  mutate(
    protection = recode(
      protection,
      prop_protected = "protected",
      prop_unprotected = "unprotected"
    )
  )


# summarise the pairs (several border segments for same country pair present)
segment_compare <- value_summary |>
  distinct(seg_id, pair_id, side, country, prop_protected) |>
  pivot_wider(
    names_from = side,
    values_from = c(country, prop_protected),
    names_sep = "_"
  )

# -----------------------------
# plot 
# -----------------------------
library(ggplot2)

ggplot(segment_compare,
       aes(x = prop_protected_A, y = prop_protected_B)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  facet_wrap(~ pair_id) +
  labs(
    x = "Protection (%) - Side A",
    y = "Protection (%) - Side B"
  ) +
  theme_minimal()
