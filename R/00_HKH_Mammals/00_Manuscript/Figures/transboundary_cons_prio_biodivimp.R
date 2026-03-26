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
# remove NA values
# -----------------------------
prio_extract_noNA <- prio_extract %>%
  filter(!is.na(value))

# ---------------------------
# count number of pixels per category for each segment side
# -----------------------------
prio_counts_long <- prio_extract_noNA %>%
  count(seg_id, country, side, value, name = "n_cells") %>%
  group_by(seg_id, country, side) %>%
  mutate(
    median_value = value[which(cumsum(n_cells) >= sum(n_cells) / 2)[1]]
  ) %>%
  ungroup()

# -----------------------------
# total extracted pixels per segment side
# -----------------------------
prio_total_pixels <- prio_extract_noNA %>%
  count(seg_id, country, side, name = "n_cells_total")

# -----------------------------
# one column per raster category
# -----------------------------
prio_counts_wide <- prio_counts_long %>%
  select(seg_id, country, side, value, n_cells, median_value) %>%
  mutate(value_col = paste0("cells_", value)) %>%
  select(-value) %>%
  pivot_wider(
    names_from = value_col,
    values_from = n_cells,
    values_fill = 0
  )

# -----------------------------
# combine
# -----------------------------
prio_summary <- prio_total_pixels %>%
  left_join(prio_counts_wide, by = c("seg_id", "country", "side")) %>%
  distinct(seg_id, country, side, .keep_all = TRUE)


# -----------------------------
# weighing scheme 
# -----------------------------
# dataframe that corresponds to the different classes 
levels_df <- data.frame(
  value = c(111,110,121,120,131,130,
            211,210,221,220,231,230,
            311,310,321,320,331,330),
  biodiv = rep(c("low","medium","high"), each = 6),
  climate = rep(c("low","low","medium","medium","high","high"), times = 3),
  protection = rep(c("protected","unprotected"), 9)
)

levels_df$class_name <- paste(
  "Biodiv:", levels_df$biodiv,
  "| Climate:", levels_df$climate,
  "|", levels_df$protection
)

levels_df <- levels_df %>%
  mutate(
    biodiv_score = c(low = 1, medium = 2, high = 3)[biodiv],
    climate_score = c(low = 1, medium = 2, high = 3)[climate],
    priority_score = biodiv_score + climate_score,
    protection_score = if_else(protection == "protected", 1, 0)
  )

# comparison looses detail because its only median !! --> add this to ppt explanation 
## 
comparison_df <- prio_summary|>
  select(seg_id,country,side,n_cells_total,median_value)|>
  left_join(levels_df,by=c("median_value"="value"))|>
  select(-class_name)|>
  left_join(transb_species,by="seg_id")

library(dplyr)
library(ggplot2)

comparison_df2 <- comparison_df %>%
  group_by(seg_id) %>%
  mutate(country_pair = paste(sort(unique(country)), collapse = " - ")) %>%
  ungroup()|>
  filter(country_pair!="India")



plot <- ggplot(comparison_df2,
       aes(x = priority_score,
           y = reorder(factor(seg_id), n_shared),
           group = seg_id)) +
  geom_line(aes(size = n_shared), color = "grey80", alpha = 0.7) +
  geom_point(aes(color = protection, size = n_shared), alpha = 0.7) +
  scale_color_manual(
    values = c(
      "protected" = "forestgreen",
      "unprotected" = "darkred"
    )
  ) +
  scale_size_area(
    max_size = 8,
    breaks = c(1, 5, 10, 20, 31),
    name = "Shared threatened species"
  ) +
  guides(
    size = guide_legend(
      override.aes = list(alpha = 1),
      title.position = "top",
      label.position = "right"
    )
  ) +
  facet_wrap(~ country_pair, scales = "free_y") +
  labs(
    x = "Priority score",
    y = "Border segment",
    color = "Protection"
  ) +
  theme_minimal()


ggsave(
  filename = paste0(
    data_storage_path,
    "Output/transboundary/transboundary_cons_prio_biodiv.jpeg"
  ),
  plot = plot,
  width = 14,
  height = 12,
  dpi = 300
)

