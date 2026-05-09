
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)
library(dplyr)
library(purrr)


source(here::here("R/00_Config_file_HKH.R"))
#---------------------------------------------#
# Plot preparations 
#---------------------------------------------#

# to only run the plots : 
pa_distance_coverage_summary <- sf::st_read(paste0(data_storage_path, "Datasets/transboundary/GADM/pairwise_border_buffers/distances/pa_distance_coverage.gpkg")) #|> sf::st_drop_geometry()


ndlsa_lookup <- readxl::read_xlsx(paste0(data_storage_path, "Datasets/transboundary/GADM/ndlsa_clean.xlsx"))
ndlsa_lookup <- setNames(ndlsa_lookup$ndlsa_name, ndlsa_lookup$GID_0)


country_area <- countries_all |>
  st_transform(6933) |>   # equal-area projection
  group_by(GID_0) |>
  summarise(geometry = st_union(geometry)) |>
  mutate(country_area_km2 = as.numeric(st_area(geometry)) / 1e6) |>
  st_drop_geometry()

coverage_df <- readxl::read_xlsx(paste0(data_storage_path, "Datasets/transboundary/GADM/country_share_borderbuffers.xlsx"))|>
  mutate(
    GID_0 = dplyr::recode(GID_0, !!!ndlsa_lookup)
  )
  

#---------------------------------------------#
# data rearangements 
#---------------------------------------------#
# internal: one point per PA on its own side
internal_points <- pa_distance_coverage_summary |>
  distinct(pair_id, border_side, WDPAID, .keep_all = TRUE) |>
  transmute(
    pair_id,
    border_side,
    comparison = "Internal",
    point_id = WDPAID,
    distance_km = nearest_internal_km
  )

# across: one point per unique nearest across-border PA
across_points <- pa_distance_coverage_summary |>
  filter(!is.na(nearest_across_WDPAID)) |>
  group_by(pair_id, border_side, nearest_across_WDPAID) |>
  summarise(
    distance_km = min(nearest_across_km, na.rm = TRUE),
    .groups = "drop"
  ) |>
  transmute(
    pair_id,
    border_side,
    comparison = "Across border",
    point_id = nearest_across_WDPAID,
    distance_km = distance_km
  )


points_df <- bind_rows(internal_points, across_points) |>
  filter(!is.na(distance_km))


points_df <- points_df |>
  mutate(
    border_side = recode(border_side, !!!ndlsa_lookup)
  )


summary_df <- points_df |>
  group_by(border_side, comparison) |>
  summarise(
    mean_km = mean(distance_km),
    se_km = sd(distance_km) / sqrt(n()),
    n = n(),
    .groups = "drop"
  )

#---------------------------------------------#
# plot nearest distance
#---------------------------------------------#
plot <- ggplot() +
  geom_jitter(
    data = points_df,
    aes(x = border_side, y = distance_km, color = comparison),
    position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.5),
    size = 1.6,
    alpha = 0.5
  ) +
  geom_point(
    data = summary_df,
    aes(x = border_side, y = mean_km, color = comparison),
    position = position_dodge(width = 0.5),
    size = 3
  ) +
  geom_errorbar(
    data = summary_df,
    aes(
      x = border_side,
      ymin = mean_km - se_km,
      ymax = mean_km + se_km,
      color = comparison
    ),
    position = position_dodge(width = 0.5),
    width = 0.2
  ) +
  labs(
    x = NULL,
    y = "Nearest neighbor distance (km)",
    color = NULL
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 12, angle = 90, hjust = 1),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12)
  )

x11()
plot(plot)

ggsave(
  filename = paste0(data_storage_path, "Output/transboundary/plots/euclidean_distance.png"),
  plot = plot,
  width = 9,
  height = 6,
  dpi = 300
)

#---------------------------------------------#
# include size of buffer area 
#---------------------------------------------#
buffer_area_country <- pa_distance_coverage_summary |>
  st_drop_geometry() |>
  mutate(
    border_side = dplyr::recode(border_side, !!!ndlsa_lookup),
    border_side = stringr::str_squish(border_side)
  ) |>
  group_by(border_side) |>
  summarise(
    total_buffer_area_km2 = sum(unique(buffer_area_km2), na.rm = TRUE),
    .groups = "drop"
  )

points_scaled <- points_df |>
  left_join(buffer_area_country, by = "border_side") |>
  mutate(
    distance_scaled = distance_km / sqrt(total_buffer_area_km2)
  )

summary_scaled <- points_scaled |>
  group_by(border_side, comparison) |>
  summarise(
    mean_scaled = mean(distance_scaled, na.rm = TRUE),
    se_scaled = sd(distance_scaled, na.rm = TRUE) / sqrt(n()),
    n = n(),
    .groups = "drop"
  )

plot_2 <- ggplot() +
  geom_jitter(
    data = points_scaled,
    aes(x = border_side, y = distance_scaled, color = comparison),
    position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.5),
    size = 1.6,
    alpha = 0.5
  ) +
  geom_point(
    data = summary_scaled,
    aes(x = border_side, y = mean_scaled, color = comparison),
    position = position_dodge(width = 0.5),
    size = 3
  ) +
  geom_errorbar(
    data = summary_scaled,
    aes(
      x = border_side,
      ymin = mean_scaled - se_scaled,
      ymax = mean_scaled + se_scaled,
      color = comparison
    ),
    position = position_dodge(width = 0.5),
    width = 0.2
  ) +
  labs(
    x = NULL,
    y = "Scaled nearest-neighbor distance",
    color = NULL
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 12, angle = 90, hjust = 1),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12)
  )
x11()
plot(plot_2)

ggsave(
  filename = paste0(data_storage_path, "Output/transboundary/plots/euclidean_distance_scaled.png"),
  plot = plot_2,
  width = 8,
  height = 6,
  dpi = 300
)

#---------------------------------------------#
# coverage % per country 
#---------------------------------------------#
coverage_country_df <- pa_distance_coverage_summary |>
  st_drop_geometry() |>
  mutate(
    border_side = dplyr::recode(border_side, !!!ndlsa_lookup),
    border_side = stringr::str_squish(border_side),
    pa_overlap_km2 = tidyr::replace_na(pa_overlap_km2, 0)
  ) |>
  filter(!is.na(border_side), border_side != "") |>
  group_by(border_side, buffer_side_id, buffer_area_km2) |>
  summarise(
    total_pa_overlap_km2 = sum(pa_overlap_km2, na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(border_side) |>
  summarise(
    total_pa_overlap_km2 = sum(total_pa_overlap_km2, na.rm = TRUE),
    total_buffer_area_km2 = sum(buffer_area_km2, na.rm = TRUE),
    
    border_pa_coverage_prop =
      total_pa_overlap_km2 / total_buffer_area_km2,
    
    n_border_buffers = n(),
    .groups = "drop"
  )


coverage_country_df <- coverage_country_df |>
  left_join(country_area, by = c("border_side" = "GID_0"))|>
  mutate(
    pa_area_per_country_area =
      total_pa_overlap_km2 / country_area_km2
  )

coverage <- ggplot( coverage_country_df, 
                    aes(x = reorder(border_side, border_pa_coverage_prop), 
                      y = border_pa_coverage_prop) ) + 
  geom_col() + 
  coord_flip() + labs( x = NULL, y = "Proportion of border area protected" ) + 
  theme_bw() + 
  theme( axis.text.x = element_text(size = 12), 
         axis.text.y = element_text(size = 12), 
         axis.title.y = element_text(size = 14) )


plot(coverage)

ggsave(
  filename = paste0(data_storage_path, "Output/transboundary/plots/border_pa_coverage.png"),
  plot = coverage,
  width = 8,
  height = 6,
  dpi = 300
)

#---------------------------------------------#
# include the information on how much is actually border area in each country
#---------------------------------------------#
coverage_plot_df <- coverage_country_df |>
  left_join(
    coverage_df |> 
      select(GID_0, border_share_of_country),
    by = c("border_side" = "GID_0")
  )

coverage <- ggplot(
  coverage_plot_df,
  aes(x = reorder(border_side, border_pa_coverage_prop))
) +
  geom_col(
    aes(y = border_pa_coverage_prop),
    fill = "grey45"
  ) +
  geom_point(
    aes(y = border_share_of_country),
    color = "red",
    size = 3
  ) +
  coord_flip() +
  labs(
    x = NULL,
    y = "Proportion",
    caption = "Bars = proportion of border region protected; red points = proportion of country within border region"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14)
  )


plot(coverage)

ggsave(
  filename = paste0(data_storage_path, "Output/transboundary/plots/border_pa_coverage_scaled.png"),
  plot = coverage,
  width = 8,
  height = 6,
  dpi = 300
)
#---------------------------------------------#
# ranking of country pairs  
#---------------------------------------------#
pair_pa_counts <- pa_distance_coverage_summary |>
  st_drop_geometry() |>
  filter(!is.na(WDPAID)) |>
  mutate(
    border_side = recode(border_side, !!!ndlsa_lookup),
    border_side = stringr::str_squish(border_side)
  ) |>
  group_by(pair_id, border_side) |>
  summarise(
    n_pas = n_distinct(WDPAID),
    .groups = "drop"
  )
pair_pa_counts_wide <- pair_pa_counts |>
  tidyr::pivot_wider(
    names_from = border_side,
    values_from = n_pas,
    values_fill = 0
  )

pair_summary <- pa_distance_coverage_summary |>
  st_drop_geometry() |>
  group_by(pair_id) |>
  summarise(
    mean_across_km = mean(nearest_across_km, na.rm = TRUE),
    .groups = "drop"
  ) |>
  left_join(pair_pa_counts_wide, by = "pair_id")

pair_pa_long <- pair_pa_counts |>
  mutate(pair_id = factor(pair_id))


country_cols <- c(
  "AFG" = "#1b9e77",
  "IND" = "#d95f02",
  "CHN" = "#7570b3",
  "NPL" = "#e7298a",
  "BTN" = "#66a61e",
  "PAK" = "#e6ab02",
  "MMR" = "#a6761d",
  "BGD" = "#666666",
  "Z05" = "#a6cee3",
  "Z06" = "#fb9a99",
  "Z07" = "#b2df8a"
)

pairs <- ggplot(pair_pa_long,
                aes(x = reorder(pair_id, n_pas, sum),
                    y = n_pas,
                    fill = border_side)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = country_cols) +
  labs(
    x = NULL,
    y = "Number of border PAs",
    fill = "Country"
  ) +
  theme_bw()+
  theme(
    axis.text.x = element_text(size = 12, angle = 90, hjust = 1),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12)
  )



plot(pairs)

ggsave(
  filename = paste0(data_storage_path, "Output/transboundary/plots/countrypairs.png"),
  plot = pairs,
  width = 8,
  height = 6,
  dpi = 300
)


