library(ggridges)


# Combine all datasets into one
assessment_combined <- bind_rows(assessment_generalists, 
                                 assessment_specialists, 
                                 assessment_bm_alp)

# mutate elevational range and midpoints 
assessment_combined <- assessment_combined|>
  mutate(range_size = max_elevation - min_elevation)|>
  mutate(mid_elevation = (max_elevation + min_elevation)/2)|>
  arrange(mid_elevation) |>
  mutate(species_order = row_number())  # For clearer plotting along x-axis

# mutate categories 
assessment_combined <- assessment_combined|>
  mutate(assessment_summary = case_when(
    redlistCategory %in% c("Near Threatened", "Vulnerable", "Endangered","Critically Endangered") ~ "Threathened",
    redlistCategory %in% c("Data Deficient", "Not assessed") ~ "DD/NA",
    redlistCategory == "Least Concern" ~ "Least Concern"
  ))

# Define mountain selection
mountain_selection <- c("Himalaya", 
                        "Northern Andes", 
                        "Central Andes", 
                        "Central European Highlands", 
                        "Intermountain West",
                        "Hindu Kush", 
                        "Ethiopian Highlands", 
                        "Albertine Rift Mountain",
                        "South Island",
                        "North European Highlands",
                        "Tibetan Plateau",
                        "Malay Archipelago",
                        "Caucasus Mountains",
                        "East European Highlands",
                        "Rocky Mountains",
                        "Eastern Rift mountains",
                        "Mexican Highlands")

# Data preparation
assessment_mountain <- assessment_combined %>%
  filter(Mountain_range %in% mountain_selection) %>%
  mutate(range_size = max_elevation - min_elevation,
         mid_elevation = (max_elevation + min_elevation) / 2) %>%
  arrange(mid_elevation) %>%
  mutate(species_order = row_number())


#----------------------------------------------------------#
#       line range plot 
#----------------------------------------------------------#
## 
x11()
ggplot(assessment_mountain) +
  geom_linerange(aes(x = mid_elevation, 
                     ymin = min_elevation, ymax = max_elevation,
                     color = assessment_summary),
                 alpha = 0.7, linewidth = 0.3) +
  geom_point(aes(x = mid_elevation, y = mid_elevation, color = assessment_summary),
             size = 0.7, alpha = 0.9) +
  facet_wrap(~Mountain_range, scales = "free", ncol = 4) +
  scale_color_manual(values = c(
    "DD/NA" = "grey20",
    "Least Concern" = "#F5C710",
    "Threathened" = "#E03426"
  ), name = "Red List Category") +
  labs(
    title = "elevation midpoint, elevational range size and conservation status mammals, birds and reptiles",
    x = "elevational midpoint (max-min/2) (m)",
    y = "min to max elevation (m)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


#----------------------------------------------------------#
#      violin plot 
#----------------------------------------------------------#

ggplot(assessment_mountain, aes(x = assessment_summary, y = mid_elevation, fill = assessment_summary)) +
  geom_violin(alpha = 0.5, scale = "width", adjust = 1.2) +
  geom_jitter(width = 0.25, size = 0.7, alpha = 0.4, color = "black") +
  facet_wrap(~Mountain_range, scales = "free_y", ncol = 4) +
  scale_fill_manual(values = c(
    "DD/NA" = "grey60",
    "Least Concern" = "lightgreen",
    "Threathened" = "red4"
  ), name = "Red List Category") +
  labs(
    title = "elevation midpoints by threat status",
    x = "Conservation Status",
    y = "Midpoint Elevation (m)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )


# Calculate counts per group for annotation
counts <- assessment_mountain %>%
  group_by(Mountain_range, assessment_summary) %>%
  summarise(count = n(), .groups = "drop")

ggplot(assessment_mountain, aes(x = assessment_summary, y = mid_elevation, fill = assessment_summary)) +
  geom_violin(alpha = 0.5, scale = "width", adjust = 1.2) +
  geom_jitter(width = 0.15, size = 0.6, alpha = 0.6, color = "black") +
  geom_text(data = counts,
            aes(x = assessment_summary, y = Inf, label = paste("n =", count)),
            position = position_nudge(y = -0.05),
            vjust = 1.5,
            size = 3.5,
            fontface = "bold") +
  facet_wrap(~Mountain_range, scales = "free_y", ncol = 4) +
  scale_fill_manual(values = c(
    "DD/NA" = "grey60",
    "Least Concern" = "lightgreen",
    "Threathened" = "red4"
  ), name = "Red List Category") +
  labs(
    title = "elevation midpoints by threat status",
    x = "Conservation Status",
    y = "Midpoint elevation (m)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )

#----------------------------------------------------------#
#      ridgeline for mipoint and elev range
#----------------------------------------------------------#

# Ridgeline density plot
ggplot(assessment_mountain, aes(y = assessment_summary, x = range_size, fill = assessment_summary)) +
  geom_density_ridges(alpha = 0.8, scale = 1.5, rel_min_height = 0.01) +
  scale_fill_manual(values = c(
    "DD/NA" = "black",
    "Least Concern" = "gold",
    "Threathened" = "#DC143C"
  ), name = "Red List Category") +
  labs(title = "Ridgeline Density Plot of Species Elevational Distributions in the Northern Andes",
       x = "range size (m)",
       y = "Red List Category") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 12),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())


# Ridgeline density plot
ggplot(assessment_mountain, aes(y = assessment_summary, x = mid_elevation, fill = assessment_summary)) +
  geom_density_ridges(alpha = 0.8, scale = 1.5, rel_min_height = 0.01) +
  scale_fill_manual(values = c(
    "DD/NA" = "black",
    "Least Concern" = "gold",
    "Threathened" = "#DC143C"
  ), name = "Red List Category") +
  labs(title = "Ridgeline Density Plot of Species Elevational Distributions in the Northern Andes",
       x = "Midpoint Elevation of Species (m)",
       y = "Red List Category") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 12),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())

#----------------------------------------------------------#
#     Plot simple scatter
#----------------------------------------------------------#
# 
ggplot(assessment_mountain, aes(x = mid_elevation, y = range_size, color = assessment_summary)) +
  geom_point(alpha = 0.7, size = 2) +
  scale_color_manual(values = c(
    "DD/NA" = "black",
    "Least Concern" = "gold",
    "Threathened" = "#DC143C"
  ), name = "Red List Category") +
  labs(title = "Elevational Range Size vs Midpoint Elevation by Red List Category in the Northern Andes",
       x = "Midpoint Elevation (m)",
       y = "Elevation Range Size (m)") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())




