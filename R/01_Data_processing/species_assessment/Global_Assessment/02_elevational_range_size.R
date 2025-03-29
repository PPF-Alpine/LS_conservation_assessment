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

# Data preparation
assessment_mountain <- assessment_combined %>%
  filter(Mountain_range == "Northern Andes") %>%
  mutate(range_size = max_elevation - min_elevation,
         mid_elevation = (max_elevation + min_elevation) / 2) %>%
  arrange(mid_elevation) %>%
  mutate(species_order = row_number())

# Plot simple scatter
ggplot(assessment_mountain, 
       aes(x = mid_elevation, 
           y = range_size)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "Elevational Range Size and Position per Species in the Northern Andes",
       x = "Midpoint of Elevational Range (m)",
       y = "Elevation Range Size (m)") +
  theme_minimal() +
  theme(text = element_text(size = 12))

# Plotting the elevational ranges and midpoints:
x11()
ggplot(assessment_mountain) +
  geom_segment(aes(x = species_order,
                   y = min_elevation, yend = max_elevation,
                   color = assessment_summary),
               linewidth = 0.4, alpha = 0.8) +
  scale_color_manual(values = c(
    "DD/NA" = "black",
    "Least Concern" = "gold",
    "Threathened" = "#DC143C"
  ), name = "Red List Category") +
  scale_x_continuous(
    breaks = seq(1, nrow(assessment_mountain), length.out = 10),
    labels = round(assessment_mountain$mid_elevation[seq(1, nrow(assessment_mountain), length.out = 10)])
  ) +
  labs(title = "Elevational Ranges and Red List Status of Species in the Northern Andes",
       x = "Midpoint Elevation of Species (m)",
       y = "Elevation Range (m)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())


# Ridgeline density plot
x11()
ggplot(assessment_mountain, aes(y = assessment_summary, x = range_size, fill = assessment_summary)) +
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
