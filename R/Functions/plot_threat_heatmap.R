# Function to create one heatmap per mountain for threats
plot_threat_heatmap <- function(mountain_name) {
  mountain <- threats_stresses_species |>
    filter(Mountain_range == mountain_name)
  
  # Skip if no data
  if (nrow(mountain) == 0) return(NULL)
  
  # Define elevation bands based on max elevation for this mountain
  breaks <- seq(0, max(mountain$mid_elevation, na.rm = TRUE) + 500, by = 500)
  labels <- paste0(head(breaks, -1), "–", tail(breaks, -1))
  
  threats_binned <- mountain |>
    mutate(elevation_band = cut(mid_elevation, breaks = breaks, labels = labels, include.lowest = TRUE))
  
  threats_long <- threats_binned |>
    separate_rows(threats_broad, sep = ";\\s*") |>
    filter(!is.na(threats_code) & threats_code != "")
  
  threat_props <- threats_long |>
    count(elevation_band, threats_broad) |>
    group_by(elevation_band) |>
    mutate(prop = n / sum(n)) |>
    ungroup() |>
    mutate(threats_broad = factor(threats_broad, levels = sort(as.numeric(unique(threats_broad))))) |>
    left_join(threat_classification_codes, by = c("threats_broad" = "code"))
  
  # Plot
  ggplot(threat_props, aes(x = elevation_band, y = description, fill = prop)) +
    geom_tile(color = "white") +
    scale_fill_viridis_c(option = "magma", name = "Proportion", labels = percent) +
    labs(
      title = mountain_name,
      x = "Mid Elevation Band (m)",
      y = NULL
    ) +
    theme_minimal(base_size = 10) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
