# Function to create one heatmap per mountain for threats
plot_threat_heatmap <- function(mountain_name) {
  mountain <- threats_stresses_species |>
    filter(Mountain_range == mountain_name)
  
  # Skip if no data
  if (nrow(mountain) == 0) return(NULL)
  
  # Split threat codes into rows
  threats_long <- mountain |>
    separate_rows(threats_broad, sep = ";\\s*")|>
    filter(!is.na(threats_code) & threats_code != "")
  
  # which are the greatest threats at each elevation belt? 
  threats_props <- threats_long |>
    count(elevation_band, threats_broad) |>
    group_by(elevation_band) |>
    mutate(prop = n / sum(n)) |>
    ungroup() |>
    mutate(
      threats_broad = factor(threats_broad, levels = sort(as.numeric(unique(threats_broad))))
    )|>
    left_join(classification_code_description, by = c("threats_broad" = "code"))
  
  # Plot
  ggplot(threats_props, aes(x = elevation_band, y = description, fill = prop)) +
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
