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
    count(alpine_category, threats_broad) |>
    group_by(alpine_category) |>
    mutate(prop = n / sum(n)) |>
    ungroup() |>
    mutate(
      threats_broad = factor(threats_broad, levels = sort(as.numeric(unique(threats_broad))))
    )|>
    left_join(classification_code_description, by = c("threats_broad" = "code"))
  
  # Plot
  ggplot(threats_props, aes(x = alpine_category, y = description, fill = prop)) +
    geom_tile(color = "white") +
    scale_fill_viridis_c(option = "inferno", name = "Proportion", labels = percent) +
    labs(
      title = mountain_name,
      x = NULL,
      y = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.key.height = unit(0.3, "cm"),
          legend.key.width = unit(0.3, "cm"),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 9))
}
