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
  
border_segments_cons_prio <- sf::st_read(paste0(data_storage_path, "Output/transboundary/border_segments_consprio.gpkg"))
  
transb_species <- sf::st_read(paste0(data_storage_path, "Output/transboundary/transb_glob_threat_new.shp"))
  


quantile(c(border_segments_cons_prio$npixA,
           border_segments_cons_prio$npixB),
         probs = 0.9, na.rm = TRUE)


filtered_df <- border_segments_cons_prio |>
  filter(pmin(npixA, npixB) > 6000)


# ---------------------------------------------#
  # with absolute
  #---------------------------------------------#



library(dplyr)
library(sf)

plot_df <- filtered_df |>
  st_drop_geometry() |>
  mutate(
    dist_prio = sqrt(
      (p1A - p1B)^2 +
        (p2A - p2B)^2 +
        (p3A - p3B)^2 +
        (p4A - p4B)^2 +
        (p5A - p5B)^2 +
        (p6A - p6B)^2
    ),
    
    protA = c1A + c2A + c3A,
    protB = c1B + c2B + c3B,
    
    prot_imbalance = case_when(
      protA > 0 & protB < 130 ~ TRUE,
      protB > 0 & protA < 130 ~ TRUE,
      TRUE ~ FALSE
    )
  ) |>
  left_join(
    transb_species |>
      st_drop_geometry() |>
      select(seg_id, n_shard),
    by = "seg_id"
  )


######## interactive

library(ggplot2)

p <- ggplot(plot_df, aes(x = dist_prio, y = n_shard, color = pair_id)) +
  geom_point(size = 3, alpha = 0.85) +
  geom_point(
    data = dplyr::filter(plot_df, prot_imbalance),
    shape = 21,
    size = 4.8,
    stroke = 1.1,
    fill = NA,
    color = "black"
  ) +
  labs(
    x = "Difference in conservation-priority",
    y = "Shared threatened species",
    color = "Country pair"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank()
  )

p


ggsave(
  filename = paste0(data_storage_path, "Output/transboundary/integration_shared_sp_prio.png"),
  plot = p,
  width = 7,
  height = 5,
  dpi = 300
)


plot_df <- plot_df |>
  mutate(
    hover_txt = paste0(
      "<b>", pair_id, "</b>",
      "<br>Segment: ", seg_id,
      "<br>Shared threatened species: ", n_shard,
      "<br>Priority distance: ", round(dist_prio, 3),
      "<br>Protected pixels A: ", protA,
      "<br>Protected pixels B: ", protB,
      "<br>Protection imbalance: ", ifelse(prot_imbalance, "Yes", "No")
    )
  )

pair_levels <- unique(plot_df$pair_id)
pair_cols <- setNames(
  colorRampPalette(brewer.pal(8, "Set2"))(length(pair_levels)),
  pair_levels
)

p_int <- plot_ly(
  data = plot_df,
  x = ~dist_prio,
  y = ~n_shard,
  type = "scatter",
  mode = "markers",
  color = ~pair_id,
  colors = pair_cols,
  marker = list(size = 9, opacity = 0.85),
  text = ~hover_txt,
  hovertemplate = "%{text}<extra></extra>"
)

p_int <- add_markers(
  p_int,
  data = dplyr::filter(plot_df, prot_imbalance),
  x = ~dist_prio,
  y = ~n_shard,
  inherit = FALSE,
  marker = list(
    symbol = "circle-open",
    size = 16,
    color = "black",
    line = list(width = 2)
  ),
  hoverinfo = "skip",
  showlegend = FALSE
)

p_int <- plotly::layout(
  p_int,
  xaxis = list(title = "Difference in conservation-priority composition"),
  yaxis = list(title = "Shared threatened species"),
  legend = list(title = list(text = "Country pair"))
)

p_int
