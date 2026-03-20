
#---------------------------------------------#
# Step 1: list files
#---------------------------------------------#
source(here::here("R/00_Config_file_HKH.R"))


library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)
library(scales)



#---------------------------------------------#
# get border segments, countries and cons priority
#---------------------------------------------#

border_segments_cons_prio <- sf::st_read(paste0(data_storage_path, "Output/transboundary/border_segments_consprio.gpkg"))

transb_species <- sf::st_read(paste0(data_storage_path, "Output/transboundary/transb_glob_threat_new.shp"))


# 

plot_segment_prio <- function(seg,
                              data = border_segments_cons_prio,
                              transb_data = transb_species) {
  
  dat <- data |>
    st_drop_geometry() |>
    filter(seg_id == seg) |>
    transmute(
      seg_id,
      pair_id,
      ctryA,
      ctryB,
      protected_A   = p1A + p2A + p3A,
      unprotected_A = p4A + p5A + p6A,
      protected_B   = p1B + p2B + p3B,
      unprotected_B = p4B + p5B + p6B
    )
  
  if (nrow(dat) == 0) stop("seg_id not found.")
  
  shared_n <- transb_data |>
    st_drop_geometry() |>
    filter(seg_id == seg) |>
    pull(n_shard)
  
  shared_n <- if (length(shared_n) == 0 || all(is.na(shared_n))) NA else shared_n[1]
  
  cA <- dat$ctryA[1]
  cB <- dat$ctryB[1]
  
  plot_df <- tibble(
    country = c(cA, cA, cB, cB),
    priority_type = c(
      "Protected priority", "Unprotected priority",
      "Protected priority", "Unprotected priority"
    ),
    value = c(
      dat$protected_A[1], dat$unprotected_A[1],
      dat$protected_B[1], dat$unprotected_B[1]
    )
  )
  
  ggplot(plot_df, aes(x = priority_type, y = value, fill = country)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.6) +
    
    annotate(
      "text",
      x = 2,
      y = max(plot_df$value) * 1.05,
      label = paste0("Shared threatened species: ", shared_n),
      hjust = 1,
      size = 4.5
    ) +
    
    scale_y_continuous(
      labels = percent_format(accuracy = 1),
      expand = expansion(mult = c(0, 0.12))
    ) +
    
    scale_fill_manual(
      values = setNames(c("grey90", "cadetblue4"), c(cA, cB)),
      name = NULL
    ) +
    
    labs(
      title = paste0("Border segment ", seg, " (", dat$pair_id[1], ")"),
      x = NULL,
      y = "Proportion of pixels (50 km buffer)"
    ) +
    
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "top",
      axis.text.x = element_text(face = "bold")
    )
}



#---------------------------------------------#
# plot segments and save
#---------------------------------------------#

# type seg ID here
seg <- 43

plot_obj <- plot_segment_prio(seg)

ggsave(
  filename = paste0(
    data_storage_path,
    "Output/transboundary/segment_plots/priority_segment_",
    seg,
    ".jpeg"
  ),
  plot = plot_obj,
  width = 6,
  height = 5,
  dpi = 300
)


