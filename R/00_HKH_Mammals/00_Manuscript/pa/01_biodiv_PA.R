library(terra)
library(sf)
library(sf)
library(dplyr)
library(units)

# Load configuration file
source(here::here("R/00_Config_file_HKH.R"))

#----------------------------------------------------------#
# 0. load files  -----
#----------------------------------------------------------#

pa <- sf::st_read(
  "C:/Users/losch5089/OneDrive - University of Bergen/Desktop/Manuscripts/Ch_2_HKH_mammals/Datasets/protected_areas/PA_HKH/PA_HKH_complete_clean.shp"
)

biodiv_imp <- rast(file.path(data_storage_path, "Output/biodiv_dimensions/biodiv_imp.tif"))
clim_velocity_temp <- terra::rast(file.path(data_storage_path, "Datasets/climate_shift/clim_velocity/clim_velocity_temp_capped.tif"))
hfi <- rast(file.path(data_storage_path, "Datasets/hfi/hfi_hkh.tif"))
# Resample to same resolution/grid
biodiv_imp <- resample(biodiv_imp, clim_velocity_temp, method = "bilinear")
hfi <- resample(hfi, clim_velocity_temp, method = "bilinear")

# Crop to climate velocity extent
biodiv_imp <- crop(biodiv_imp, clim_velocity_temp)
hfi <- crop(hfi, clim_velocity_temp)

#----------------------------------------------------------#
# 01---extract values from raster within PAs
#----------------------------------------------------------#

# Stack rasters
r_stack <- c(biodiv_imp, clim_velocity_temp, hfi)

# Rename layers
names(r_stack) <- c("biodiv_imp", "clim_velocity", "hfi")

# classify as low medium, high

# Convert sf to terra vector
pa_vect <- vect(pa)

# Mean values
mean_vals <- terra::extract(
  r_stack,
  pa_vect,
  fun = mean,
  na.rm = TRUE
)

# Median values
median_vals <- terra::extract(
  r_stack,
  pa_vect,
  fun = median,
  na.rm = TRUE
)

# Rename columns
colnames(mean_vals)[-1] <- paste0(colnames(mean_vals)[-1], "_mean")
colnames(median_vals)[-1] <- paste0(colnames(median_vals)[-1], "_median")

# Combine with PA attributes
pa_stats <- pa %>%
  bind_cols(
    mean_vals %>% select(-ID),
    median_vals %>% select(-ID)
  )

#----------------------------------------------------------#
# 02 PLOT
#----------------------------------------------------------#

library(plotly)
library(dplyr)

pa_plot_df <- pa_stats %>%
  st_drop_geometry() %>%
  filter(
    !is.na(biodiv_imp_median),
    !is.na(hfi_median)
  )

plot_ly(
  data = pa_plot_df,
  x = ~biodiv_imp_median,
  y = ~hfi_median,
  type = "scatter",
  mode = "markers",
  color = ~trnsbnd,
  colors = c("no" = "grey70", "yes" = "red"),
  text = ~name,
  hovertemplate = paste(
    "<b>%{text}</b><br>",
    "Biodiv imp median: %{x}<br>",
    "HFI median: %{y}",
    "<extra></extra>"
  )
) %>%
  layout(
    xaxis = list(title = "Median biodiversity importance"),
    yaxis = list(title = "Median HFI")
  )

# static 
p <- ggplot(
  pa_plot_df,
  aes(
    x = biodiv_imp_median,
    y = hfi_median
  )
) +
  
  geom_point(
    aes(
      shape = transboundary,
      size = transboundary,
      fill = transboundary
    ),
    colour = "black",
    stroke = 0.7,
    alpha = 0.9
  ) +
  
  scale_shape_manual(
    values = c(
      "no" = 21,   # circle
      "yes" = 24   # triangle
    ),
    name = "Transboundary"
  ) +
  
  scale_size_manual(
    values = c(
      "no" = 2.8,
      "yes" = 4
    ),
    guide = "none"
  ) +
  
  scale_fill_manual(
    values = c(
      "no" = "grey70",
      "yes" = "red"
    ),
    name = "Transboundary"
  ) +
  
  labs(
    x = "Median biodiversity importance",
    y = "Median HFI"
  ) +
  
  theme_bw() +
  
  theme(
    panel.grid = element_blank(),
    
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14)
  )


#----------------------------------------------------------#
# same for clim veloc
#----------------------------------------------------------#


pa_plot_df_2 <- pa_stats %>%
  st_drop_geometry() %>%
  filter(
    !is.na(biodiv_imp_median),
    !is.na(clim_velocity_median)
  )

plot_ly(
  data = pa_plot_df_2,
  x = ~biodiv_imp_median,
  y = ~clim_velocity_median,
  type = "scatter",
  mode = "markers",
  color = ~transboundary,
  colors = c("no" = "grey70", "yes" = "red"),
  text = ~name,
  hovertemplate = paste(
    "<b>%{text}</b><br>",
    "Biodiv imp median: %{x}<br>",
    "HFI median: %{y}",
    "<extra></extra>"
  )
) %>%
  layout(
    xaxis = list(title = "Median biodiversity importance"),
    yaxis = list(title = "Median clim_velocity")
  )



# static 
p2 <- ggplot(
  pa_plot_df_2,
  aes(
    x = biodiv_imp_median,
    y = clim_velocity_median
  )
) +
  
  geom_point(
    aes(
      shape = transboundary,
      size = transboundary,
      fill = transboundary
    ),
    colour = "black",
    stroke = 0.7,
    alpha = 0.9
  ) +
  
  scale_shape_manual(
    values = c(
      "no" = 21,   # circle
      "yes" = 24   # triangle
    ),
    name = "Transboundary"
  ) +
  
  scale_size_manual(
    values = c(
      "no" = 2.8,
      "yes" = 4
    ),
    guide = "none"
  ) +
  
  scale_fill_manual(
    values = c(
      "no" = "grey70",
      "yes" = "red"
    ),
    name = "Transboundary"
  ) +
  
  labs(
    x = "Median biodiversity importance",
    y = "Median clim velocity"
  ) +
  
  theme_bw() +
  
  theme(
    panel.grid = element_blank(),
    
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14)
  )
#----------------------------------------------------------#
# 01--- colour as with bivariate 
#----------------------------------------------------------#
#----------------------------------------------------------#
# classify rasters into low / medium / high BEFORE extract
#----------------------------------------------------------#

classify_tertiles <- function(r) {
  qs <- terra::global(
    r,
    fun = quantile,
    probs = c(1/3, 2/3),
    na.rm = TRUE
  ) |> as.numeric()
  
  terra::classify(
    r,
    rcl = matrix(
      c(
        -Inf, qs[1], 1,   # low
        qs[1], qs[2], 2,  # medium
        qs[2], Inf, 3     # high
      ),
      ncol = 3,
      byrow = TRUE
    )
  )
}

biodiv_cat <- classify_tertiles(biodiv_imp)
hfi_cat    <- classify_tertiles(hfi)

names(biodiv_cat) <- "biodiv_cat"
names(hfi_cat) <- "hfi_cat"

# Bivariate class:
# 11 = low biodiv, low HFI
# 12 = low biodiv, medium HFI
# 13 = low biodiv, high HFI
# ...
# 33 = high biodiv, high HFI

bivar_biodiv_hfi <- biodiv_cat * 10 + hfi_cat
names(bivar_biodiv_hfi) <- "bivar_biodiv_hfi"

#----------------------------------------------------------#
# extract PA values
#----------------------------------------------------------#

r_stack <- c(biodiv_imp, clim_velocity_temp, hfi)
names(r_stack) <- c("biodiv_imp", "clim_velocity", "hfi")

pa_vect <- vect(pa_dissolved)

median_vals <- terra::extract(
  r_stack,
  pa_vect,
  fun = median,
  na.rm = TRUE
)

colnames(median_vals)[-1] <- paste0(colnames(median_vals)[-1], "_median")

modal_fun <- function(x, ...) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  as.numeric(names(which.max(table(x))))
}

bivar_vals <- terra::extract(
  bivar_biodiv_hfi,
  pa_vect,
  fun = modal_fun,
  na.rm = TRUE
)

pa_stats <- pa_dissolved |>
  bind_cols(
    median_vals |> select(-ID),
    bivar_vals |> select(-ID)
  )

pa_plot_df <- pa_stats |>
  st_drop_geometry() |>
  mutate(
    bivar_biodiv_hfi = as.character(bivar_biodiv_hfi),
    bivar_label = recode(
      bivar_biodiv_hfi,
      "11" = "Low biodiv / Low HFI",
      "12" = "Low biodiv / Medium HFI",
      "13" = "Low biodiv / High HFI",
      "21" = "Medium biodiv / Low HFI",
      "22" = "Medium biodiv / Medium HFI",
      "23" = "Medium biodiv / High HFI",
      "31" = "High biodiv / Low HFI",
      "32" = "High biodiv / Medium HFI",
      "33" = "High biodiv / High HFI"
    )
  ) |>
  filter(
    !is.na(biodiv_imp_median),
    !is.na(hfi_median),
    !is.na(bivar_biodiv_hfi)
  )

bivar_cols <- c(
  "11" = "#e8e8e8",
  "12" = "#ace4e4",
  "13" = "#5ac8c8",
  "21" = "#dfb0d6",
  "22" = "#a5add3",
  "23" = "#5698b9",
  "31" = "#be64ac",
  "32" = "#8c62aa",
  "33" = "#3b4994"
)

plot_ly(
  data = pa_plot_df,
  x = ~biodiv_imp_median,
  y = ~hfi_median,
  type = "scatter",
  mode = "markers",
  color = ~bivar_biodiv_hfi,
  colors = bivar_cols,
  symbol = ~transboundary,
  symbols = c("no" = "circle", "yes" = "triangle-up"),
  text = ~paste0(
    "<b>", name, "</b><br>",
    "Transboundary: ", transboundary, "<br>",
    "Class: ", bivar_label
  ),
  hovertemplate = paste(
    "%{text}<br>",
    "Biodiv imp median: %{x}<br>",
    "HFI median: %{y}",
    "<extra></extra>"
  ),
  marker = list(size = 9, opacity = 0.85)
) %>%
  layout(
    xaxis = list(title = "Median biodiversity importance"),
    yaxis = list(title = "Median HFI"),
    legend = list(title = list(text = "Bivariate class / Transboundary"))
  )

#----------------------------------------------------------#
# static
#----------------------------------------------------------#

bivpa <- ggplot(
  pa_plot_df,
  aes(
    x = biodiv_imp_median,
    y = hfi_median
  )
) +
  
  geom_point(
    aes(
      fill = bivar_biodiv_hfi,
      shape = transboundary,
      size = transboundary
    ),
    colour = "black",
    stroke = 0.7,
    alpha = 0.7
  ) +
  
  scale_fill_manual(
    values = bivar_cols,
    guide = "none"
  ) +
  
  scale_shape_manual(
    values = c(
      "no" = 21,   # circle
      "yes" = 24   # triangle
    ),
    name = "Transboundary"
  ) +
  
  scale_size_manual(
    values = c(
      "no" = 2.8,
      "yes" = 4
    ),
    guide = "none"
  ) +
  
  labs(
    x = "Median biodiversity importance",
    y = "Median HFI"
  ) +
  
  theme_bw() +
  
  theme(
    panel.grid = element_blank(),
    
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14)
  )

#----------------------------------------------------------#
# save plots 
#----------------------------------------------------------#
ggsave(
  filename = file.path(data_storage_path, "Output/pa/bivariat_paplot.png"),
  plot = bivpa,
  width = 14,
  height = 9,
  dpi = 300
)


ggsave(
  filename = file.path(data_storage_path, "Output/pa/stat_paplot.png"),
  plot = p,
  width = 14,
  height = 9,
  dpi = 300
)

ggsave(
  filename = file.path(data_storage_path, "Output/pa/stat_paplot_clim.png"),
  plot = p2,
  width = 14,
  height = 9,
  dpi = 300
)
