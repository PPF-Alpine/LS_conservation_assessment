#----------------------------------------------------------#
# PLOT 
#----------------------------------------------------------#
cons_prio <- terra::rast(paste0(data_storage_path, "Output/priority_indices/threedim_rgb.tif"))
border_segments <- sf::st_read(paste0(data_storage_path, "Output/transboundary/borders_segments_100_countrypairs.shp"))
library(terra)
library(sf)

border_segments <- st_transform(border_segments, crs(cons_prio))
border_segments_v <- vect(border_segments)

x11(width = 13, height = 8)

par(mar = c(3,3,3,3))

plotRGB(
  cons_prio,
  r = 1,
  g = 2,
  b = 3,
  stretch = "lin"
)

plot(
  border_segments_v,
  add = TRUE,
  col = "white",
  lwd = 2
)

# -------------------------------------------------
# map extent
# -------------------------------------------------

e <- ext(cons_prio)

# -------------------------------------------------
# RGB TRIANGLE POSITION
# -------------------------------------------------

tri_x <- e$xmin + 0.05 * (e$xmax - e$xmin)
tri_y <- e$ymin + 0.05 * (e$ymax - e$ymin)

tri_scale <- 0.12 * (e$xmax - e$xmin)

# -------------------------------------------------
# RGB TRIANGLE FUNCTION
# -------------------------------------------------

draw_rgb_triangle <- function(
    x_offset,
    y_offset,
    scale,
    n = 120
){
  
  x <- c(0, 1, 0.5)
  y <- c(0, 0, sqrt(3)/2)
  
  for(i in 1:n){
    for(j in 1:n){
      
      a <- i/n
      b <- j/n
      c <- 1 - a - b
      
      if(c >= 0){
        
        col <- rgb(a, b, c)
        
        px <- a*x[1] + b*x[2] + c*x[3]
        py <- a*y[1] + b*y[2] + c*y[3]
        
        points(
          px * scale + x_offset,
          py * scale + y_offset,
          pch = 15,
          cex = 0.35,
          col = col
        )
      }
    }
  }
  
  # outline
  polygon(
    c(0,1,0.5)*scale + x_offset,
    c(0,0,sqrt(3)/2)*scale + y_offset,
    border = "black",
    lwd = 1.2
  )
  
  # labels
  text(
    x_offset,
    y_offset - 0.04*scale,
    "Biodiversity",
    col = "red",
    adj = 0,
    cex = 0.7
  )
  
  text(
    x_offset + scale,
    y_offset - 0.04*scale,
    "Climate",
    col = "green4",
    adj = 1,
    cex = 0.7
  )
  
  text(
    x_offset + scale/2,
    y_offset + sqrt(3)/2 * scale + 0.04*scale,
    "Human footprint",
    col = "blue",
    cex = 0.7
  )
  
  text(
    x_offset + scale/2,
    y_offset + 0.32*scale,
    "balanced",
    cex = 0.6
  )
}

# -------------------------------------------------
# draw triangle inset
# -------------------------------------------------

draw_rgb_triangle(
  x_offset = tri_x,
  y_offset = tri_y,
  scale = tri_scale
)

# -------------------------------------------------
# compact explanatory legend
# -------------------------------------------------

legend(
  "bottomleft",
  inset = c(0.20, 0.05),
  legend = c(
    "High biodiversity",
    "High climate change",
    "High human footprint",
    "Bio + climate high",
    "Bio + footprint high",
    "Climate + footprint high",
    "All three high",
    "All three low"
  ),
  fill = c(
    "red", "green", "blue",
    "yellow", "magenta", "cyan",
    "white", "black"
  ),
  border = "grey40",
  bg = "white",
  cex = 0.7,
  pt.cex = 1.2,
  ncol = 2
)

# -------------------------------------------------
# show the same thing but in bivariate combinations 
# -------------------------------------------------
library(terra)
library(ggplot2)
library(patchwork)

# -----------------------------
# 1. extract layers
# -----------------------------

biodiv  <- cons_prio[[1]]
climate <- cons_prio[[2]]
hfi     <- cons_prio[[3]]

# -----------------------------
# 2. classify raster into low / medium / high
# -----------------------------

classify_3 <- function(r) {
  qs <- quantile(values(r), probs = c(1/3, 2/3), na.rm = TRUE)
  
  classify(
    r,
    rcl = matrix(
      c(
        -Inf, qs[1], 1,
        qs[1], qs[2], 2,
        qs[2], Inf, 3
      ),
      ncol = 3,
      byrow = TRUE
    )
  )
}

biodiv_class  <- classify_3(biodiv)
climate_class <- classify_3(climate)
hfi_class     <- classify_3(hfi)


make_bivar_plot <- function(r1_class, r2_class, border_segments, xlab, ylab) {
  
  bivar <- r1_class * 10 + r2_class
  
  bivar_df <- as.data.frame(bivar, xy = TRUE, na.rm = TRUE)
  names(bivar_df)[3] <- "class"
  
  bivar_df$class <- factor(
    bivar_df$class,
    levels = c(11,12,13,21,22,23,31,32,33)
  )
  
  bivar_cols <- c(
    "11" = "#e8e8e8",
    "12" = "#b8d6be",
    "13" = "#73ae80",
    "21" = "#dfb0d6",
    "22" = "#a5add3",
    "23" = "#5698b9",
    "31" = "#be64ac",
    "32" = "#8c62aa",
    "33" = "#3b4994"
  )
  
  p_map <- ggplot() +
    geom_tile(
      data = bivar_df,
      aes(x = x, y = y, fill = class)
    ) +
    geom_sf(
      data = border_segments,
      fill = NA,
      color = "white",
      linewidth = 0.35
    ) +
    scale_fill_manual(values = bivar_cols) +
    coord_sf() +
    theme_void() +
    theme(legend.position = "none")
  
  legend_df <- expand.grid(x = 1:3, y = 1:3)
  
  legend_df$class <- factor(
    c("11","12","13",
      "21","22","23",
      "31","32","33"),
    levels = names(bivar_cols)
  )
  
  p_legend <- ggplot(legend_df, aes(x = x, y = y, fill = class)) +
    geom_tile() +
    scale_fill_manual(values = bivar_cols, guide = "none") +
    scale_x_continuous(
      breaks = 1:3,
      labels = c("Low","Medium","High"),
      expand = c(0,0)
    ) +
    scale_y_continuous(
      breaks = 1:3,
      labels = c("Low","Medium","High"),
      expand = c(0,0)
    ) +
    labs(x = xlab, y = ylab) +
    coord_fixed() +
    theme_minimal(base_size = 8) +
    theme(panel.grid = element_blank())
  
  p_map +
    inset_element(
      p_legend,
      left = 0.05,
      bottom = 0.05,
      right = 0.32,
      top = 0.32
    )
}


p_bio_clim <- make_bivar_plot(
  biodiv_class,
  climate_class,
  border_segments = border_segments,
  xlab = "Climate velocity",
  ylab = "Biodiversity"
)

p_bio_hfi <- make_bivar_plot(
  biodiv_class,
  hfi_class,
  border_segments = border_segments,
  xlab = "Human footprint",
  ylab = "Biodiversity"
)

p_clim_hfi <- make_bivar_plot(
  climate_class,
  hfi_class,
  border_segments = border_segments,
  xlab = "Human footprint",
  ylab = "Climate velocity"
)

x11(width = 15, height = 10)

combined <- p_clim_hfi /
  (p_bio_clim | p_bio_hfi) +
  plot_layout(heights = c(1.3, 1))

#----------------------------------------------------------#
# save plots 
#----------------------------------------------------------#
ggsave(
  filename = file.path(data_storage_path, "Output/pa/bivariat_map.png"),
  plot = combined,
  width = 14,
  height = 9,
  dpi = 300
)
