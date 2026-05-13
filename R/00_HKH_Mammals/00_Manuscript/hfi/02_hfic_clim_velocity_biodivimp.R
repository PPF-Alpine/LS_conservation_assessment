

source(here::here("R/00_Config_file_HKH.R"))

biodiv_imp <- rast(file.path(data_storage_path, "Output/biodiv_dimensions/biodiv_imp.tif"))
clim_velocity_temp <- terra::rast(file.path(data_storage_path, "Datasets/climate_shift/clim_velocity/clim_velocity_temp_capped.tif"))
hfi <- rast(file.path(data_storage_path, "Datasets/hfi/hfi_hkh.tif"))

#---------------------------------------------#
# bring to ssame extent and resolution 
#---------------------------------------------#
template <- clim_velocity_temp

# crop biodiversity to climate extent/grid
biodiv_rs <- crop(biodiv_imp, template)
biodiv_rs <- resample(biodiv_rs, template, method = "bilinear")

# resample HFI down to climate grid
hfi_rs <- crop(hfi, template)
hfi_rs <- resample(hfi_rs, template, method = "bilinear")


template[!is.na(template)] <- 1

# fill HFI NA with 0
hfi_rs[is.na(hfi_rs)] <- 0

# remove values outside the template/HKH area
hfi_rs <- mask(hfi_rs, template)

plot(hfi_rs)

#---------------------------------------------#
# in cells with high hfi there is lower clim velocity ? 
#---------------------------------------------#

plot(clim_velocity_temp)

vals <- cbind(
  values(clim_velocity_temp),
  values(hfi_rs)
)

vals <- na.omit(vals)

colnames(vals) <- c("clim_velocity", "hfi")


ggplot(vals,
       aes(x = clim_velocity,
           y = hfi)) +
  geom_hex(bins = 70) +
  scale_fill_viridis_c(
    trans = "log10"
  ) +
  theme_minimal() +
  labs(
    x = "Climate velocity (km/year)",
    y = "Human Footprint Index",
    fill = "Cell density"
  )

#---------------------------------------------#
# how does biodiv importance relate to clim velocity and hfi 
#---------------------------------------------#

# scale 0–1
scale01 <- function(x) {
  (x - global(x, "min", na.rm = TRUE)[1,1]) /
    (global(x, "max", na.rm = TRUE)[1,1] -
       global(x, "min", na.rm = TRUE)[1,1])
}

biodiv_01 <- scale01(biodiv_rs)
clim_01   <- scale01(clim_velocity_temp)
hfi_01    <- scale01(hfi_rs)

# RGB stack
tri_stack <- c(biodiv_01, clim_01, hfi_01)
names(tri_stack) <- c("Biodiversity", "Climate_velocity", "Human_footprint")

# plot trivariate RGB map
threedim<- plotRGB(
  tri_stack,
  r = 1,   # biodiversity
  g = 2,   # climate velocity
  b = 3,   # human footprint
  stretch = "lin"
)

tri_rgb <- scale(tri_stack) * 255
tri_rgb <- round(tri_rgb)

writeRaster(
  tri_rgb,
  file.path(data_storage_path,"Output/priority_indices/threedim_rgb.tif"),
  overwrite = TRUE
)


#----------------------------------------------------------#
# add  legend
#----------------------------------------------------------#

draw_rgb_triangle <- function(
    x_offset = 0,
    y_offset = 0,
    scale = 1,
    n = 100,
    add_labels = TRUE
){
  
  # triangle coordinates
  x <- c(0, 1, 0.5)
  y <- c(0, 0, sqrt(3)/2)
  
  for(i in 1:n){
    for(j in 1:n){
      
      a <- i/n
      b <- j/n
      c <- 1 - a - b
      
      if(c >= 0){
        
        col <- rgb(
          red   = a,
          green = b,
          blue  = c
        )
        
        px <- a*x[1] + b*x[2] + c*x[3]
        py <- a*y[1] + b*y[2] + c*y[3]
        
        points(
          px * scale + x_offset,
          py * scale + y_offset,
          pch = 15,
          cex = 0.5,
          col = col
        )
      }
    }
  }
  
  if(add_labels){
    text(x_offset, y_offset - 2, "biodiv", adj = 0)
    text(x_offset + scale, y_offset - 2,
         "clim vel", adj = 1)
    text(x_offset + scale/2,
         y_offset + sqrt(3)/2 * scale + 2,
         "HFI")
  }
}

# empty plotting area for legend only
par(mar = c(5, 4, 4, 4))

plot(
  NA,
  xlim = c(0, 10),
  ylim = c(-1.5, 10),
  asp = 1,
  axes = FALSE,
  xlab = "",
  ylab = ""
)

draw_rgb_triangle(
  n = 180,
  scale = 8,
  x_offset = 1,
  y_offset = 1
)

#----------------------------------------------------------#
# add  the borders on top of the plot 
#----------------------------------------------------------#
cons_prio <- terra::rast(paste0(data_storage_path, "Output/priority_indices/threedim_rgb.tif"))
border_segments <- sf::st_read(paste0(data_storage_path, "Output/transboundary/borders_segments_100_countrypairs.shp"))
library(terra)
library(sf)

border_segments <- st_transform(border_segments, crs(cons_prio))
border_segments_v <- vect(border_segments)

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
