

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

# bind all values together
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

# scale by min and max value 0–1
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


plot(threedim)
writeRaster(
  tri_rgb,
  file.path(data_storage_path,"Output/priority_indices/threedim_rgb.tif"),
  overwrite = TRUE
)

