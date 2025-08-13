#----------------------------------------------------------#
# 2. rasterize species distribution  -----
#----------------------------------------------------------#

mammal$val <- 1
mammal_raster <- rasterize(mammal, dem_crop, field = "val", background = 0, touches = TRUE)
# crop to remove extra 0s
mammal_raster <- mask(mammal_raster, mammal)
plot(mammal_raster)