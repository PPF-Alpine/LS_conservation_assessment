#----------------------------------------------------------#
# 1. Set up  -----
#----------------------------------------------------------#
library(here)
library(terra)

# Load configuration file
source(here::here("R/00_Config_file.R"))


#----------------------------------------------------------#
#         load chelsa data and mountain range
#----------------------------------------------------------#

annual_temp <- rast(paste0(data_storage_path,"Datasets/Mountains/Chelsa/averaged_climate_time_series/CHELSA_bio1_1981-2010_V.2.1.tif"))
annual_prec <- rast(paste0(data_storage_path,"Datasets/Mountains/Chelsa/averaged_climate_time_series/CHELSA_bio12_1981-2010_V.2.1.tif"))


# this is averaged data for ssp85 2040-2070
annual_temp_ssp85 <- rast(paste0(data_storage_path,"Datasets/Mountains/Chelsa/averaged_climate_time_series/CHELSA_bio1_2041-2070_mpi-esm1-2-hr_ssp585_V.2.1.tif"))
annual_prec_ssp85 <- rast(paste0(data_storage_path,"Datasets/Mountains/Chelsa/averaged_climate_time_series/CHELSA_bio12_2041-2070_mpi-esm1-2-hr_ssp585_V.2.1.tif"))

#----------------------------------------------------------#
#       ‼️TIMESTAMPS  load chelsa data and mountain range
#----------------------------------------------------------#
# Load data
annual_temp <- rast(paste0(data_storage_path,"Datasets/Mountains/Chelsa/raw_download/CHELSA_2.1_0.5m_bio01_v1.0.0_vsi.vrt"))
annual_prec <- rast(paste0(data_storage_path, "Datasets/Mountains/Chelsa/raw_download/CHELSA_2.1_0.5m_bio12_v1.0.0_vsi.vrt"))


# load future data
annual_temp_ssp85 <- rast(paste0(data_storage_path,"Datasets/Mountains/Chelsa/time_slices_2055/temp_2055_585.tif"))
annual_prec_ssp85 <- rast(paste0(data_storage_path,"Datasets/Mountains/Chelsa/time_slices_2055/prec_2055_585.tif"))


# source HKH boundary
hkh_boundary <- sf::st_read("~/Desktop/Manuscripts/Ch_2_HKH_mammals/Datasets/HKH_boundary/HKH_boundary.shp")


# select a mountain range
mountain_range <- hkh_boundary

mountain_range_name <- mountain_range$Mntn_rn


#----------------------------------------------------------#
#         crop and mask rasters by HKH boundary
#----------------------------------------------------------#

# temp
temp_mountain_crop <- crop(annual_temp, mountain_range)
temp_mountain <- mask(temp_mountain_crop, mountain_range)
x11()
plot(temp_mountain)

#prec
prec_mountain_crop <- crop(annual_prec, mountain_range)
prec_mountain <- mask(prec_mountain_crop, mountain_range)


# temp future
temp_mountain_crop_future <- crop(annual_temp_ssp85, mountain_range)
temp_mountain_future <- mask(temp_mountain_crop_future, mountain_range)
plot(temp_mountain_future)

#prec future
prec_mountain_crop_future <- crop(annual_prec_ssp85, mountain_range)
prec_mountain_future <- mask(prec_mountain_crop_future, mountain_range)

#----------------------------------------------------------#
#      calculate temp and prec difference
#----------------------------------------------------------#

temp_diff <- temp_mountain_future - temp_mountain
plot(temp_diff, main = "Temperature change")


prec_diff <- prec_mountain_future - prec_mountain
plot(prec_diff, main = "Precipitation change")
# positive = wetter
# negative = drier

prec_diff_pct <- ifel(prec_mountain == 0, NA, ((prec_mountain_future - prec_mountain) / prec_mountain) * 100)
plot(prec_diff_pct, main = "Precipitation change (%)")

#----------------------------------------------------------#
#      absolute changes 
#----------------------------------------------------------#
temp_diff_abs <- abs(temp_diff)
plot(temp_diff_abs)
prec_diff_pct_abs <- abs(prec_diff_pct)

#----------------------------------------------------------#
#     scale the rasters because different units
#----------------------------------------------------------#
temp_scaled <- (temp_diff_abs - global(temp_diff_abs, "min", na.rm=TRUE)[1,1]) /
  (global(temp_diff_abs, "max", na.rm=TRUE)[1,1] - global(temp_diff_abs, "min", na.rm=TRUE)[1,1])
plot(temp_scaled)

prec_scaled <- (prec_diff_pct_abs - global(prec_diff_pct_abs, "min", na.rm=TRUE)[1,1]) /
  (global(prec_diff_pct_abs, "max", na.rm=TRUE)[1,1] - global(prec_diff_pct_abs, "min", na.rm=TRUE)[1,1])

climate_exposure <- (temp_scaled + prec_scaled) / 2
x11()
plot(climate_exposure, main = "Combined climate change")

#----------------------------------------------------------#
#     select the ones with highest projecte change the rasters 
#----------------------------------------------------------#
# top 25% as high exposure
thresh <- quantile(values(climate_exposure), probs = 0.75, na.rm = TRUE)
climate_high <- ifel(climate_exposure >= thresh, 1, 0)
plot(climate_high, main = "High climate exposure")

writeRaster(
  climate_exposure,
  "~/Desktop/Manuscripts/Ch_2_HKH_mammals/Datasets/climate_shift/climate_exposure.tif",
  overwrite = TRUE
)

