#----------------------------------------------------------#
# 1. Set up  -----
#----------------------------------------------------------#
library(here)
library(terra)

# Load configuration file
source(here::here("R/00_Config_file.R"))

library(terra)
#----------------------------------------------------------#
#         load chelsa data and mountain range
#----------------------------------------------------------#

annual_temp <- rast(paste0(data_storage_path,"Datasets/Mountains/Chelsa/averaged_climate_time_series/CHELSA_bio1_1981-2010_V.2.1.tif"))
annual_prec <- rast(paste0(data_storage_path,"Datasets/Mountains/Chelsa/averaged_climate_time_series/CHELSA_bio12_1981-2010_V.2.1.tif"))


# this is averaged data for ssp85 2040-2070
annual_temp_ssp85 <- rast(paste0(data_storage_path,"Datasets/Mountains/Chelsa/averaged_climate_time_series/CHELSA_bio1_2041-2070_mpi-esm1-2-hr_ssp585_V.2.1.tif"))
annual_prec_ssp85 <- rast(paste0(data_storage_path,"Datasets/Mountains/Chelsa/averaged_climate_time_series/CHELSA_bio12_2041-2070_mpi-esm1-2-hr_ssp585_V.2.1.tif"))


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

plot(temp_mountain,main = "temp current")

#prec
prec_mountain_crop <- crop(annual_prec, mountain_range)
prec_mountain <- mask(prec_mountain_crop, mountain_range)
plot(prec_mountain,main = "prec current")


# temp future
temp_mountain_crop_future <- crop(annual_temp_ssp85, mountain_range)
temp_mountain_future <- mask(temp_mountain_crop_future, mountain_range)
plot(temp_mountain_future,main = "temp future")

#prec future
prec_mountain_crop_future <- crop(annual_prec_ssp85, mountain_range)
prec_mountain_future <- mask(prec_mountain_crop_future, mountain_range)
plot(prec_mountain_future,main = "prec future")


#----------------------------------------------------------#
#    climate velocity
#----------------------------------------------------------#

# CHELSA bio1 is often stored as °C * 10
annual_temp <- temp_mountain 
annual_temp_ssp85 <- temp_mountain_future 

# temporal change: future minus current
temp_change <- annual_temp_ssp85 - annual_temp
plot(temp_change)

# years between midpoints:
# current = 1981-2010 midpoint ~1995.5
# future = 2041-2070 midpoint ~2055.5
years_diff <- 60

temp_trend <- temp_change / years_diff   # °C/year
plot(temp_trend)

# spatial gradient
slope_temp <- terrain(
  annual_temp,
  v = "slope",
  unit = "radians"
)

temp_gradient_km <- tan(slope_temp) * 1000

# remove tiny gradients to avoid crazy values
temp_gradient_km[temp_gradient_km < 0.001] <- NA

clim_velocity_temp <- abs(temp_trend) / temp_gradient_km
clim_velocity_temp[is.infinite(clim_velocity_temp)] <- NA

x11()
plot(clim_velocity_temp)

#----------------------------------------------------------#
#    climate velocity: precipitation
#----------------------------------------------------------#

annual_prec <- prec_mountain
annual_prec_ssp85 <- prec_mountain_future

# temporal change: future minus current
prec_change <- annual_prec_ssp85 - annual_prec
plot(prec_change, main = "precipitation change")

# annual precipitation trend
prec_trend <- prec_change / years_diff   # mm/year
plot(prec_trend, main = "precipitation trend")

# spatial gradient of current precipitation
slope_prec <- terrain(
  annual_prec,
  v = "slope",
  unit = "radians"
)

# convert to mm/km
prec_gradient_km <- tan(slope_prec) * 1000

# remove tiny gradients to avoid extreme values
prec_gradient_km[prec_gradient_km < 0.001] <- NA

# precipitation velocity: km/year
clim_velocity_prec <- abs(prec_trend) / prec_gradient_km
clim_velocity_prec[is.infinite(clim_velocity_prec)] <- NA

plot(clim_velocity_prec, main = "precipitation climate velocity")

global(
  clim_velocity_prec,
  quantile,
  probs = c(0, 0.5, 0.9, 0.95, 0.99, 1),
  na.rm = TRUE
)

plot(
  log1p(clim_velocity_prec),
  main = "Log precipitation climate velocity"
)

q99 <- global(
  clim_velocity_prec,
  quantile,
  probs = 0.99,
  na.rm = TRUE
)[1,1]

plot(
  clamp(clim_velocity_prec, upper = q99),
  main = "Precipitation climate velocity"
)

#----------------------------------------------------------#
# TIME STAMPS --
#----------------------------------------------------------#
#----------------------------------------------------------#
#       ‼️TIMESTAMPS  load chelsa data and mountain range
#----------------------------------------------------------#
# Load data
annual_temp_<- rast(paste0(data_storage_path,"Datasets/Mountains/Chelsa/raw_download/CHELSA_2.1_0.5m_bio01_v1.0.0_vsi.vrt"))
annual_prec <- rast(paste0(data_storage_path, "Datasets/Mountains/Chelsa/raw_download/CHELSA_2.1_0.5m_bio12_v1.0.0_vsi.vrt"))
plot(annual_temp_)

# load future data
annual_temp_ssp85 <- rast(paste0(data_storage_path,"Datasets/Mountains/Chelsa/time_slices_2055/temp_2055_585.tif"))
annual_prec_ssp85 <- rast(paste0(data_storage_path,"Datasets/Mountains/Chelsa/time_slices_2055/prec_2055_585.tif"))

#----------------------------------------------------------#
#         crop and mask rasters by HKH boundary
#----------------------------------------------------------#

# temp
temp_mountain_crop <- crop(annual_temp, mountain_range)
temp_mountain <- mask(temp_mountain_crop, mountain_range)

plot(temp_mountain,main = "temp current")

#prec
prec_mountain_crop <- crop(annual_prec, mountain_range)
prec_mountain <- mask(prec_mountain_crop, mountain_range)
plot(prec_mountain,main = "prec current")


# temp future
temp_mountain_crop_future <- crop(annual_temp_ssp85, mountain_range)
temp_mountain_future <- mask(temp_mountain_crop_future, mountain_range)
plot(temp_mountain_future,main = "temp future")

#prec future
prec_mountain_crop_future <- crop(annual_prec_ssp85, mountain_range)
prec_mountain_future <- mask(prec_mountain_crop_future, mountain_range)
plot(prec_mountain_future,main = "prec future")


# check out temp range 
global(temp_mountain, range, na.rm = TRUE)
global(temp_mountain_future, range, na.rm = TRUE)
global(temp_change, range, na.rm = TRUE)


#----------------------------------------------------------#
#    climate velocity: temperature
#----------------------------------------------------------#

# Climate velocity asks:
# "How far would a species need to move per year to keep the same temperature?"
#
# It is calculated as:
# climate velocity = temporal temperature change / spatial temperature gradient
#
# Units:
# (°C / year) / (°C / km) = km / year


#----------------------------------------------------------#
# 1. Prepare temperature rasters
#----------------------------------------------------------#

# Current temperature is already in °C
annual_temp <- temp_mountain

# Future CHELSA bio1 appears to be stored as °C * 10,
# so divide by 10 to convert it back to °C
annual_temp_ssp85 <- temp_mountain_future / 10


#----------------------------------------------------------#
# 2. Calculate temperature change
#----------------------------------------------------------#

# Future minus current temperature
# Positive values = warming
# Negative values = cooling
temp_change <- annual_temp_ssp85 - annual_temp

global(temp_change, range, na.rm = TRUE)
plot(temp_change, main = "Temperature change")


#----------------------------------------------------------#
# 3. Convert temperature change to annual trend
#----------------------------------------------------------#

# Current period: 1981–2010, midpoint = 1995.5
# Future time slice: 2055
current_year <- 1995.5
future_year  <- 2055

years_diff <- future_year - current_year

# This gives warming rate in °C per year
temp_trend <- temp_change / years_diff

plot(temp_trend, main = "Temperature trend (°C/year)")


#----------------------------------------------------------#
# 4. Calculate spatial temperature gradient
#----------------------------------------------------------#

# The spatial gradient describes how quickly temperature changes across space.
#
# In mountains, temperature often changes quickly over short distances,
# so the spatial gradient is high.
#
# In flat or climatically uniform areas, temperature changes slowly across space,
# so the spatial gradient is low.

slope_temp <- terrain(
  annual_temp,
  v = "slope",
  unit = "radians"
)

# Convert slope angle to temperature change per km.
# This gives the denominator of climate velocity: °C/km
temp_gradient_km <- tan(slope_temp) * 1000

plot(temp_gradient_km, main = "Spatial temperature gradient (°C/km)")


#----------------------------------------------------------#
# 5. Avoid division by very small gradients
#----------------------------------------------------------#

# Climate velocity becomes unstable when  spatial gradient is very close to zero.
#
# Example:
# 0.03 °C/year / 0.00001 °C/km = extremely large velocity
#
# These huge values are often numerical artefacts from nearly flat climate surfaces.
#
# Instead of setting them to NA, we set a minimum gradient of 0.001 °C/km.
# This means that any gradient smaller than 0.001 is treated as 0.001.
#
# This avoids holes in the map and prevents unrealistically huge values.
# 0.001 °C/km is conservative because it only affects the very weakest gradients.

temp_gradient_km[temp_gradient_km < 0.001] <- 0.001


#----------------------------------------------------------#
# 6. Calculate climate velocity
#----------------------------------------------------------#

# Climate velocity:
# (°C/year) / (°C/km) = km/year
clim_velocity_temp <- abs(temp_trend) / temp_gradient_km

# Remove infinite values if any remain
clim_velocity_temp[is.infinite(clim_velocity_temp)] <- NA


#----------------------------------------------------------#
# 7. Cap extreme outliers for stability/visualisation
#----------------------------------------------------------#

# Loarie-style climate velocity can still produce a few very large outliers.
# These can dominate the colour scale and make the map hard to interpret.
#
# Here we cap values at the 99th percentile.
# This keeps the spatial pattern but prevents a few extreme cells from dominating.

q99 <- global(
  clim_velocity_temp,
  quantile,
  probs = 0.99,
  na.rm = TRUE
)[1, 1]

clim_velocity_temp_capped <- clamp(
  clim_velocity_temp,
  upper = q99
)


#----------------------------------------------------------#
# 8. Plot results
#----------------------------------------------------------#

plot(
  clim_velocity_temp,
  main = "Temperature climate velocity (km/year)"
)

plot(
  clim_velocity_temp_capped,
  main = "Temperature climate velocity, capped at 99th percentile"
)

plot(
  temp_gradient_km,
  main = "Spatial temperature gradient after minimum threshold"
)

#----------------------------------------------------------#
# save results as tif 
#----------------------------------------------------------#

writeRaster(
  clim_velocity_temp_capped,
  "~/Desktop/Manuscripts/Ch_2_HKH_mammals/Datasets/climate_shift/clim_velocity/clim_velocity_temp_capped.tif",
  overwrite = TRUE
)
