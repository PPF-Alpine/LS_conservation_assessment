library(raster)
library(sf)
library(tidyverse)

lc_data_descr <- readxl::read_excel(paste0(data_storage_path,"Datasets/land_cover/lc_data_description.xlsx"))
#----------------------------------------------------------#
# 5.1 polygonize distribution range  -----
#----------------------------------------------------------#

mammal_poly <- terra::as.polygons(mammal_elev_masked, na.rm = TRUE, dissolve = TRUE)
plot(mammal_poly)

#----------------------------------------------------------#
# 5. pixels of ecoregions and landcover within distribution range  -----
#----------------------------------------------------------#

# count the numbers of each eco region within range
eco_in_poly <- mask(crop(eco, mammal_poly), mammal_poly)
plot(eco_in_poly)

lc_in_poly <- mask(crop(lc,mammal_poly),mammal_poly)

# counts per ecoregion
eco_counts <- freq(eco_in_poly) |> 
  as.data.frame() |> 
  dplyr::rename(category = value)|>
  mutate(source = "ecoregion")|>
  dplyr::select(-layer)|>
  mutate(
    # compute ratio, then format as percentage
    proportion = round(count / sum(count, na.rm = TRUE) * 100, 1)                   
  ) 
  
# counts per landcover
lc_counts <- freq(lc_in_poly) |> 
  as.data.frame() |> 
  dplyr::rename(landcover_id = value)|>
  mutate(source = "land_cover")|>
  left_join(lc_data_descr,by="landcover_id")|>
  dplyr::select(lc_class,count,source)|>
  rename(category=lc_class)|>
  mutate(
    # compute ratio, then format as percentage
    proportion = round(count / sum(count, na.rm = TRUE) * 100, 1)                   
  ) 

#----------------------------------------------------------#
# combine in df 
#----------------------------------------------------------#
# 
combined <- rbind(eco_counts,lc_counts)



