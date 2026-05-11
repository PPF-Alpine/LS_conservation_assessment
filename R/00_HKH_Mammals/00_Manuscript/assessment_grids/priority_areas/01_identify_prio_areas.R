source(here::here("R/00_Config_file_HKH.R"))

#---------------------------------------------#
# Input data








#---------------------------------------------#
biodiv_imp <- terra::rast(file.path(data_storage_path, "Output/biodiv_dimensions/biodiv_imp.tif"))

terra::plot(biodiv_imp)




