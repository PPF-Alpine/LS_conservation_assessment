#----------------------------------------------------------#
# 5. pixels of ecoregions within distribution range  -----
#----------------------------------------------------------#

# resample to the eco grid
mammal_elev_masked_res <- resample(mammal_elev_masked,eco,method = "near")
plot(mammal_elev_masked_res)

# get Cell area (km²) on the presence grid 
cell_km2 <- cellSize(mammal_elev_masked_res, unit = "km")

# Total cell count per ecoregion
ones <- mammal_elev_masked_res; values(ones) <- 1

# number of cells
tot_cells <- zonal(ones, eco, fun = "sum", na.rm = TRUE)        

# Total area per ecoregion in km2
tot_area  <- zonal(cell_km2, eco, fun = "sum", na.rm = TRUE)    

# Count of presence cells 
pres_cells <- zonal(mammal_elev_masked_res, eco, fun = "sum", na.rm = TRUE)

# Area of presence within each ecoregion in km2
pres_area  <- zonal(cell_km2 * mammal_elev_masked_res, eco, fun = "sum", na.rm = TRUE)

