#----------------------------------------------------------#
# pixels of land cover classes within distribution range  -----
#----------------------------------------------------------#

# resample to the eco grid
mammal_elev_masked_res <- resample(mammal_elev_masked,lc,method = "near")
plot(mammal_elev_masked_res)

# get Cell area (km²) on the presence grid 
cell_km2 <- cellSize(mammal_elev_masked_res, unit = "km")

# Total cell count per ecoregion
ones <- mammal_elev_masked_res; values(ones) <- 1

tot_cells <- zonal(ones, lc, fun = "sum", na.rm = TRUE)        # number of cells

# Total area per ecoregion in km2
tot_area  <- zonal(cell_km2, lc, fun = "sum", na.rm = TRUE)     # km²

# Count of presence cells 
pres_cells <- zonal(mammal_elev_masked_res, lc, fun = "sum", na.rm = TRUE)

# Area of presence within each ecoregion in km2
pres_area  <- zonal(cell_km2 * mammal_elev_masked_res, lc, fun = "sum", na.rm = TRUE)