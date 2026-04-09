# =========================================================
# 1. Setup
# =========================================================
library(terra)
library(sf)
library(stringr)
library(purrr)
library(dplyr)
library(tibble)
library(Matrix)

source(here::here("R/00_Config_file_HKH.R"))

# =========================================================
# 2. Load data
# =========================================================

# Species community matrix: rows = valid cells, cols = species
sp_communtiy <- readRDS(
  file.path(data_storage_path,
            "Output/phylo_comm_chunks/comm_matrix/comm_sparse_with_NA.rds")
)

# Template raster
template <- rast(file.path(data_storage_path, "Datasets/DEM_HKH/DEM_HKH.tif"))

# Grid info for all cells
grid_info <- as.data.frame(template, cells = TRUE, xy = TRUE, na.rm = FALSE) |>
  select(cell, x, y)

# Keep only valid raster cells
valid_cells <- !is.na(values(template))
grid_info <- grid_info[valid_cells, ]

# Sanity check
stopifnot(nrow(grid_info) == nrow(sp_communtiy))

# =========================================================
# 3. Calculate cell-level metrics
# =========================================================

# Species richness per cell
richness <- Matrix::rowSums(sp_communtiy)

# Species occupancy across all cells
occ <- Matrix::colSums(sp_communtiy)
occ[occ == 0] <- NA

# Rarity weights
w <- 1 / occ

# Total rarity-weighted uniqueness
uniq_weighted <- as.numeric(sp_communtiy %*% w)

# Mean rarity per species in the cell
uniq_weighted_mean <- uniq_weighted / richness
uniq_weighted_mean[richness == 0] <- NA

# Log-transform for visualization
log_uniq <- log10(uniq_weighted_mean + 1e-10)

# Percentile rank for mapping hotspots
uniq_pct <- rank(uniq_weighted_mean, na.last = "keep") /
  sum(!is.na(uniq_weighted_mean))

# Hotspots: top 5%
hotspot_5 <- uniq_pct > 0.95

# Store in grid_info
grid_info <- grid_info |>
  mutate(
    richness = richness,
    uniq_weighted = uniq_weighted,
    uniq_weighted_mean = uniq_weighted_mean,
    log_uniq = log_uniq,
    uniq_pct = uniq_pct,
    hotspot_5 = hotspot_5
  )

# =========================================================
# 4. Helper function: put values back into raster
# =========================================================

make_metric_raster <- function(template, valid_cells, values_vec) {
  r <- rast(template)
  values(r) <- NA
  values(r)[valid_cells] <- values_vec
  r
}

# Create rasters
r_richness   <- make_metric_raster(template, valid_cells, grid_info$richness)
r_uniq_total <- make_metric_raster(template, valid_cells, grid_info$uniq_weighted)
r_uniq_mean  <- make_metric_raster(template, valid_cells, grid_info$uniq_weighted_mean)
r_log_uniq   <- make_metric_raster(template, valid_cells, grid_info$log_uniq)
r_uniq_pct   <- make_metric_raster(template, valid_cells, grid_info$uniq_pct)
r_hotspot_5  <- make_metric_raster(template, valid_cells, as.numeric(grid_info$hotspot_5))

# =========================================================
# 5. Summaries
# =========================================================

summary(grid_info$richness)
summary(grid_info$uniq_weighted)
summary(grid_info$uniq_weighted_mean)
summary(grid_info$log_uniq)
summary(grid_info$uniq_pct)

table(grid_info$hotspot_5, useNA = "ifany")

# =========================================================
# 6. Histograms
# =========================================================

par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

hist(grid_info$uniq_weighted_mean,
     breaks = 50,
     main = "Histogram of mean uniqueness",
     xlab = "uniq_weighted_mean",
     col = "grey85",
     border = "white")

hist(grid_info$log_uniq,
     breaks = 50,
     main = "Histogram of log uniqueness",
     xlab = "log10(uniq_weighted_mean + 1e-10)",
     col = "grey85",
     border = "white")

hist(grid_info$uniq_pct,
     breaks = 50,
     main = "Histogram of uniqueness percentile",
     xlab = "uniq_pct",
     col = "grey85",
     border = "white")

plot(grid_info$richness, grid_info$uniq_weighted_mean,
     pch = 16, cex = 0.3,
     xlab = "Richness",
     ylab = "Mean uniqueness",
     main = "Richness vs mean uniqueness")

par(mfrow = c(1, 1))

# =========================================================
# 7. Clean maps
# =========================================================

# Choose a nice palette
pal <- hcl.colors(100, "viridis")

par(mfrow = c(2, 3), mar = c(3, 3, 3, 5))

plot(r_richness,
     col = pal,
     main = "Species richness")

plot(r_uniq_total,
     col = pal,
     main = "Total rarity-weighted uniqueness")

plot(r_uniq_mean,
     col = pal,
     main = "Mean rarity-weighted uniqueness")

plot(r_log_uniq,
     col = pal,
     main = "Log mean uniqueness")

plot(r_uniq_pct,
     col = pal,
     main = "Uniqueness percentile")

plot(r_hotspot_5,
     col = c("grey90", "red"),
     legend = FALSE,
     main = "Top 5% uniqueness hotspots")

legend("bottomleft",
       legend = c("Other cells", "Hotspot"),
       fill = c("grey90", "red"),
       bty = "n")

par(mfrow = c(1, 1))

