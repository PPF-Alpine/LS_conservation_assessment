
# Local contributions to beta diversity (LCBD) 
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
library(Matrix)
library(dplyr)
library(vegan)
library(adespatial)

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

# =========================================================
# create a subset of the data
# =========================================================
# richness from your matrix
richness <- Matrix::rowSums(sp_communtiy)

# stratified subset
set.seed(1)

q <- quantile(richness, probs = c(0.1, 0.5, 0.9), na.rm = TRUE)

low_idx  <- which(richness <= q[1])
mid_idx  <- which(richness > q[1] & richness <= q[2])
high_idx <- which(richness >= q[3])

idx <- c(
  sample(low_idx,  min(1000, length(low_idx))),
  sample(mid_idx,  min(1000, length(mid_idx))),
  sample(high_idx, min(1000, length(high_idx)))
)

comm_sub <- sp_communtiy[idx, ]
grid_sub <- grid_info[idx, ]

length(idx)                 # initial sampled cells
nrow(comm_sub)              # after subsetting
nrow(comm_sub_dense)        # after removing empty cells
nrow(grid_sub)              # final number used for LCBD

# =========================================================
# Hellinger transformation
# =========================================================

# convert sparse subset to dense matrix
comm_sub_dense <- as.matrix(comm_sub)

# remove empty cells if present
keep <- rowSums(comm_sub_dense) > 0
comm_sub_dense <- comm_sub_dense[keep, , drop = FALSE]
grid_sub <- grid_sub[keep, ]

# Hellinger transform
comm_hel <- vegan::decostand(comm_sub_dense, method = "hellinger")

# =========================================================
# compute LCBD 
# =========================================================

lcbd_obj <- adespatial::beta.div(comm_hel, method = "euclidean", nperm = 999)

grid_sub$LCBD <- lcbd_obj$LCBD
grid_sub$p_LCBD <- lcbd_obj$p.LCBD


# output inspection 
summary(grid_sub$LCBD)
hist(grid_sub$LCBD, breaks = 50, main = "LCBD distribution")
table(grid_sub$p_LCBD < 0.05, useNA = "ifany")



r_lcbd_sub <- rast(template)
values(r_lcbd_sub) <- NA

vals <- values(r_lcbd_sub)
vals[grid_sub$cell] <- grid_sub$LCBD
values(r_lcbd_sub) <- vals

plot(r_lcbd_sub, main = "LCBD (subset cells only)")

plot(grid_sub$richness, grid_sub$LCBD,
     pch = 16, cex = 0.5,
     xlab = "Richness", ylab = "LCBD")


# test the same for full raster/ matrix 

library(Matrix)
library(vegan)
library(adespatial)

# remove empty cells
keep <- Matrix::rowSums(sp_communtiy) > 0
comm_full <- sp_communtiy[keep, ]
grid_full <- grid_info[keep, ]

# dense matrix
comm_dense <- as.matrix(comm_full)

# Hellinger transform
comm_hel <- vegan::decostand(comm_dense, method = "hellinger")

# LCBD without permutations first
lcbd_obj <- adespatial::beta.div(comm_hel, method = "euclidean", nperm = 0)

grid_full$LCBD <- lcbd_obj$LCBD