
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


# =========================================================
# compute LCBD for full raster. computationally difficult
# =========================================================

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


# =========================================================
# USE THIS FUNCTION INSTEAD
# =========================================================


compute_lcbd_sparse <- function(comm, grid_info) {
  keep <- Matrix::rowSums(comm) > 0
  comm <- comm[keep, ]
  grid <- grid_info[keep, ]
  
  richness <- Matrix::rowSums(comm)
  
  H <- Diagonal(x = 1 / sqrt(richness)) %*% comm
  col_means <- Matrix::colMeans(H)
  
  row_norm2  <- Matrix::rowSums(H^2)
  Hm         <- as.numeric(H %*% col_means)
  mean_norm2 <- sum(col_means^2)
  
  row_ss <- row_norm2 - 2 * Hm + mean_norm2
  row_ss[row_ss < 0] <- 0
  
  LCBD <- row_ss / sum(row_ss)
  
  grid <- grid |>
    mutate(
      richness = richness,
      LCBD = LCBD,
      LCBD_pct = rank(LCBD, na.last = "keep") / sum(!is.na(LCBD)),
      LCBD_hotspot_5 = LCBD_pct > 0.95,
      LCBD_hotspot_1 = LCBD_pct > 0.99
    )
  
  list(grid = grid, keep = keep)
}

lcbd_res <- compute_lcbd_sparse(sp_communtiy, grid_info)
grid_full <- lcbd_res$grid

# =========================================================
# visualization 
# =========================================================

make_metric_raster <- function(template, cell_ids, values_vec) {
  r <- rast(template)
  values(r) <- NA
  vals <- values(r)
  vals[cell_ids] <- values_vec
  values(r) <- vals
  r
}


r_lcbd <- make_metric_raster(
  template = template,
  cell_ids = grid_full$cell,
  values_vec = grid_full$LCBD
)


r_lcbd_pct <- make_metric_raster(
  template = template,
  cell_ids = grid_full$cell,
  values_vec = grid_full$LCBD_pct
)

r_lcbd_hot5 <- make_metric_raster(
  template = template,
  cell_ids = grid_full$cell,
  values_vec = as.numeric(grid_full$LCBD_hotspot_5)
)



plot(r_lcbd,
     col = hcl.colors(100, "viridis"),
     main = "LCBD")


plot(r_lcbd_pct,
     col = hcl.colors(100, "viridis"),
     main = "LCBD percentile")


plot(r_lcbd_hot5,
     col = c("grey90", "red"),
     legend = FALSE,
     main = "Top 5% LCBD hotspots")

legend("bottomleft",
       legend = c("Other cells", "Hotspot"),
       fill = c("grey90", "red"),
       bty = "n")



# visualization scatterplot 

set.seed(1)
df_plot <- grid_full[sample(nrow(grid_full), 50000), ]  # or 10k

p <- ggplot(df_plot, aes(x = richness, y = LCBD)) +
  geom_point(alpha = 0.3, size = 0.5) +
  theme_minimal()

print(p)


# =========================================================
# save raster grid 
# =========================================================

writeRaster(r_lcbd, paste0(data_storage_path, "Output/uniqueness/r_lcbd.tif"))

ggsave(
  filename = paste0(data_storage_path, "Output/uniqueness/richness_lcbd.jpeg"),
  plot = p,
  width = 14,
  height = 9,
  dpi = 300
)
