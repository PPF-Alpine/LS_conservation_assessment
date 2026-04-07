
library(terra)
library(sf)
library(stringr)
library(purrr)
library(dplyr)
library(tibble)
library(Matrix)


source(here::here("R/00_Config_file_HKH.R"))


# build community matrix 

#---------------------------------------------#
#  read and combine chunk files
#---------------------------------------------#
chunk_dir <- file.path(data_storage_path, "Output", "phylo_comm_chunks")

chunk_files <- list.files(
  chunk_dir,
  pattern = "\\.rds$",
  full.names = TRUE
)

# make sure they are in correct order
chunk_files <- sort(chunk_files)

length(chunk_files)


#prepare the template grid info
dem_crop <- rast(paste0(data_storage_path, "Datasets/DEM_HKH/DEM_HKH.tif"))
template <- dem_crop

grid_info <- as.data.frame(template, cells = TRUE, xy = TRUE, na.rm = FALSE) |>
  select(cell, x, y)

valid_cells <- !is.na(values(template))
grid_info <- grid_info[valid_cells, ]


# read prepared chunk files
comm_df <- readRDS(chunk_files[1])

for (i in 2:length(chunk_files)) {
  chunk_i <- readRDS(chunk_files[i])
  stopifnot(identical(comm_df$cell, chunk_i$cell))
  
  comm_df <- cbind(
    comm_df,
    chunk_i[, setdiff(names(chunk_i), "cell"), drop = FALSE]
  )
  
  rm(chunk_i)
  gc()
}

#---------------------------------------------#
# remove duplicated species columns
# 
#---------------------------------------------#

comm_df <- comm_df[, !duplicated(names(comm_df)), drop = FALSE]

#---------------------------------------------#
# keep coordinates in a look up table
#---------------------------------------------#

idx <- match(comm_df$cell, grid_info$cell)
grid_info_final <- grid_info[idx, c("cell", "x", "y")]

#---------------------------------------------#
# build  matrix
#---------------------------------------------#

species_cols <- setdiff(names(comm_df), "cell")

comm_sparse <- Matrix(
  as.matrix(comm_df[, species_cols, drop = FALSE]),
  sparse = TRUE
)

comm_sparse[is.na(comm_sparse)] <- 0

#---------------------------------------------#
#  remove empty cells
#---------------------------------------------#

keep <- Matrix::rowSums(comm_sparse) > 0

comm_sparse <- comm_sparse[keep, ]
grid_info_final <- grid_info_final[keep, ]

#---------------------------------------------#
# 6. save
#---------------------------------------------#

saveRDS(comm_sparse, file.path(data_storage_path, "Output/phylo_comm_chunks/comm_matrix/comm_sparse.rds"))
saveRDS(grid_info_final, file.path(data_storage_path, "Output/phylo_comm_chunks/comm_matrix/grid_info_final.rds"))

