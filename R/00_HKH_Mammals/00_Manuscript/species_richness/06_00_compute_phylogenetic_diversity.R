library(terra)
library(sf)
library(stringr)
library(purrr)
library(dplyr)
library(tibble)

source(here::here("R/00_Config_file_HKH.R"))

species_from_file <- function(path, with_space = FALSE) {
  bn <- tools::file_path_sans_ext(basename(path))
  parts <- strsplit(bn, "_", fixed = TRUE)[[1]]
  sp <- parts[1:min(2, length(parts))]
  if (with_space) paste(sp, collapse = " ") else paste(sp, collapse = "_")
}

align_to_dem <- function(r, template) {
  if (!identical(crs(r), crs(template))) {
    r <- project(r, template, method = "near")
  }
  r <- resample(r, template, method = "near")
  r <- extend(r, template)
  r <- ifel(is.na(r), 0, r)
  r
}

process_species_chunk <- function(files_subset, template, chunk_id, out_dir) {
  
  message("Processing chunk ", chunk_id, " with ", length(files_subset), " rasters")
  
  ras_aligned <- lapply(files_subset, function(fp) {
    r <- rast(fp)
    r <- align_to_dem(r, template)
    r <- ifel(r > 0, 1, 0)
    
    nm <- species_from_file(fp, with_space = FALSE)
    names(r) <- nm
    r
  })
  
  ln <- vapply(ras_aligned, names, character(1))
  if (any(duplicated(ln))) {
    new_names <- make.unique(ln, sep = "_")
    for (i in seq_along(ras_aligned)) {
      names(ras_aligned[[i]]) <- new_names[i]
    }
  }
  
  stk <- rast(ras_aligned)
  
  chunk_df <- as.data.frame(stk, cells = TRUE, xy = FALSE, na.rm = FALSE)
  
  valid_cells <- !is.na(values(template))
  chunk_df <- chunk_df[valid_cells, ]
  
  out_file <- file.path(out_dir, paste0("comm_chunk_", sprintf("%03d", chunk_id), ".rds"))
  saveRDS(chunk_df, out_file)
  
  rm(ras_aligned, stk, chunk_df)
  gc()
  
  out_file
}

# template
dem_crop <- rast(paste0(data_storage_path, "Datasets/DEM_HKH/DEM_HKH.tif"))
template <- dem_crop

# file list
in_dir1 <- file.path(data_storage_path, "Datasets", "species_list", "rasterfiles")
in_dir2 <- file.path(data_storage_path, "Datasets", "species_list", "rasterfiles", "missing_species")

files1 <- list.files(in_dir1, pattern = "\\.tif$", full.names = TRUE)
files2 <- list.files(in_dir2, pattern = "\\.tif$", full.names = TRUE)
files <- unique(c(files1, files2))

message("Found ", length(files), " rasters in total.")

# grid info
grid_info <- as.data.frame(template, cells = TRUE, xy = TRUE, na.rm = FALSE) |>
  select(cell, x, y)

valid_cells <- !is.na(values(template))
grid_info <- grid_info[valid_cells, ]

# chunks
chunk_size <- 50
file_chunks <- split(files, ceiling(seq_along(files) / chunk_size))

out_dir <- file.path(data_storage_path, "Output", "phylo_comm_chunks")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

chunk_files <- map_chr(seq_along(file_chunks), function(i) {
  process_species_chunk(
    files_subset = file_chunks[[i]],
    template = template,
    chunk_id = i,
    out_dir = out_dir
  )
})





library(Matrix)

