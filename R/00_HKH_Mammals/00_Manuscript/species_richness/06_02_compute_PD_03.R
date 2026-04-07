
#install.packages(c("rotl", "ape"))
library(rotl)
library(ape)
library(Matrix)
library(picante)
library(terra)
library(dplyr)
library(ggplot2)

# first check out phylogenies with rotl

# there are ~30 species to check for taxonomy see taxa (550 species) vs matched tree (515 species)

species <- colnames(comm_sparse)
head(species)
length(species)

# spaces in names
species_ot <- gsub("_", " ", species)

# match names to Open Tree taxonomy
taxa <- tnrs_match_names(species_ot)

# inspect unmatched names
unmatched <- taxa[is.na(taxa$ott_id), ]
matched   <- taxa[!is.na(taxa$ott_id), ]

nrow(unmatched)
head(unmatched[, c("search_string", "unique_name")])


# all matched species have an ott id
matched_ok <- matched |>
  filter(!is.na(ott_id))

# Check which OTT ids are actually in the synthetic tree
in_tree <- is_in_tree(matched_ok$ott_id)

matched_ok$in_tree <- in_tree

# keep only species that are in the tree
matched_tree <- matched_ok |>
  filter(in_tree)

# build subtree
tr_topology <- tol_induced_subtree(ott_ids = matched_tree$ott_id)


# clean labels
taxon_map <- structure(matched$search_string, names = matched$unique_name)
otl_tips <- strip_ott_ids(tr_topology$tip.label, remove_underscores = TRUE)
tr_topology$tip.label <- gsub(" ", "_", taxon_map[otl_tips])

# plot
plot(tr_topology, cex = 0.3)


# for phylogenetic diversity we need branches. PD = sum of branch lenghts per cell
# download phylogenetic tree manually from vertslife,  (upham 2019)

#---------------------------------------------#
# match names
#---------------------------------------------#

phy <- read.tree(
  file.path(
    data_storage_path,
    "Datasets/phylogenies/mammals_vertslife",
    "MamPhy_BDvr_Completed_5911sp_topoCons_NDexp_v2_tree0000.tre"
  )
)

phy$tip.label <- gsub(" ", "_", phy$tip.label)

shared <- intersect(colnames(comm_sparse), phy$tip.label)

phy <- drop.tip(phy, setdiff(phy$tip.label, shared))
comm_sparse_sub <- comm_sparse[, shared]

# keep only occupied cells
keep_cells <- Matrix::rowSums(comm_sparse_sub) > 0
comm_sparse_sub <- comm_sparse_sub[keep_cells, ]
grid_pd <- grid_info_final[keep_cells, ]


comm_sparse_sub <- comm_sparse_sub[, phy$tip.label]
#---------------------------------------------#
# make the edge matrix
#---------------------------------------------#

make_tip_edge_matrix <- function(phy) {
  n_tips <- length(phy$tip.label)
  n_edges <- nrow(phy$edge)
  
  # children list by parent node
  children <- split(phy$edge[, 2], phy$edge[, 1])
  
  # recursive function: descendant tips of a node
  get_desc_tips <- local({
    memo <- vector("list", max(phy$edge))
    
    f <- function(node) {
      if (!is.null(memo[[node]])) return(memo[[node]])
      
      if (node <= n_tips) {
        memo[[node]] <- node
      } else {
        kids <- children[[as.character(node)]]
        memo[[node]] <- unlist(lapply(kids, f), use.names = FALSE)
      }
      memo[[node]]
    }
    f
  })
  
  i_idx <- integer(0)
  j_idx <- integer(0)
  
  for (e in seq_len(n_edges)) {
    child_node <- phy$edge[e, 2]
    desc_tips <- get_desc_tips(child_node)
    
    i_idx <- c(i_idx, desc_tips)
    j_idx <- c(j_idx, rep(e, length(desc_tips)))
  }
  
  Matrix::sparseMatrix(
    i = i_idx,
    j = j_idx,
    x = 1,
    dims = c(n_tips, n_edges),
    dimnames = list(phy$tip.label, paste0("edge_", seq_len(n_edges)))
  )
}


tip_edge_mat <- make_tip_edge_matrix(phy)
branch_lengths <- phy$edge.length

#---------------------------------------------#
# calculate Faith's phylogenetic diversity in chunks
#---------------------------------------------#

chunk_size <- 5000
n_cells <- nrow(comm_sparse_sub)
cell_chunks <- split(seq_len(n_cells), ceiling(seq_len(n_cells) / chunk_size))

pd_vec <- numeric(n_cells)
sr_vec <- numeric(n_cells)

for (k in seq_along(cell_chunks)) {
  message("Processing chunk ", k, " / ", length(cell_chunks))
  
  idx <- cell_chunks[[k]]
  
  # cell × species
  comm_chunk <- comm_sparse_sub[idx, , drop = FALSE]
  
  # cell × edge: how many descendant species of each branch are present
  edge_counts <- comm_chunk %*% tip_edge_mat
  
  # convert to branch presence/absence
  edge_present <- edge_counts > 0
  
  # Faith's PD = sum(branch lengths of present branches)
  pd_vec[idx] <- as.numeric(edge_present %*% branch_lengths)
  
  # species richness
  sr_vec[idx] <- Matrix::rowSums(comm_chunk)
  
  rm(comm_chunk, edge_counts, edge_present)
  gc()
}

#---------------------------------------------#
# map the PD
#---------------------------------------------#


pd_map_df <- cbind(
  grid_pd[, c("cell", "x", "y")],
  PD = pd_vec,
  SR = sr_vec
)



pd_raster <- rast(
  pd_map_df[, c("x", "y", "PD")],
  type = "xyz",
  crs = crs(template)
)

sr_raster <- rast(
  pd_map_df[, c("x", "y", "SR")],
  type = "xyz",
  crs = crs(template)
)

plot(pd_raster)
plot(sr_raster)

#---------------------------------------------#
# save results
#---------------------------------------------#


out_dir <- file.path(data_storage_path, "Output/phylogenetic_diversity")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

writeRaster(pd_raster, file.path(out_dir, "PD_raster.tif"), overwrite = TRUE)
writeRaster(sr_raster, file.path(out_dir, "SR_raster.tif"), overwrite = TRUE)
saveRDS(pd_map_df, file.path(out_dir, "pd_map_df.rds"))


#---------------------------------------------#
# standardize by total richness
#---------------------------------------------#

# standardize by richness (quick option)
mod <- lm(PD ~ SR, data = pd_map_df)
pd_map_df$PD_resid <- resid(mod)

pd_resid_raster <- rast(
  pd_map_df[, c("x", "y", "PD_resid")],
  type = "xyz",
  crs = crs(template)
)

writeRaster(pd_resid_raster, file.path(out_dir, "PD_residuals_raster.tif"), overwrite = TRUE)

plot(pd_resid_raster, main = "phylogenetic diversity residuals")

