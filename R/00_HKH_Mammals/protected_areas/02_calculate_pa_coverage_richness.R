
library(terra)
library(sf)
library(sf)
library(dplyr)
library(units)

# Load configuration file
source(here::here("R/00_Config_file_HKH.R"))

#----------------------------------------------------------#
# 0. load files  -----
#----------------------------------------------------------#

# what proportion of richness is covered by PA
# how well are the top 30% of values represented in PA
# which unprotected areas have high richness


richness<-rast(paste0(data_storage_path, "Datasets/species_list/species_richness/HKH_species_richness_TOTAL.tif"))
pa_path <- paste0(data_storage_path, "Datasets/protected_areas/PA_HKH/pa_in_hkh.gpkg")
pa <- st_read(pa_path)

#----------------------------------------------------------#
# calculate proportion of richness covered by PA ----
#----------------------------------------------------------#
pa <- st_transform(pa, crs(richness))   # make sure both have the same projection
total_richness <- global(richness, "sum", na.rm = TRUE)[[1]]

richness_in_pa <- mask(richness, vect(pa))
covered_richness <- global(richness_in_pa, "sum", na.rm = TRUE)[[1]]
proportion_covered <- covered_richness / total_richness
proportion_covered

# 6% of the spatially distributed species richness is covered by PAs

#----------------------------------------------------------#
# how well are the top 30% of values represented in PA ----
#----------------------------------------------------------#
# Extract all cell values as a vector
vals <- values(richness, na.rm = TRUE)

# Get the 70th percentile value
thresh <- quantile(vals, probs = 0.90, na.rm = TRUE)
thresh

# Priority cells: top 30%
priority_cells <- richness >= thresh
plot(priority_cells)

# Priority cells within PAs
priority_in_pa <- mask(priority_cells, vect(pa))
plot(priority_in_pa)

# Count cells
total_priority <- global(priority_cells, "sum", na.rm = TRUE)[[1]]
covered_priority <- global(priority_in_pa, "sum", na.rm = TRUE)[[1]]

proportion_priority_covered <- covered_priority / total_priority
proportion_priority_covered

#----------------------------------------------------------#
# which unprotected areas have high richness ----
#----------------------------------------------------------#

priority_outside_pa <- mask(priority_cells, vect(pa), inverse = TRUE)
plot(priority_outside_pa)

total_priority <- global(priority_cells, "sum", na.rm = TRUE)[[1]]
priority_outside <- global(priority_outside_pa, "sum", na.rm = TRUE)[[1]]

prop_outside <- priority_outside / total_priority
prop_outside

plot(priority_outside_pa, main = "High-richness areas outside PAs")
plot(vect(pa), add = TRUE, border = "red")


#----------------------------------------------------------#
# SENSITIVITY ANALYSIS ----
#----------------------------------------------------------#

sensitivity_priority <- function(richness, pa,
                                 probs = seq(0.5, 0.95, by = 0.05),
                                 tie_rule = c(">", ">="),
                                 plot = TRUE,
                                 area_km2 = TRUE) {
  stopifnot(inherits(richness, "SpatRaster"), nlyr(richness) == 1)
  tie_rule <- match.arg(tie_rule)
  
  # Extract values once and prep layers used repeatedly
  vals <- values(richness, na.rm = TRUE)
  stopifnot(length(vals) > 0)
  pa_vect <- vect(pa)
  
  # Optional: cell areas in km² (accounting for lat/lon)
  if (area_km2) {
    # terra::cellSize returns area; unit="km" gives km^2
    cell_area <- cellSize(richness, unit = "km")
  }
  
  results <- lapply(probs, function(p) {
    thr <- stats::quantile(vals, probs = p, na.rm = TRUE)
    
    # Priority mask as 1/NA so sums count cells and mask is easy
    priority_cells <- if (tie_rule == ">") ifel(richness >  thr, 1, NA)
    else ifel(richness >= thr, 1, NA)
    
    total_priority_cells <- global(priority_cells, "sum", na.rm = TRUE)[[1]]
    
    # Richness restricted to priority cells
    priority_richness <- mask(richness, priority_cells)
    total_priority_richness <- global(priority_richness, "sum", na.rm = TRUE)[[1]]
    
    # Inside / outside PAs
    priority_cells_in  <- mask(priority_cells,  pa_vect)
    priority_cells_out <- mask(priority_cells,  pa_vect, inverse = TRUE)
    
    pr_in  <- mask(priority_richness, pa_vect)
    pr_out <- mask(priority_richness, pa_vect, inverse = TRUE)
    
    # Cell counts
    n_in  <- global(priority_cells_in,  "sum", na.rm = TRUE)[[1]]
    n_out <- global(priority_cells_out, "sum", na.rm = TRUE)[[1]]
    
    # Richness sums (within priority)
    r_in  <- global(pr_in,  "sum", na.rm = TRUE)[[1]]
    r_out <- global(pr_out, "sum", na.rm = TRUE)[[1]]
    
    # Area-weighted (km²), if requested
    if (area_km2) {
      area_prio       <- mask(cell_area, priority_cells)
      area_prio_in    <- mask(area_prio, pa_vect)
      area_prio_out   <- mask(area_prio, pa_vect, inverse = TRUE)
      a_total <- global(area_prio,     "sum", na.rm = TRUE)[[1]]
      a_in    <- global(area_prio_in,  "sum", na.rm = TRUE)[[1]]
      a_out   <- global(area_prio_out, "sum", na.rm = TRUE)[[1]]
    } else {
      a_total <- a_in <- a_out <- NA_real_
    }
    
    if (is.na(total_priority_cells) || total_priority_cells == 0) {
      return(tibble::tibble(
        prob = p, thr = as.numeric(thr),
        prop_cells_in = NA_real_, prop_cells_out = NA_real_,
        prop_rich_in  = NA_real_, prop_rich_out  = NA_real_,
        n_priority = 0, sum_priority_rich = 0,
        area_priority_km2 = a_total, area_in_km2 = a_in, area_out_km2 = a_out,
        prop_area_in = NA_real_, prop_area_out = NA_real_
      ))
    }
    
    tibble::tibble(
      prob = p,
      thr = as.numeric(thr),
      # proportions among priority cells
      prop_cells_in  = n_in  / total_priority_cells,
      prop_cells_out = n_out / total_priority_cells,
      prop_rich_in   = ifelse(is.na(total_priority_richness) || total_priority_richness == 0,
                              NA_real_, r_in  / total_priority_richness),
      prop_rich_out  = ifelse(is.na(total_priority_richness) || total_priority_richness == 0,
                              NA_real_, r_out / total_priority_richness),
      # diagnostics
      n_priority = total_priority_cells,
      sum_priority_rich = total_priority_richness,
      # area metrics (km²)
      area_priority_km2 = a_total,
      area_in_km2 = a_in,
      area_out_km2 = a_out,
      prop_area_in = ifelse(is.na(a_total) || a_total == 0, NA_real_, a_in  / a_total),
      prop_area_out= ifelse(is.na(a_total) || a_total == 0, NA_real_, a_out / a_total)
    )
  }) |> bind_rows()
  
  if (plot) {
    oldpar <- par(no.readonly = TRUE); on.exit(par(oldpar))
    plot(results$prob, results$prop_cells_in, type = "l", lwd = 2,
         xlab = "Quantile cutoff (p).  Top share = 1 - p",
         ylab = "Proportion inside PAs",
         main = "Coverage of priority cells across thresholds")
    lines(results$prob, results$prop_rich_in, lwd = 2, lty = 2)
    legend("topright",
           legend = c("Cells inside (priority)", "Richness inside (priority)"),
           lty = c(1, 2), lwd = 2, bty = "n")
  }
  
  results
}

# Top 50% down to top 5% (p = 0.50..0.95), strict threshold, with plot + area metrics
res_ric <- sensitivity_priority(richness, pa,
                            probs = seq(0.50, 0.95, by = 0.05),
                            tie_rule = ">", plot = TRUE, area_km2 = TRUE)

# Clean table with explicit "top share"
res_mod_ric <- res_ric |>
  mutate(top_share = 1 - prob) |>
  select(top_share, thr,
         prop_cells_in, prop_rich_in, prop_area_in,
         n_priority, sum_priority_rich, area_priority_km2)



plot_ric<- ggplot(res_mod_ric, aes(x = top_share, y = prop_rich_in)) +
  geom_line(color = "darkred", size = 1.2) +
  geom_point(size = 2) +
  scale_x_reverse(
    limits = c(0.50, 0.05),
    breaks = seq(0.50, 0.05, by = -0.05),
    labels = function(b) paste0("Top ", scales::percent(b, accuracy = 1)),
    sec.axis = sec_axis(~ 1 - ., name = "Quantile cutoff (p)",
                        breaks = seq(0.50, 0.95, by = 0.05),
                        labels = scales::percent_format(accuracy = 1))
  ) +
  scale_y_continuous(
    limits = c(0, max(res_mod$prop_rich_in, na.rm = TRUE)),
    labels = scales::percent_format(accuracy = 0.1),
    name = "Share of richest cells inside PAs"
  ) +
  labs(
    x = "Threshold for species richness cells",
    title = "How well protected are areas with the highest species richness?"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

plot(plot)

# optional: save a high-res imageggsave
ggsave(
  filename = paste0(data_storage_path, "Datasets/species_list/species_richness/species_richness_in_PA.png"),
  plot = plot,
  width = 12,
  height = 6,
  dpi = 300
)
