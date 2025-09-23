#----------------------------------------------------------#
# 0. Set up  -----
#----------------------------------------------------------#
library(here)
#devtools::install_github("alrobles/mdd")
library(rasterdiv)
library(terra) # for plot

# Load configuration file
source(here::here("R/00_Config_file_HKH.R"))

#----------------------------------------------------------#
# 0. load files  -----
#----------------------------------------------------------#

# load files 
# dem 
# lc 
# species list 
# ecoregions

lc_fraction <- rast(paste0(data_storage_path, "Datasets/land_cover/fraction_land_cover_new.tif")) 

prop_1<-lc_fraction$lc_prop_1
plot(prop_1)

# lc description
lc_data_descr <- readxl::read_excel(paste0(data_storage_path,"Datasets/land_cover/lc_data_description.xlsx"))


species_rast<-rast(paste0(data_storage_path, "Datasets/species_list/species_richness/HKH_species_richness_TOTAL.tif"))

#----------------------------------------------------------#
# 1. compute shannon diversity index for the cells-----
#----------------------------------------------------------#

# Fractions should be 0..1 and sum to 1 per pixel; normalize robustly and handle NAs/zeros.
normalize_fractions <- function(x) {
  if (all(is.na(x))) return(rep(NA_real_, length(x)))
  x[x < 0] <- 0
  x[x > 1] <- 1
  s <- sum(x, na.rm = TRUE)
  if (!is.finite(s) || s == 0) return(rep(0, length(x)))
  x / s
}

lc_norm <- app(lc_fraction, normalize_fractions)

# shannon
K <- nlyr(lc_norm)

shannon_fun <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  p <- x
  p <- p[p > 0]               # 0*log(0) is 0, so drop zeros
  if (length(p) == 0) return(0)
  -sum(p * log(p))            # natural log
}


shannon  <- app(lc_norm, shannon_fun)

names(shannon)  <- "lc_shannon"
plot(shannon)

out_file <- file.path(data_storage_path, "Datasets", "land_cover","shannon_lc_diversity.tif")
writeRaster(shannon, out_file, overwrite = TRUE)

#----------------------------------------------------------#
# 1. compute land cover richness ----
#----------------------------------------------------------#

richness_fun <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  sum(x > 0, na.rm = TRUE)
}
lcRich   <- app(lc_norm, richness_fun)
plot(lcRich)


#---------------------------------------------------------------------#
# write to new file from here --> relate lc diversity to richness ----
#----------------------------------------------------------#

shannon<-rast(paste0(data_storage_path, "Datasets/land_cover/shannon_lc_diversity.tif"))

richness_aligned <- resample(species_rast,shannon, method = "bilinear")
plot(shannon)


# 5) Now you can stack & sample
stacked <- c(shannon, richness_aligned)
names(stacked) <- c("lc_shannon", "richness")

set.seed(1)
pts <- spatSample(stacked, size = 80000, method = "regular",
                  na.rm = TRUE, as.points = TRUE, values = TRUE, xy = TRUE)
df <- as.data.frame(pts)

library(mgcv)
m_pois <- glm(richness ~ lc_shannon, data = df, family = poisson())
summary(m_pois)


library(ggplot2)

set.seed(1)
df_plot <- df[sample(nrow(df), min(30000, nrow(df))), ]  # light subset

ggplot(df_plot, aes(lc_shannon, richness)) +
  geom_point(alpha = 0.15, size = 0.6) +
  geom_smooth(method = "glm",
              method.args = list(family = poisson(link = "log")),
              se = TRUE) +
  labs(x = "Land-cover Shannon (H')",
       y = "Species richness",
       title = "Richness vs. land-cover heterogeneity") +
  theme_minimal()

