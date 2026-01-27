library(terra)
library(stringr)
library(terra)
library(sf)
library(stringr)
library(purrr)
library(dplyr)
library(tibble)
library(tools)
library(stringr)

#---------------------------------------------#
# Step 1: list files
#---------------------------------------------#
source(here::here("R/00_Config_file_HKH.R"))

total_endemism_join <- read.csv(paste0(data_storage_path,"Datasets/species_list/species_endemism/total_species_endemism.csv"))|>
  filter(!is.na(pct_in_HKH_area), pct_in_HKH_area > 0)

species_list <- readxl::read_excel(paste0(data_storage_path,"Datasets/species_list/assessment_hkh_mammals_09082025_LS.xlsx"))|>
  rename(species = sciname)

# join status code to range 
list_join <- total_endemism_join |>
  left_join(
    species_list |>
      select(species, status_summary_global,status_code_global,status_summary_national,status_code_national) |>
      distinct(),
    by = "species"
  )


#-------------------------------------------------#
# test range ~ threathened species  GLOBAL status
#-------------------------------------------------#

# elev range
# total area km2
# pct in HKH area

# status summary global 
# status summary national

list_join$concern_bin <- ifelse(
  list_join$status_summary_global %in% c("threatened", "data deficient","NA"),
  1,  # conservation concern
  0   # not threatened
)

m_elev <- glm(concern_bin ~ elev_range,        family = binomial, data = list_join)
m_area <- glm(concern_bin ~ total_area_km2,    family = binomial, data = list_join)
m_pct  <- glm(concern_bin ~ pct_in_HKH_area,   family = binomial, data = list_join)


#---------------------------------------------#
# plot the models  
#---------------------------------------------#
library(ggeffects)

pred_elev <- ggpredict(m_elev, terms = "elev_range [all]") %>% mutate(variable = "Elevational range (m)")
pred_area <- ggpredict(m_area, terms = "total_area_km2 [all]") %>% mutate(variable = "Total area (km²)")
pred_pct  <- ggpredict(m_pct,  terms = "pct_in_HKH_area [all]") %>% mutate(variable = "% of range in HKH")

pred_all <- bind_rows(pred_elev, pred_area, pred_pct)

plot_global <- ggplot(pred_all, aes(x = x, y = predicted)) +
  geom_ribbon(
    aes(ymin = conf.low, ymax = conf.high),
    fill = "grey75",
    alpha = 0.4
  ) +
  geom_line(linewidth = 1) +
  facet_wrap(~ variable, scales = "free_x") +
  labs(x = NULL, y = "Predicted probability of conservation concern") +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.6),
    panel.spacing = unit(0.8, "lines"),
    strip.background = element_rect(fill = "grey90", colour = "black"),
    strip.text = element_text(size = 13),
    axis.title.y = element_text(size = 13),
    axis.text = element_text(size = 12)
  )

plot_global
#-------------------------------------------------#
# test range ~ threathened species  NATIONAL status
#-------------------------------------------------#

# elev range
# total area km2
# pct in HKH area

# status summary global 
# status summary national

list_join$concern_bin <- ifelse(
  list_join$status_summary_national %in% c("threatened", "data deficient","NA"),
  1,  # conservation concern
  0   # not threatened
)

m_elev <- glm(concern_bin ~ elev_range,        family = binomial, data = list_join)
m_area <- glm(concern_bin ~ total_area_km2,    family = binomial, data = list_join)
m_pct  <- glm(concern_bin ~ pct_in_HKH_area,   family = binomial, data = list_join)


#---------------------------------------------#
# plot the models  
#---------------------------------------------#
library(ggeffects)

pred_elev <- ggpredict(m_elev, terms = "elev_range [all]") %>% mutate(variable = "Elevational range (m)")
pred_area <- ggpredict(m_area, terms = "total_area_km2 [all]") %>% mutate(variable = "Total area (km²)")
pred_pct  <- ggpredict(m_pct,  terms = "pct_in_HKH_area [all]") %>% mutate(variable = "% of range in HKH")

pred_all <- bind_rows(pred_elev, pred_area, pred_pct)

plot_national <- ggplot(pred_all, aes(x = x, y = predicted)) +
  geom_ribbon(
    aes(ymin = conf.low, ymax = conf.high),
    fill = "grey75",
    alpha = 0.4
  ) +
  geom_line(linewidth = 1) +
  facet_wrap(~ variable, scales = "free_x") +
  labs(x = NULL, y = "Predicted probability of conservation concern") +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.6),
    panel.spacing = unit(0.8, "lines"),
    strip.background = element_rect(fill = "grey90", colour = "black"),
    strip.text = element_text(size = 13),
    axis.title.y = element_text(size = 13),
    axis.text = element_text(size = 12)
  )

plot_national
plot_global

plot_combined <- plot_national / plot_global +
  plot_annotation(tag_levels = "A")
x11()
plot(plot_combined)

ggsave(
  filename = paste0(data_storage_path, "Output/range_threat/mammals_range_threat_combined.png"),
  plot = plot_combined,
  width = 8,
  height = 10,
  dpi = 300
)


ggsave(
  filename = paste0(data_storage_path, "Output/range_threat/mammals_range_threat_national.png"),
  plot = plot_national,
  width = 12,
  height = 9,
  dpi = 300
)

ggsave(
  filename = paste0(data_storage_path, "Output/range_threat/mammals_range_threat_global.png"),
  plot = plot_global,
  width = 12,
  height = 9,
  dpi = 300
)
