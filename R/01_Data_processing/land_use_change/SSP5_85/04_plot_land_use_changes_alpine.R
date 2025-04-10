#----------------------------------------------------------#
# 1. Set up  -----
#----------------------------------------------------------#
library(here)
library(rredlist)
library(stringr)
library(scales)
library(purrr)
library(patchwork)

# Load configuration file
source(here::here("R/00_Config_file.R"))


#----------------------------------------------------------#
#        read in the LUC summaries 
#----------------------------------------------------------#

# Load both datasets
luc_summary_ssp5_85 <- read_csv(file.path(data_storage_path, "Outputs/land_use_change/luc_summary_ssp5_85_20250410.csv")) |>
  mutate(scenario = "SSP5-8.5_2100")

luc_summary_ssp1_26 <- read_csv(file.path(data_storage_path, "Outputs/land_use_change/luc_summary_ssp1_26_20250410.csv")) |>
  mutate(scenario = "SSP1-2.6_2100")


luc_combined <- bind_rows(luc_summary_ssp1_26, luc_summary_ssp5_85)

#----------------------------------------------------------#
#       Plot the LUC
#----------------------------------------------------------#
ggplot(luc_combined, aes(x = layer, y = mean_luc, fill = scenario)) +
  geom_col(position = position_dodge(width = 0.7)) +
  facet_wrap(~ Mntn_rn, scales = "free_x") +
  scale_y_continuous(limits = c(0, 1)) +  # fixes y-axis from 0 to 1
  labs(
    title = "Land Use Change Comparison by Scenario",
    x = "Land Use Change Category",
    y = "Mean Land Use Change",
    fill = "Scenario"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold")
  )


### ❗ add the current LUC (2015) here
