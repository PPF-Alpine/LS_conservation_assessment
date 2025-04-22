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
luc_summary_ssp5_85 <- read_csv(file.path(data_storage_path, "Outputs/land_use_change/all_mountains_luc_summary_ssp5_85_20250421.csv")) |>
  mutate(scenario = "SSP5-8.5_2100")

luc_summary_ssp1_26 <- read_csv(file.path(data_storage_path, "Outputs/land_use_change/all_mountains_luc_summary_ssp1_26_20250421.csv")) |>
  mutate(scenario = "SSP1-2.6_2100")

luc_summary_current <- read_csv(file.path(data_storage_path, "Outputs/land_use_change/all_mountains_luc_summary_current_20250422.csv")) |>
  mutate(scenario = "baseline")

luc_combined <- bind_rows(luc_summary_current,luc_summary_ssp1_26, luc_summary_ssp5_85)

luc_combined <- luc_combined %>%
  mutate(layer = recode(layer,
                        "range_86" = "rangeland",
                        "urban_86" = "urban land",
                        "pastr_86"= "managed pastures",
                        "c3ann_86" = "C3 annual crops",
                        "c3per_86" = "C3 perennial crops",
                        "c4ann_86" = "C4 annual crops",
                        "c4per_86" = "C4 perennial crops",
                        "c3nfx_86" = "C3 nitrogen-fixing crops",
                        "range_1166" = "rangeland",
                        "urban_1166" = "urban land",
                        "pastr_1166"= "managed pastures",
                        "c3ann_1166" = "C3 annual crops",
                        "c3per_1166" = "C3 perennial crops",
                        "c4ann_1166" = "C4 annual crops",
                        "c4per_1166" = "C4 perennial crops",
                        "c3nfx_1166" = "C3 nitrogen-fixing crops"))
#----------------------------------------------------------#
#       Plot the LUC
#----------------------------------------------------------#

plot <- ggplot(luc_combined, aes(x = layer, y = mean_luc, fill = scenario)) +
  geom_col(position = position_dodge(width = 0.6)) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_fill_viridis_d(option = "viridis") +
  labs(
    x = NULL,
    y = "mean land use fraction",
    fill = "Scenario"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold")
  )

#----------------------------------------------------------#
#   save the plot
#----------------------------------------------------------#
today <- format(Sys.Date(), "%Y%m%d")

# Define file path for PNG
plot_path <- paste0(data_storage_path, "Outputs/Figures/land_use_change/all_mountains_LUC_", today, ".jpeg")

# Save last plot as PNG
ggsave(filename = plot_path, plot=plot,width = 10, height = 6, dpi = 300)

