#----------------------------------------------------------#
# load red list assesments -----
#----------------------------------------------------------#

library(tidyverse)
library(ggplot2)
library(patchwork)

# load RL and HKH list
assessments_comp <- read.csv(paste0(data_storage_path,"Datasets/species_list/national_global_threat_comparison.csv"))

# load RL and HKH list
full_assessment_hkh_mammals <- readxl::read_excel(paste0(data_storage_path,"Datasets/species_list/assessment_hkh_mammals_09082025_LS.xlsx"))

full_ass_work_data <- full_assessment_hkh_mammals|>
  filter(str_squish(countries_iso) != "Nepal, India, Pakistan, Myanmar, Viet Nam, Thailand, Indonesia, Philippines (the), Singapore")|>
  select(sciname,status_code_national,countries_iso,status_summary_national,year_assessed_national, year_assessed_global,status_code_global)|>
  mutate(across(everything(), ~na_if(.x, "NA")))

#----------------------------------------------------------#
# # Panel A: assessment coverage -----
#----------------------------------------------------------#

rl_full <- full_ass_work_data |>
  mutate(
    global_assessed   = !is.na(status_code_global),
    national_assessed = !is.na(status_code_national),
    
    assessment_status = case_when(
      global_assessed & national_assessed ~ "Global + National",
      global_assessed & !national_assessed ~ "Global only",
      !global_assessed & national_assessed ~ "National only",
      !global_assessed & !national_assessed ~ "Not assessed"
    )
  )

rl_full_species <- rl_full |>
  group_by(sciname) |>
  summarise(
    global_assessed   = any(global_assessed),
    national_assessed = any(national_assessed),
    assessment_status = case_when(
      global_assessed & national_assessed ~ "Global + National",
      global_assessed & !national_assessed ~ "Global only",
      !global_assessed & national_assessed ~ "National only",
      TRUE ~ "Not assessed"
    ),
    .groups = "drop"
  )
table(rl_full_species$assessment_status)
prop.table(table(rl_full_species$assessment_status))


# 
rl_full_species$assessment_status <- factor(
  rl_full_species$assessment_status,
  levels = c("Global + National", "Global only", "National only", "Not assessed")
)

ggplot(rl_full_species, aes(x = assessment_status)) +
  geom_bar() +
  labs(x = NULL, y = "Number of species") +
  theme_bw()



# --- Panel B: comparison ---
plot_df <- assessments_comp |>
  filter(countries_iso != "NA") |>   # keep real countries
  group_by(countries_iso, rl_change) |>
  summarise(n = n(), .groups = "drop")

# add proportions
plot_df <- plot_df |>
  group_by(countries_iso) |>
  mutate(prop = n / sum(n)) |>
  ungroup()

# as factors
plot_df$rl_change <- factor(
  plot_df$rl_change,
  levels = c(
    "Upgraded nationally",
    "Same category",
    "Downgraded nationally",
    "Data deficient nationally",
    "Data deficient globally",
    "Data deficient both",
    "Not comparable / not assessed"
  )
)

# plot rl change all 
ggplot(plot_df, aes(x = countries_iso, y = prop, fill = rl_change)) +
  geom_col(color = "black", width = 0.8) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Country",
    y = "Proportion of species",
    fill = "National vs global status"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank()
  )

# --- Panel B: upgrade/downgrade (paired only) ---


# --- Panel C: by country (optional) ---

# --- Combine (2 panels or 3 panels) ---
(pA | pB) / pC   # or: pA | pB   (if you want just two panels)




# Alluvial plot for species changes 

