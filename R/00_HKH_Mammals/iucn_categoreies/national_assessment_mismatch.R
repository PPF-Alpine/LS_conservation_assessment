# Load configuration file
source(here::here("R/00_Config_file_HKH.R"))
#----------------------------------------------------------#
# load red list assesments -----
#----------------------------------------------------------#

library(tidyverse)
library(ggplot2)
library(ggalluvial)

# load RL and HKH list
assessments_comp <- read.csv(paste0(data_storage_path,"Datasets/species_list/national_global_threat_comparison.csv"))

# load RL and HKH list
full_assessment_hkh_mammals <- readxl::read_excel(paste0(data_storage_path,"Datasets/species_list/assessment_hkh_mammals_09082025_LS.xlsx"))

full_ass_work_data <- full_assessment_hkh_mammals|>
  filter(str_squish(countries_iso) != "Nepal, India, Pakistan, Myanmar, Viet Nam, Thailand, Indonesia, Philippines (the), Singapore")|>
  select(sciname,status_code_national,countries_iso,status_summary_national,year_assessed_national, year_assessed_global,status_code_global)|>
  mutate(across(everything(), ~na_if(.x, "NA")))


library(dplyr)
library(ggplot2)
library(ggalluvial)

library(dplyr)
library(stringr)
#----------------------------------------------------------#
# global vs national mismatch -----
#----------------------------------------------------------#

rl_order <- c("LC","NT","VU","EN","CR","DD","RE")

allu_rows <- full_ass_work_data |>
  mutate(
    status_code_global   = str_squish(status_code_global),
    status_code_national = str_squish(status_code_national),
    
    status_code_global   = na_if(status_code_global, "NA"),
    status_code_national = na_if(status_code_national, "NA"),
    
    # if you might have blanks too:
    status_code_global   = na_if(status_code_global, ""),
    status_code_national = na_if(status_code_national, ""),
    
    global_cat   = status_code_global,
    national_cat = status_code_national,
    
    flow_id = interaction(sciname, countries_iso, drop = TRUE)
  ) |>
  filter(!is.na(flow_id)) |>
  mutate(
    global_cat   = factor(global_cat,   levels = rl_order),
    national_cat = factor(national_cat, levels = rl_order)
  ) |>
  filter(!is.na(global_cat), !is.na(national_cat)) |>   # <-- removes any leftover NA strata
  select(flow_id, global_cat, national_cat)


allu_lodes <- ggalluvial::to_lodes_form(
  allu_rows,
  axes = c("global_cat", "national_cat"),
  id   = "flow_id"
) |>
  mutate(y = 1)


# Plot
ggplot(allu_lodes,
       aes(x = x, stratum = stratum, alluvium = flow_id, y = y)) +
  geom_flow(aes(fill = stratum), width = 0.12, alpha = 0.85) +
  geom_stratum(width = 0.12, color = "grey30", fill = "grey95") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3) +
  scale_fill_viridis_d(option = "inferno",
                       direction = -1,
                       end = 0.95) +
  scale_x_discrete(labels = c("global_cat" = "Global IUCN",
                              "national_cat" = "National Red List")) +
  labs(x = NULL,
       y = NULL,
       fill = "Red List category") +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )


library(dplyr)
library(stringr)
library(ggplot2)

rl_order <- c("LC","NT","VU","EN","CR","DD","RE")

dat_nat <- full_ass_work_data |>
  mutate(
    countries_iso = str_squish(countries_iso),
    status_code_national = str_squish(status_code_national),
    status_code_national = na_if(status_code_national, "NA"),
    status_code_national = na_if(status_code_national, "")
  ) |>
  filter(!is.na(countries_iso)) |>
  mutate(
    national_cat = factor(status_code_national, levels = rl_order)
  )


p_inconsistency <- dat_nat |>
  filter(!is.na(national_cat)) |>
  group_by(sciname) |>
  summarise(
    n_countries  = n_distinct(countries_iso),
    n_categories = n_distinct(national_cat),
    .groups = "drop"
  )

max_n <- max(p_inconsistency$n_categories)

ggplot(p_inconsistency, aes(x = n_categories)) +
  geom_bar(width = 0.8) +
  scale_x_continuous(
    breaks = seq_len(max_n)
  ) +
  labs(
    x = "Number of distinct national Red List categories per species",
    y = "Number of species"
  ) +
  theme_bw() +
  theme(panel.border = element_blank())


