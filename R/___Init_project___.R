#----------------------------------------------------------#
#
#
#                     Project name
#

#
#----------------------------------------------------------#

# Script to prepare all necessary components of environment to run the Project.
#   Needs to be run only once


#----------------------------------------------------------#
# Step 1: Install 'renv' package -----
#----------------------------------------------------------#

utils::install.packages("renv")


#----------------------------------------------------------#
# Step 2: Deactivate 'renv' package -----
#----------------------------------------------------------#

# deactivate to make sure that packages are updated on the machine
renv::deactivate()


#----------------------------------------------------------#
# Step 3: Create a list of packages
#----------------------------------------------------------#

package_list <- 
  c("ggplot2",
    "leaflet",
    "here",
    "sf",
    "stringr",
    "tidyverse",
    "usethis",
    "htmltools",
    "raster",
    "fasterize",
    "GIFT",
    "remotes",
    "rgdal",
    "rgeos",
    "CoordinateCleaner",
    "tidyr",
    "elevatr",
    "purrr",
    "openxlsx",
    "data.table",
    "readxl",
    "stringr",
    "broom",
    "rnaturalearthdata",
    "rnaturalearth",
    "insight",
    "ggrepel",
    "mgcv",
    "performance",
    "patchwork",
    "biscale",
    "svglite",
    "gridExtra",
    "palettes",
    "writexl",
    "gt",
    "janitor",
    "plotly",
    "visdat"
  )

remotes::install_github(
  repo = "HOPE-UIB-BIO/R-Utilpol-package",
  ref = "HEAD",
  quiet = FALSE,
  upgrade = "ask"
)

#----------------------------------------------------------#
# Step 4: Install packages to the machine
#----------------------------------------------------------#

sapply(package_list, utils::install.packages, character.only = TRUE)

#----------------------------------------------------------#
# Step 5: Activate 'renv' project
#----------------------------------------------------------#

renv::activate()

#----------------------------------------------------------#
# Step 6: Install packages to the project
#----------------------------------------------------------#

package_list <- 
  c("devtools",
    "ggplot2",
    "leaflet",
    "here",
    "sf",
    "stringr",
    "tidyverse",
    "usethis",
    "htmltools",
    "raster",
    "fasterize",
    "GIFT",
    "remotes",
    "rgdal",
    "rgeos",
    "CoordinateCleaner",
    "tidyr",
    "elevatr",
    "purrr",
    "openxlsx",
    "data.table",
    "readxl",
    "stringr",
    "broom",
    "rnaturalearthdata",
    "rnaturalearth",
    "insight",
    "ggrepel",
    "mgcv",
    "performance",
    "patchwork",
    "biscale",
    "svglite",
    "gridExtra"
  )


sapply(package_list, utils::install.packages, character.only = TRUE)

remotes::install_github(
  repo = "HOPE-UIB-BIO/R-Utilpol-package",
  ref = "HEAD",
  quiet = FALSE,
  upgrade = "ask"
)

#----------------------------------------------------------#
# Step 7: Synchronize package versions with the project 
#----------------------------------------------------------#
renv::snapshot(lockfile = "renv/library_list.lock")

library(here)
renv::restore(lockfile = here::here( "renv/library_list.lock"))
renv::snapshot()

#----------------------------------------------------------#
# Step 8: GitHub hook
#----------------------------------------------------------#

# Prevent commiting to the Main
#usethis::use_git_hook(
  #hook = "pre-commit",
  #script = '#!/bin/sh
  #branch="$(git rev-parse --abbrev-ref HEAD)"
  #if [ "$branch" = "main" ]; then
  #echo "You cannot commit directly to main branch. Please make a new branch"
  #exit 1
  #fi'
#)
