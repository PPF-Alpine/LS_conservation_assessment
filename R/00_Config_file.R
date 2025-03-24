#----------------------------------------------------------#
#
#
#                       Project name
#
#                       Config file
#                 
#
#                         Names 
#                         Year
#
#----------------------------------------------------------#
# Configuration script with the variables that should be consistent throughout 
#   the whole repo. It loads packages, defines important variables, 
#   authorises the user, and saves config file.


#----------------------------------------------------------#
# 1. Load packages -----
#----------------------------------------------------------#

# define packages
package_list <- 
  c(
    "devtools",
    "here",      
    "renv",       
    "tidyverse",  
    "usethis",
    "ggplot2",
    "purrr"
  )

# load all packages
sapply(package_list, library, character.only = TRUE)


#----------------------------------------------------------#
# 2. Define space -----
#----------------------------------------------------------#

current_date <- Sys.Date()

# project directory is set up by 'here' package, Adjust if needed 
current_dir <- here::here()


#----------------------------------------------------------#
# 3. Load functions -----
#----------------------------------------------------------#

# get vector of general functions
fun_list <- 
  list.files(
    path = "R/Functions/",
    pattern = "*.R",
    recursive = TRUE) 

# source them
sapply(
  paste0("R/functions/", fun_list, sep = ""),
  source)


#----------------------------------------------------------#
# 4. Authorise the user -----
#----------------------------------------------------------#

# specify here the data storage path

auth_tibble <-
  tibble(
    name = c("losch5089","lotta"),
    paths = c(
      "C:/Users/losch5089/OneDrive - University of Bergen/Desktop/Manuscripts/Ch_2_Conservation_assessment_mountains/",
      "C:/Users/lotta/OneDrive - University of Bergen/Desktop/Manuscripts/Ch_2_Conservation_assessment_mountains/"
    )
  )

sys_info <- Sys.info()

username <- 
  sys_info["user"]

data_storage_path <-
  auth_tibble |> 
  dplyr::filter(name == username) |> 
  dplyr::select(paths) |> 
  pluck(1)


#----------------------------------------------------------#
# 7. Save current config setting -----
#----------------------------------------------------------#

current_setting <- .extract.config.data()

