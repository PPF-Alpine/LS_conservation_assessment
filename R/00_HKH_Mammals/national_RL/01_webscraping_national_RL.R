

# Load configuration file
source(here::here("R/00_Config_file.R"))# in this code only needed for datastorage path to save the final file

# install.packages("rvest")
library(rvest)
library(httr2)
library(tidyverse)
library(glue)

#----------------------------------------------------------#
# Webscrape table from nationalredlist.org ---
#----------------------------------------------------------#

# Note: this code is used to scrape ALL national assessments available from the website. 
# there are NO filters applied for countries, class, etc. 
# code takes around 20 min to run

# set base URL 
base_url <- "https://www.nationalredlist.org/assessments?search_api_fulltext=&field_year_assessed=&field_sco=&field_phylum=&field_class=&field_order=&field_family=&field_genus=&page="

# set a limit for how many pages to scrape (there is 931 pages in total without any filters applied)
# pls test first with a lower number e.g., 5
limit <- 931 

# Store tables
all_tables <- list()

# loop over pages to scrape
for (i in 0:(limit - 1)) {
  url <- glue("{base_url}{i}")
  cat("Scraping page:", i + 1, "\n")
  
  page <- read_html(url)
  table <- page |>
    html_element("table") |>
    html_table(fill = TRUE)
  
  if (!is.null(table)) {
    # Clean column names
    names(table) <- trimws(gsub("\n.*", "", names(table)))
    all_tables[[i + 1]] <- table
  } else {
    message("No table found on page ", i)
  }
  
  Sys.sleep(1)
}

# Combine everything into df 
final_df <- bind_rows(all_tables)

#----------------------------------------------------------#
# save the data ---
#----------------------------------------------------------#

# write data to your folder 

write.csv(final_df,paste0(data_storage_path,"RL_assessments/national_assessment_webscraping_output.csv"))