#----------------------------------------------------------#
# SETUP ----
#----------------------------------------------------------#

# Load required libraries
library(pdftools)
library(dplyr)
library(stringr)
library(tibble)
library(purrr)
library(tidyverse)
library(here)

# Load configuration
source(here::here("R/00_Config_file.R"))


#---- SCHEDULE 1

# load pdf
file <- "C:/Users/lotta/OneDrive - University of Bergen/Desktop/ICIMOD_work/RL_assessments/national/INDIA/INDIA_schedule_1_mammals-ocr.pdf"

# 
text<-pdftools::pdf_text(file)

# Combine all pages into a single string
full_text <- paste(text, collapse = "\n")

# Split into lines
lines <- str_split(full_text, "\n")[[1]] %>%
  str_trim() %>%
  discard(~ .x == "")


joined_lines <- lines %>%
  str_replace_all("\\s+", " ") 


# Step 1: Keep only relevant lines (starting from line 7)
relevant_lines <- joined_lines[7:length(joined_lines)]

# Step 2: Initialize variables
group <- NULL
result <- list()

# Step 3: Process each line
for (line in relevant_lines) {
  # Detect group header (all caps, no number at start)
  if (!str_detect(line, "^\\d")) {
    group <- line
  } else {
    # Extract number
    number <- str_extract(line, "^\\d+")
    
    # Remove number
    line_wo_number <- str_remove(line, "^\\d+\\.?\\s*")
    
    # Extract scientific name (last two words)
    scientific_name <- str_extract(line_wo_number, "\\b\\w+\\s\\w+$")
    
    # Extract common name (everything before scientific name)
    common_name <- str_remove(line_wo_number, "\\b\\w+\\s\\w+$") %>% str_trim()
    
    # Append to result
    result[[length(result) + 1]] <- tibble(
      number = as.integer(number),
      common_name = common_name,
      scientific_name = scientific_name,
      group = group
    )
  }
}

# Step 4: Combine into a single data frame
final_table <- bind_rows(result)|>
  mutate(schedule = "schedule_1")



################################# SCHEDULE 2 ##################

# load pdf
file_2 <- "C:/Users/lotta/OneDrive - University of Bergen/Desktop/ICIMOD_work/RL_assessments/national/INDIA/INDIA_schedule_2_mammals-ocr.pdf"

# 
text_2<-pdftools::pdf_text(file_2)

# Combine all pages into a single string
full_text_2 <- paste(text_2, collapse = "\n")

# Split into lines
lines_2 <- str_split(full_text_2, "\n")[[1]] %>%
  str_trim() %>%
  discard(~ .x == "")


joined_lines_2 <- lines_2 %>%
  str_replace_all("\\s+", " ") 


# Step 1: Keep only relevant lines (starting from line 7)
relevant_lines_2 <- joined_lines_2[7:length(joined_lines_2)]

# Step 2: Initialize variables
group <- NULL
result <- list()

# Step 3: Process each line
for (line in relevant_lines_2) {
  # Detect group header (all caps, no number at start)
  if (!str_detect(line, "^\\d")) {
    group <- line
  } else {
    # Extract number
    number <- str_extract(line, "^\\d+")
    
    # Remove number
    line_wo_number <- str_remove(line, "^\\d+\\.?\\s*")
    
    # Extract scientific name (last two words)
    scientific_name <- str_extract(line_wo_number, "\\b\\w+\\s\\w+$")
    
    # Extract common name (everything before scientific name)
    common_name <- str_remove(line_wo_number, "\\b\\w+\\s\\w+$") %>% str_trim()
    
    # Append to result
    result[[length(result) + 1]] <- tibble(
      number = as.integer(number),
      common_name = common_name,
      scientific_name = scientific_name,
      group = group
    )
  }
}

# Step 4: Combine into a single data frame
final_table_2 <- bind_rows(result)|>
  mutate(schedule = "schedule_2")



########## combine 

full_table <- bind_rows(final_table,final_table_2)


#----------------------------------------------------------#
#   save
#----------------------------------------------------------#

writexl::write_xlsx(
  full_table,
  path = paste0(data_storage_path, "RL_assessments/national/India_wildlife_schedules.xlsx")
)