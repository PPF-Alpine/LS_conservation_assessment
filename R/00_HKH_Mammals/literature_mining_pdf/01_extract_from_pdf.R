

# this code works for the red list of Pakistan 

# you need to adapt this dependent on the content of your pdf

#----------------------------------------------------------#
# set up -----
#----------------------------------------------------------#

install.packages("pdftools")
library(pdftools)
library(pdftools)
library(dplyr)
library(stringr)
library(tibble)

# load RL and HKH list
file <- file.path(data_storage_path, "RL_assessments/national/summary_2003_CAMP_Pakistan_Mammals.pdf")

# Load the text from the PDF
text <- pdf_text(file)

#----------------------------------------------------------#
# read as string and split text into lines -----
#----------------------------------------------------------#

# Combine all pages into a single string
full_text <- paste(text, collapse = "\n")

# Split into lines
lines <- str_split(full_text, "\n")[[1]] %>%
  str_trim() %>%
  discard(~ .x == "")

#----------------------------------------------------------#
# restructure text -----
#----------------------------------------------------------#
# Join lines that are split over multiple rows
# This step joins lines where the Red List status appears on the next line
joined_lines <- lines %>%
  str_replace_all("\\s+", " ") %>%
  reduce(function(acc, line) {
    if (str_detect(line, "\\b(LC|NT|VU|EN|CR|DD|NE|RE)\\b")) {
      c(acc, line)
    } else {
      # Append line to the last element
      acc[length(acc)] <- paste(acc[length(acc)], line)
      acc
    }
  }, .init = character(0))

#----------------------------------------------------------#
# parse lines into a dataframe -----
#----------------------------------------------------------#

# Red List codes
status_codes <- c("LC", "NT", "VU", "EN", "CR", "DD", "NE", "RE")
status_pattern <- str_c("\\b(", str_c(status_codes, collapse = "|"), ")\\b")

# Parse lines
parsed <- tibble(raw = joined_lines) %>%
  mutate(
    number = str_extract(raw, "^\\d+"),  # Handles 1, 10, 100, etc.
    text = str_remove(raw, "^\\d+\\.?\\s*"),  # Remove number and dot
    genus = word(text, 1),
    species = word(text, 2),
    status_code = str_extract(text, status_pattern),
    additional_info = str_remove(text, paste0("^", genus, "\\s+", species, ".*?\\b", status_code, "\\b\\s*"))
  ) %>%
  select(number, genus, species, status_code, additional_info)


parsed$number <- as.numeric(parsed$number)


final_table <- parsed|>
  mutate(sciname = paste(genus, species, sep = " "))|>
  select(-number)


#----------------------------------------------------------#
#   save
#----------------------------------------------------------#

write.csv(final_table,paste0(data_storage_path,"RL_assessments/national/pakistan_national_rl.csv"))



