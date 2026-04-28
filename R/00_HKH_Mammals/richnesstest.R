library(rvest)
library(dplyr)
library(writexl)


# write to Excel
writexl::write_xlsx(df, "output.xlsx")

html <- read_html("~/Desktop/Manuscripts/Ch_1_Global_Alpine_Biodiversity/suppl_tables/verts_richness_table.html")
# extract tables
tables <- html_table(html)

# take first table (or choose one)
df <- tables[[1]]


writexl::write_xlsx(df,"~/Desktop/Manuscripts/Ch_1_Global_Alpine_Biodiversity/suppl_tables/verts_richness_table.xlsx")
