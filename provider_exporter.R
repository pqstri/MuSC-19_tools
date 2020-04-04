# this script exports the content from text file by providers

# replace the string with the path of the export file
setwd("~/path_to_export_file.csv")

# install (if needed) or load libreries
if(!("tidyverse" %in% installed.packages()[,"Package"])) install.packages("tidyverse")
require("tidyverse")

# read file
rows <- readr::read_csv("report.csv", skip = 4, col_names = "line")

# indentify tables
rows <- mutate(rows, sep_n = stringr::str_count(line, ";"))

# name tables
rows <- mutate(rows, table = ifelse(sep_n == 0, line, NA)) %>% tidyr::fill(table)
rows <- filter(rows, table != line) %>% select(-sep_n)

# separate tables
tables <- split(rows, rows$table)

# separate columns
tables <- purrr::map(tables, ~ tidyr::separate(., line, sep = ";",
  into = unlist(stringr::str_split(.$line[1], ";"))))

# remove fake header lines
tables <- purrr::map(tables, ~ filter(., row_number() > 1))

# create a new data for converted files
dir.create("tables", showWarnings = FALSE)

# save tables
purrr::map2(tables, names(tables), function(td, tn) {
  names(td) <- stringr::str_replace_all(names(td), ";|,|-| |\\(|\\)", "_") %>% 
    strtrim(40)
  tn <- stringr::str_replace_all(tn, ";|,|-| ", "_")
  fname <- paste("tables/", tn, ".sav", sep = "")
  haven::write_sav(td, fname)
  return("exported")
  })







