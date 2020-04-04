# this script exports the content from text file by providers

# replace the string with the path of the export file
setwd("~/Downloads/")

# install (if needed) or load libreries
if(!("tidyverse" %in% installed.packages()[,"Package"])) install.packages("tidyverse")
require("tidyverse")

# read file
f <- data.frame(line = read_lines("report-4.csv"))
f <- mutate(f, fields = str_count(line, ";"))

# indentify patients
f <- mutate(f, pt = fields == 0)

# name patients
f <- mutate(f, pt = cumsum(pt)) %>%
  filter(fields != 0)

# indentify headers
f <- mutate(f, header = row_number() %% 2 == 1)


# condensate info
f <- group_by(f, pt, header) %>%
  summarise(line = paste(line, collapse = ";"),
            fields = sum(fields))

f <- f %>%
  separate(line, as.character(1:250), sep = ";") %>% 
  gather("position", "value", as.character(1:100), convert = TRUE) %>%
  group_by(pt, position) %>% 
  summarise(value = paste(value, collapse = "!!!")) %>% 
  separate(value, c("value", "header"), sep = "!!!") %>% 
  filter(header != "NA", !is.na(header)) %>%
  arrange(pt, position)

f <- f %>% 
  group_by(pt) %>% 
  mutate(blank = ifelse(value == "" & header == "", 1, 0)) %>% 
  mutate(prev_blank = lag(blank)) %>% 
  mutate(title = lag(blank == 1 & prev_blank == 1)) %>% 
  mutate(title = ifelse(title, header, NA)) %>% 
  filter(!blank) %>% 
  fill(title) %>% 
  mutate(title = ifelse(is.na(title), "Base", title)) %>%
  select(-blank, -prev_blank) %>% 
  ungroup()

f <- f %>%
  unite("variable", title, header) %>% 
  group_by(pt, variable) %>%
  mutate(multiple = row_number()) %>% 
  ungroup() %>% 
  mutate(variable = ifelse(multiple > 1, paste(variable, multiple, sep = "_"), variable)) %>%
  select(-position, -multiple) %>% 
  spread(variable, value)










# separate tables
tables <- split(lines, lines$table)

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







