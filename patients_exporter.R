# this script exports the content from text file by providers
exported_file_folder <- "~/Downloads/"
imported_file_path <- "~/Downloads/report-4.csv"

# replace the string with the path of the export file
setwd(exported_file_folder)

# install (if needed) or load libreries
if(!("tidyverse" %in% installed.packages()[,"Package"])) install.packages("tidyverse")
require("tidyverse")

# read file
f <- data.frame(line = readr::read_lines(imported_file_path))
f <- mutate(f, fields = stringr::str_count(line, ";"))

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

# tokenization and verticalization
f <- f %>%
  separate(line, as.character(1:250), sep = ";") %>% 
  tidyr::gather("position", "value", as.character(1:100), convert = TRUE) %>%
  group_by(pt, position) %>% 
  summarise(value = paste(value, collapse = "!!!")) %>% 
  tidyr::separate(value, c("value", "header"), sep = "!!!") %>% 
  filter(header != "NA", !is.na(header)) %>%
  arrange(pt, position)

# include topic in variable name
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

# avoid duplicates and horizontalize
f <- f %>%
  tidyr::unite("variable", title, header) %>% 
  group_by(pt, variable) %>%
  mutate(multiple = row_number()) %>% 
  ungroup() %>% 
  mutate(variable = ifelse(multiple > 1, paste(variable, multiple, sep = "_"), variable)) %>%
  select(-position, -multiple) %>% 
  tidyr::spread(variable, value)

# clean name for spss
names(f) <- stringr::str_replace_all(names(f), ";|,|-| |\\(|\\)|\\.|\\\\|\\/|\\+", "_") %>% 
  strtrim(40) %>% make.unique("_")
# f <- mutate_all(f, ~ stringr::str_replace_all(., ";|,|-| |:|/", "_"))


# save spss output in wd
fname <- paste("musc19", format(Sys.time(), "_%d%b%Y"), ".sav", sep = "")
haven::write_sav(f, fname)








