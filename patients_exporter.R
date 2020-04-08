# this script exports the content from text file by providers
exported_file_folder <- ""
imported_file_path <- ""

# replace the string with the path of the export file
setwd(exported_file_folder)

# install (if needed) or load libreries
if(!("tidyverse" %in% installed.packages()[,"Package"])) install.packages("tidyverse")
require("tidyverse")

headers <- c("Provider;", ";;Demography", ";;MS history", ";;COVID 19 - ", 
             ";;COVID19 - ", ";;Comorbidity", ";;Surgery", ";;Comment")

# read file
f <- data.frame(line = readr::read_lines(imported_file_path))
f <- mutate(f, fields = stringr::str_count(line, ";"))

# indentify patients
f <- mutate(f, pt = fields == 0)

# name patients
f <- mutate(f, pt = cumsum(pt)) %>%
  filter(fields != 0)

# indentify headers
f <- mutate(f, is_header = map_lgl(line, ~ any(str_detect(., headers))))

# identify multiple objects
f <- f %>% group_by(pt) %>% mutate(block = cumsum(is_header)) %>% 
  group_by(pt, block) %>% mutate(sheets = n()) %>% 
  mutate(multiplier = ifelse(is_header & sheets > 2, sheets-1, 1)) %>% 
  uncount(multiplier) %>% 
  ungroup()

# condensate info
f <- group_by(f, pt, is_header) %>%
  summarise(line = paste(line, collapse = ";"),
            fields = sum(fields))

# integrity check
all(f %>% group_by(pt) %>% summarise(ok = all(fields == fields[1])) %>% .$ok)

# fix the ";" bug
reimport <- function(line) {
  read.csv2(text = line, header = FALSE) %>% 
    gather("k", "v") %>% 
    mutate(v = ifelse(is.na(v), "", v)) %>% 
    mutate(v = str_replace_all(v, ";", ",")) %>% 
    {paste(.$v, collapse = ";")}
}

f <- f %>%
  rowwise() %>% 
  mutate(line = ifelse(grepl("\"", line), reimport(line), line)) %>% 
  mutate(fields = str_count(line, ";")) %>% 
  ungroup()

# integrity check
all(f %>% group_by(pt) %>% summarise(ok = all(fields == fields[1])) %>% .$ok)


# tokenization and verticalization
max_length <- 300
f <- f %>%
  mutate(ambiguous_separator = ifelse(grepl("\".*\"", line), 0, 1)) %>% 
  separate(line, as.character(1:max_length), sep = ";") %>% 
  tidyr::gather("position", "value", as.character(1:max_length), convert = TRUE) %>%
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

# patient ID
f <- mutate(f, upid = sprintf("%s-%s-%s", Demography_Country, 
                              `Demography_Site Code`, `Demography_Patient Code`))

# dummification
# COVID 19 - Sign and symptom_Other symptoms
f <- separate(f, `COVID 19 - Sign and symptom_Other symptoms`,
         into = as.character(1:10), sep = ",") %>% 
  select(upid, as.character(1:10)) %>% 
  gather("position", "symptom", -upid) %>% 
  filter(!is.na(symptom) & symptom != "") %>% 
  mutate(symptom = trimws(symptom)) %>% 
  mutate(symptom = paste("COVID 19 - Sign and symptom_Other symptoms", symptom, sep = "_")) %>% 
  mutate(position = 1) %>% 
  spread(symptom, position) %>% 
  right_join(f) %>% 
  mutate_at(vars(contains("COVID 19 - Sign and symptom_Other symptoms")), ~ replace_na(., 0))

# COVID 19 - Sign and symptom_Signs of infection
f <- separate(f, `COVID 19 - Sign and symptom_Signs of infection`,
              into = as.character(1:10), sep = ",") %>% 
  select(upid, as.character(1:10)) %>% 
  gather("position", "symptom", -upid) %>% 
  filter(!is.na(symptom) & symptom != "") %>% 
  mutate(symptom = trimws(symptom)) %>% 
  mutate(symptom = paste("COVID 19 - Sign and symptom_Signs of infection", symptom, sep = "_")) %>% 
  mutate(position = 1) %>% 
  spread(symptom, position) %>% 
  right_join(f) %>% 
  mutate_at(vars(contains("COVID 19 - Sign and symptom_Signs of infection")), ~ replace_na(., 0))

# COVID19 - Diagnosis, Treatment_Treatments
f <- separate(f, `COVID19 - Diagnosis, Treatment_Treatments`,
              into = as.character(1:10), sep = ",") %>% 
  select(upid, as.character(1:10)) %>% 
  gather("position", "choice", -upid) %>% 
  filter(!is.na(choice) & choice != "") %>% 
  mutate(choice = trimws(choice)) %>% 
  mutate(choice = paste("COVID19 - Diagnosis, Treatment_Treatments", choice, sep = "_")) %>% 
  mutate(position = 1) %>% 
  spread(choice, position) %>% 
  right_join(f) %>% 
  mutate_at(vars(contains("COVID19 - Diagnosis, Treatment_Treatments")), ~ replace_na(., 0))


# clean name for spss
# names(f) <- stringr::str_replace_all(names(f), ";|,|-| |\\(|\\)|\\.|\\\\|\\/|\\+", "_") %>% 
#   strtrim(40) %>% make.unique("_")
# f <- mutate_all(f, ~ stringr::str_replace_all(., ";|,|-| |:|/", "_"))


# save spss output
fname <- paste("musc19", format(Sys.time(), "_%d%b%Y"), ".sav", sep = "")
selected_col <- unname(MPS_format$var_dictionary) %>% na.omit()
selected_col <- intersect(selected_col, names(f))
col_index <- which(MPS_format$var_dictionary %in% names(f))
export <- select(f, selected_col) %>% 
  rename(MPS_format$var_dictionary[col_index])
haven::write_sav(export, fname)








