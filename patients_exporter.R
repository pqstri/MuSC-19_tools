#############################
# Set up
#############################
# this script exports the content from text file by providers
# exported_file_folder <- ""
imported_file_path <- ""

# replace the string with the path of the export file
# setwd(exported_file_folder)

# install (if needed) or load libreries
# if(!("tidyverse" %in% installed.packages()[,"Package"])) install.packages("tidyverse")
library("tidyverse")

# First run all the script then run the following commented line


#############################
# Format
#############################

headers <- c("Provider;", ";;Demography", ";;MS history", ";;COVID 19 - ", 
             ";;COVID19 - ", ";;Comorbidity", ";;Surgery;", ";;Comment", ";;Serology")

repeated_ev <- c(";;COVID 19 - Follow-up", ";;COVID 19 - Detailed treatment",
             ";;COVID19 - Complication", ";;Comment")

section_constrains <- c(Base = 1, 
                     Demography = 1, 
                     Comorbidity = 1, 
                     `COVID 19 - Laboratory data` = 1,
                     `COVID 19 - Radiological data` = 1,
                     `COVID 19 - Sign and symptom` = 1,
                     `COVID19 - Complication` = 1,
                     `COVID19 - Diagnosis, Treatment` = 1,
                     `MS history` = 1,
                     Surgery = 1)

section_size <- list(Base = function(x) all(x == 5, na.rm = TRUE), 
                     Comment = function(x) all(x %% 4 == 0, na.rm = TRUE), 
                     Comorbidity = function(x) all(x == 39, na.rm = TRUE), 
                     `COVID 19 - Detailed treatment` = function(x) all(x %% 10 == 0, na.rm = TRUE),
                     `COVID 19 - Follow-up` = function(x) all(x %% 20 == 0, na.rm = TRUE), 
                     `COVID 19 - Laboratory data` = function(x) all(x == 17, na.rm = TRUE),
                     `COVID 19 - Radiological data` = function(x) all(x == 19, na.rm = TRUE),
                     `COVID 19 - Sign and symptom` = function(x) all(x == 14, na.rm = TRUE),
                     `COVID19 - Complication` = function(x) all(x == 7, na.rm = TRUE),
                     `COVID19 - Diagnosis, Treatment` = function(x) all(x == 12, na.rm = TRUE),
                     Demography = function(x) all(x == 25, na.rm = TRUE), 
                     `MS history` = function(x) all(x == 25, na.rm = TRUE), 
                     `Serology blood test` = function(x) all(x == 6, na.rm = TRUE),
                     Surgery = function(x) all(x == 11, na.rm = TRUE))

new_patient_tag <- "Provider;Patient;Sex;Created at;Updated at"

#####
MPS_format <- list(
  # dummy variables
  var_dummy = c("COVID 19 - Sign and symptom_Other symptoms",
                "COVID 19 - Sign and symptom_If other symptoms, please specify",
                "COVID 19 - Laboratory data_WHITE BLOOD CELL count",
                "COVID 19 - Laboratory data_RED BLOOD CELL count",
                "COVID 19 - Laboratory data_PLATELET count",
                "COVID 19 - Laboratory data_LYMPHOCYTE count",
                "COVID 19 - Laboratory data_HEMOGLOBIN",
                "COVID 19 - Laboratory data_PaO2/FiO2"),
  
  staff = c("Francesca", "Nicolo’ Di Tullio",
            "Marta Ponzano", "Irene Schiavetti"),
  
  provider_blacklist = c("Francesca", "Fake", "Nicolo’ Di Tullio",
                         #"Marta Ponzano", 
                         "temp", "Irene Schiavetti"),
  
  # format converter
  var_dictionary = c(
    "PAT_ID"                 = "upid",
    "SITE"                   = "Demography_Site Code",
    "COUNTRY"                = "Demography_Country",
    "CONTACT_TYPE"           = "Demography_Type of contact",     
    "DOV"                    = "Demography_Date of Visit",
    "DIC"                    = "Demography_Date of Written Informed Consent",
    "SEX"                    = "Base_Sex",
    "AGE"                    = "Demography_Age",
    "HEIGHT"                 = "Demography_Height",
    "WEIGHT"                 = "Demography_Weight",
    "ETHN"                   = "Demography_Ethnicity",
    "ETHN_OTH"               = "Demography_If Ethnicity is Other, please specify", 
    "PREGN"                  = "Demography_Pregnancy",
    "EMPL"                   = "Demography_Employment",
    "EMPL_OTH"               = "Demography_If Employment is Other, please specify", 
    "COHAB"                  = "Demography_Number of Cohabitants",
    "COHAB_CHILD"            = "Demography_How many of these are school-age children?",    
    "COVID_COHAB"            = "Demography_Number of COVID+ Cohabitants",    
    "HEALTHCARE_JOB"         = "Demography_Work in the Healthcare Sector",       
    "SMOKE"                  = "Demography_Smoker History",
    "SMOKE_TYPE"             = "Demography_If Smoker History is Current Smoker, please specify",   
    "ALCOHOL"                = "Demography_Alcohol",
    "ABUSE"                  = "Demography_Any history of alcohol/drug abuse within the last year?",
    "MS_TYPE"                = "MS history_MS Type",
    "DIAGN_DATE"             = "MS history_Date of MS Diagnosis",   
    "LAST_EDSS"              = "MS history_Last available EDSS",  
    "EDSS_DATE"              = "MS history_Date of EDSS",  
    "TREAT"                  = "MS history_In Treatment",
    "LAST_DMD"               = "MS history_If in treatment, Type of DMD", 
    "NAME_DMD"               = "MS history_If in treatment, Name of DMD", 
    "DMD_OTH"                = "MS history_If DMD is Other, please specify",
    "START_DMD"              = "MS history_Start Date of Last DMD",  
    "STOP_DMD"               = "MS history_Stop Date of Last DMD", 
    "ONG_DMD"                = "MS history_Ongoing",
    "DMD_INTERR"             = "MS history_If Stopped, Reason for DMD Interruption",   
    "DMD_INTER_OTH"          = "MS history_If Stop Reason is Other, please specify",      
    "NAIVE"                  = "naive",
    "PREVIOUS_TREAT"         = "MS history_If Yes, report the name of the last previous DMD",
    "METH_GLUC"              = "MS history_Any Cycle of Methylprednisolone or Other Glucocorticoid",  
    "METGL_START"            = "MS history_If yes, please Specify Start Date",    
    "METGL_STOP"             = "MS history_If yes, please Specify Stop Date",   
    "WBC_DATE"               = "MS history_White Blood Cell Count Assessment Date", 
    "WBC_RESULT"             = "MS history_White Blood Cell Count",   
    "WBC_VALUE"              = "MS history_If WBC Abnormal, please specify value",  
    "LYMPH_DATE"             = "MS history_Lymphocyte Count Assessment Date",   
    "LYMPH_RESULT"           = "MS history_Lymphocyte Count",     
    "LYMPH_VALUE"            = "MS history_If Lymphocyte Abnormal, please specify value",    
    "SYMPTOMS_DATE"          = "COVID 19 - Sign and symptom_Date of first symptoms",      
    "FEVER"                  = "COVID 19 - Sign and symptom_Fever at admission",
    "FEVER_ONSET"            = "COVID 19 - Sign and symptom_Date of fever",    
    "FEVER_VALUE"            = "COVID 19 - Sign and symptom_Temperature (°C)",    
    "COUGH"                  = "COVID 19 - Sign and symptom_Other symptoms_Cough", # dummy
    "FATIGUE"                = "COVID 19 - Sign and symptom_Other symptoms_Fatigue", # dummy
    "SPUTUM_PROD"            = "COVID 19 - Sign and symptom_Other symptoms_Sputum production", # dummy
    "SORE_THR"               = "COVID 19 - Sign and symptom_Other symptoms_Sore throat", # dummy
    "HEADACHE"               = "COVID 19 - Sign and symptom_Other symptoms_Headache", # dummy
    "BONE_JOINT"             = "COVID 19 - Sign and symptom_Other symptoms_Bone or joint pain", # dummy 
    "SHORT_BR"               = "COVID 19 - Sign and symptom_Other symptoms_Shortness of breath", # dummy
    "NAS_CONG"               = "COVID 19 - Sign and symptom_Other symptoms_Nasal congestion", # dummy 
    "CHILLS"                 = "COVID 19 - Sign and symptom_Other symptoms_Chills", # dummy
    "TASTE_LOSS"             = "COVID 19 - Sign and symptom_Other symptoms_Loss of taste", # dummy   
    "SMELL_LOSS"             = "COVID 19 - Sign and symptom_Other symptoms_Loss of smell", # dummy    
    "SYMPOTH"                = "COVID 19 - Sign and symptom_Other symptoms_Other", # dummy   
    "SYMPOTHR_SP"            = "COVID 19 - Sign and symptom_If other symptoms, please specify",    
    "LYMPH_NOD"              = "COVID 19 - Sign and symptom_Signs of infection_Lymph nodes enlarged", # dummy
    "TONS_SWELL"             = "COVID 19 - Sign and symptom_Signs of infection_Tonsils swelling", # dummy
    "THR_CONG"               = "COVID 19 - Sign and symptom_Signs of infection_Throat congestion", # dummy
    "RASH"                   = "COVID 19 - Sign and symptom_Signs of infection_Rash", # dummy
    "SIGNSOTH"               = "COVID 19 - Sign and symptom_Signs of infection_Other", # dummy
    "SIGNSOTH_SP"            = "COVID 19 - Sign and symptom_If other signs, please specify",
    "EXPOSURE"               = "COVID 19 - Sign and symptom_Potential exposure to source of transmission within past two weeks?", 
    "EXPOSURE_SP"            = "COVID 19 - Sign and symptom_If YES, please detail",    
    "GEO_AREA"               = "COVID 19 - Sign and symptom_Supposed geographical area of infection", 
    "LAB_DATA"               = "COVID 19 - Laboratory data_Date of laboratory data", 
    "WBC_ND"                 = "COVID 19 - Laboratory data_WHITE BLOOD CELL count",
    "WBC"                    = "COVID 19 - Laboratory data_WHITE BLOOD CELL count", # dummy
    "WBC_VALUE_Covid"        = "COVID 19 - Laboratory data_WBC count, if abnormal",        
    "RBC_ND"                 = "COVID 19 - Laboratory data_RED BLOOD CELL count",
    "RBC"                    = "COVID 19 - Laboratory data_RED BLOOD CELL count", # dummy
    "RBC_VALUE"              = "COVID 19 - Laboratory data_RBC count, if abnormal",  
    "LYMPH_ND"               = "COVID 19 - Laboratory data_LYMPHOCYTE count", 
    "LYMPH"                  = "COVID 19 - Laboratory data_LYMPHOCYTE count", # dummy
    "LYMPH_VALUE_Covid"      = "COVID 19 - Laboratory data_Lymphocyte count, if abnormal",          
    "PLATELETS_ND"           = "COVID 19 - Laboratory data_PLATELET count",     
    "PLATELETS"              = "COVID 19 - Laboratory data_PLATELET count", # dummy
    "PLATELETS_VALUE"        = "COVID 19 - Laboratory data_Platelet count, if abnormal",        
    "HEMOGL_ND"              = "COVID 19 - Laboratory data_HEMOGLOBIN",  
    "HEMOGL"                 = "COVID 19 - Laboratory data_HEMOGLOBIN", # dummy
    "HEMOGL_VALUE"           = "COVID 19 - Laboratory data_Hemoglobin value, if abnormal",     
    "RATIO_ND"               = "COVID 19 - Laboratory data_PaO2/FiO2", 
    "RATIO"                  = "COVID 19 - Laboratory data_PaO2/FiO2", # dummy
    "RATIO_VALUE"            = "COVID 19 - Laboratory data_PaO2/FiO2, if abnormal",    
    "FIRST_STREP"            = "FIRST_STREP", #"COVID19 - Diagnosis, Treatment_First oropharyngeal/nasopharyngeal swabs",    
    "SECOND_STREP"           = "SECOND_STREP", #"COVID19 - Diagnosis, Treatment_Second oropharyngeal/nasopharyngeal swabs",     
    "ASYMPTOMATIC"           = "COVID 19 - Sign and symptom_No symptoms",
    "ASYMPT_REASON"          = "COVID 19 - Sign and symptom_In case of asymptomatic patient, reason for test execution (SWAB and/or SEROLOGICAL) ",
    "ASYMPT_REASON_Other"    = "COVID 19 - Sign and symptom_If other reason for test execution, please specify",
    # "FIRST_STREP_oldmethod"  = "COVID19 - Diagnosis, Treatment_First oropharyngeal/nasopharyngeal swabs",    
    # "SECOND_STREP_oldmethod" = "COVID19 - Diagnosis, Treatment_Second oropharyngeal/nasopharyngeal swabs",     
    "IgG"                    = "IgG",
    "IgM"                    = "IgM",
    "any_serology"           = "any_serology",
    "ANTIVIRAL"              = "COVID19 - Diagnosis, Treatment_Treatments_Antiviral",  
    "CHLOROQ"                = "COVID19 - Diagnosis, Treatment_Treatments_Antiviral", # dummy
    "HYDROX"                 = "COVID19 - Diagnosis, Treatment_Treatments_Hydroxychloroquine", # dummy
    "IV_ANTIB"               = "COVID19 - Diagnosis, Treatment_Treatments_Intravenous Antibiotics", # dummy
    "SYST_GLUC"              = "COVID19 - Diagnosis, Treatment_Treatments_Systemic Glucocorticoids", # dummy
    "OXYG_THER"              = "COVID19 - Diagnosis, Treatment_Treatments_Oxygen Therapy", # dummy
    "IVIMGL"                 = "COVID19 - Diagnosis, Treatment_Treatments_Intravenous Immune Globulin", # dummy
    "OTHER"                  = "COVID19 - Diagnosis, Treatment_Treatments_Other", # dummy
    "OTHER_TRT"              = NA, #?
    "VENT"                   = "COVID19 - Diagnosis, Treatment_Mechanical ventilation",
    "VENT_TYPE"              = "COVID19 - Diagnosis, Treatment_If ventilation is mechanical, please specify",  
    "COVID_SEVERITY"         = "COVID19 - Diagnosis, Treatment_Severity (for COVID positive patients)",       
    "HOSPIT"                 = NA, #"COVID19 - Diagnosis, Treatment_Hospitalization",
    "FU1_DATE"               = NA, #"COVID 19 - Follow-up_Date of Visit (or mail or telephone contact)", 
    "FU1_OUTCOME"            = NA, #"COVID 19 - Follow-up_Outcome",    
    "FU1_RECOV_DATE"         = NA, #"COVID 19 - Follow-up_If recovered, please report date",       
    "FU1_DEATH_DATE"         = NA, #"COVID 19 - Follow-up_In case of death, report date",       
    "FU1_DEATH_CAUSE"        = NA, #"COVID 19 - Follow-up_Cause of death",        
    "FU1_HOSPIT"             = NA, #"COVID 19 - Follow-up_Hospitalized since previous contact",   
    "FU1_HOSPIT_DATE"        = NA, #"COVID 19 - Follow-up_If hospitalized, date",        
    "FU1_DISC"               = NA, #"COVID 19 - Follow-up_Discharged since previous contact", 
    "FU1_DISC_DATE"          = NA, #"COVID 19 - Follow-up_If discharged, date",      
    # "Tampone"                = "COVID19 - Diagnosis, Treatment_First oropharyngeal/nasopharyngeal swabs",
    # "Tampone2"               = "COVID19 - Diagnosis, Treatment_Second oropharyngeal/nasopharyngeal swabs", 
    "COVID_DATE"             = NA,   
    "HOSPIT_DATE"            = NA,    
    "COVID_SEVERITY2"        = NA,        
    "RRvsProgressive"        = NA,        
    "COVID_SEVERITY3"        = NA,        
    "Death"                  = NA,
    "HOSPIT_overall"         = NA,       
    "HOSPIT_final"           = NA,
    "DATE_CREATED"           = "Base_Created at"
  )
)

#############################
# Convert
#############################

convert <- function(file) {
  
  # read file
  f <- data.frame(line = readr::read_lines(file))
  f <- mutate(f, fields = stringr::str_count(line, ";"))
  
  # bugfix turco
  marker <- ";;;;;;;13/10/2016;Relapsing remitting MS (RRMS);1;14/07/2020;Yes;1st line;Interferon;;16/06/2020;16/07/2020;No;Patient's decision;;No;;Yes;14/07/2020;19/07/2020;14/07/2020;Normal or NCS;;14/07/2020;Normal or NCS;;30/08/2020 10:25;30/08/2020 10:25"
  to_remove <- which(f$line == marker)
  if (!is_empty(to_remove)) {f <- f[-to_remove,]}
  
  # indentify patients
  f <- mutate(f, pt = line == new_patient_tag)
  
  # name patients
  f <- mutate(f, pt = cumsum(pt)) %>%
    filter(!(fields == 0 & trimws(line) == ""))
  
  # indentify headers
  f <- mutate(f, is_header = map_lgl(line, ~ any(str_detect(., headers))))
  
  # identify multiple objects
  f <- f %>% group_by(pt) %>% 
    mutate(block = cumsum(is_header)) %>% 
    group_by(pt, block) %>% 
    
    # \n bug
    mutate(newline = str_count(line, "\"") %% 2) %>% 
    mutate(newline_open = cumsum(newline) %% 2) %>% 
    mutate(newline = as.numeric(newline == 1 | newline_open == 1)) %>% 
    group_by(pt, block, is_header, newline) %>% 
    
    # continue
    group_by(pt, block) %>% 
    mutate(sheets = n()) %>% 
    
    mutate(newline_penalty = ifelse(is_header & any(newline), sum(newline)-1, 0)) %>%
    group_by(pt, block) %>%
    mutate(multiplier = ifelse(is_header & sheets > 2, sheets-1-newline_penalty, 1)) %>%
    
    uncount(multiplier) %>% 
    ungroup()
  
  # remove multiple insertion after pass
  # f <- group_by(f, pt, block) %>%
  #   mutate(compl_header = sum_f == header_f)
  #   mutate(is_unique = !duplicated(str_remove_all(line, ";"))) %>%
  #   mutate(shouldBe_unique = !map_lgl(line, ~ any(str_detect(., repeated_ev)))) %>%
  #   # filter(!(shouldBe_unique & !is_unique)) %>%
  #   ungroup()
  
  # condensate info
  f <- group_by(f, pt, is_header) %>%
    summarise(line = paste(line, collapse = ";"),
              fields = sum(fields))
  
  # integrity check
  cat("First attempt: plain text")
  f %>% group_by(pt) %>% summarise(ok = all(fields == fields[1])) %>% 
    filter(!ok) %>% .$pt
  
  # bug multiple insertion after pass
  # f <- filter(f, pt != 464)
  
  # fix the ";" bug
  reimport <- function(line) {
    # cat("--> handling errors on: ", line, "\n\n")
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
  cat("Second attempt: reimport trick")
  f %>% group_by(pt) %>% summarise(ok = all(fields == fields[1])) %>% 
    filter(!ok) %>% .$pt
  
  # tokenization and verticalization
  max_length <- 400
  f <- f %>%
    mutate(ambiguous_separator = ifelse(grepl("\".*\"", line), 0, 1)) %>%
    separate(line, as.character(1:max_length), sep = ";") %>% 
    tidyr::gather("position", "value", as.character(1:max_length), convert = TRUE) %>%
    group_by(pt, position) %>% 
    summarise(value = paste(value, collapse = "!@!@")) %>% 
    tidyr::separate(value, c("value", "header"), sep = "!@!@") %>% 
    filter(header != "NA", !is.na(header)) %>%
    arrange(pt, position)
  
  # include topic in variable name
  f <- f %>% 
    group_by(pt) %>% 
    mutate(blank = ifelse(header == "", 1, 0)) %>% 
    mutate(prev_blank = lag(blank)) %>% 
    mutate(title = lag(blank == 1 & prev_blank == 1)) %>% 
    mutate(title = ifelse(title, header, NA)) %>% 
    group_by(pt, title) %>%
    mutate(n = row_number(),
           constrains = ifelse(!is.na(title) & is.na(section_constrains[title]), TRUE,
                               section_constrains[title] == n)) %>%
    group_by(pt) %>%
    fill(constrains) %>% 
    {filter(., !is.na(constrains) & constrains == FALSE) %>% 
        select(pt) %>% unique() %>% unlist() %>% 
        cat("\nPotential data loss on these pts:", ., "\n"); .} %>% 
    filter(constrains | is.na(constrains)) %>% 
    filter(!blank) %>% 
    fill(title) %>% 
    mutate(title = ifelse(is.na(title), "Base", title)) %>%
    select(-blank, -prev_blank, -constrains, -n) %>% 
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
  
  return(f)
}

#############################
# Clean
#############################

clean <- function(data) {

  # leading zeros on Site Code
  f <- mutate(data, `Demography_Site Code` = str_pad(`Demography_Site Code`, 2, pad = "0"))
  f <- mutate(f, `Demography_Patient Code` = str_pad(`Demography_Patient Code`, 2, pad = "0"))

  # patient ID
  f <- mutate(f, upid = sprintf("%s-%s-%s", Demography_Country, 
                                `Demography_Site Code`, `Demography_Patient Code`))
  
  # check univocal ID
  excpt <- count(f, upid) %>% filter(n > 1) %>% 
    filter(upid != "--00") %>% {unlist(.$upid)}
  cat("***", paste("The following unique patient identifiers are not uinque:\n", 
             paste(excpt, collapse = ", "), ".\n"))
  
  # handle duplicate ID
  f <- f %>% 
    group_by(upid) %>% 
    mutate(duplicates = row_number()) %>%
    ungroup() %>% 
    mutate(upid = ifelse(upid %in% excpt, 
                         yes = paste("DUPLICATE", upid, duplicates, sep = "_"), upid))
  
  # dummification
  # COVID 19 - Sign and symptom_Other symptoms
  dummification_size <- 20
  f <- separate(f, `COVID 19 - Sign and symptom_Other symptoms`,
           into = as.character(1:dummification_size), sep = ",") %>% 
    select(upid, as.character(1:dummification_size)) %>% 
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
  
  # reduce size
  f <- mutate_all(f, ~ ifelse(trimws(.) == "", NA, .)) %>% 
    select_if( ~ !all(is.na(.)))
  
  f <- select(f, upid, contains("swab")) %>%
    gather("var", "val", -upid) %>%
    filter(!grepl("On going", val)) %>%
    mutate(n_swab = ifelse(grepl("First", var), "FIRST_STREP", "SECOND_STREP")) %>%
    # select(-var) %>%
    filter(!is.na(val)) %>%
    group_by(upid, n_swab) %>%
    summarise(val = case_when(any(val == "Positive")     ~ "Positive",
                              any(val == "Negative")     ~ "Negative",
                              any(val == "Not executed") ~ "Not executed")) %>%
    spread(n_swab, val) %>%
    ungroup() %>%
    right_join(f)
  
  f <- select(f, upid, contains("Serology blood test_Ig")) %>%
    gather("var", "val", -upid) %>% 
    filter(!is.na(val)) %>%
    mutate(type = str_extract(var, "Ig.")) %>% 
    group_by(upid, type) %>% 
    summarise(val = case_when(any(val == "Positive") ~ "Positive",
                              any(val == "Negative") ~ "Negative",
                              any(val == "Not done") ~ "Not done",
                              TRUE ~ NA_character_)) %>% 
    ungroup() %>% 
    spread(type, val) %>% 
    mutate(any_serology = ifelse(IgG == "Positive" | IgM == "Positive", "Positive", NA)) %>% 
    right_join(f)
  
  # reverse naive
  f$naive <- ifelse(f$`MS history_Was the patient previously treated?` == "Yes",
                    yes = "No", no = "Yes")
  
  return(f)
}

# clean name for spss
# names(f) <- stringr::str_replace_all(names(f), ";|,|-| |\\(|\\)|\\.|\\\\|\\/|\\+", "_") %>% 
#   strtrim(40) %>% make.unique("_")
# f <- mutate_all(f, ~ stringr::str_replace_all(., ";|,|-| |:|/", "_"))

#############################
# Summary
#############################

enrollment_summary <- function(data) {
  data %>% 
    filter(!(`Demography_Site Code` %in% c("", "00"))) %>% 
    count(Base_Provider,
        Demography_Country,
        `Demography_Site Code`,
        sort = TRUE)
}

enrollment_summary_detail <- function(data) {
  data %>% 
    filter(!(`Demography_Site Code` %in% c("", "00"))) %>% 
    select(Base_Provider,
           Demography_Country,
           `Demography_Site Code`,
           `Demography_Patient Code`,
           `Base_Created at`) %>% 
    arrange(Demography_Country, 
            `Demography_Site Code`, 
            `Demography_Patient Code`,
            `Base_Created at`)
}


#############################
# type conversion
#############################

type_adjust <- function(data) {
  type_convert(data) %>%
    mutate_at(vars(DIC, DOV, ends_with("DATE"), METGL_START, METGL_STOP,
                   START_DMD, STOP_DMD, FEVER_ONSET, LAB_DATA), lubridate::dmy)
}

#############################
# Export
#############################

prepare_export <- function(f, staff = FALSE) {
  # name of the exported file
  fname <- paste("musc19", format(Sys.time(), "_%d%b%Y"), ".sav", sep = "")
  
  # fit destination format
  selected_col <- unname(MPS_format$var_dictionary) %>% na.omit()
  selected_col <- intersect(selected_col, names(f))
  col_index <- which(MPS_format$var_dictionary %in% names(f))
  
  # rename columns and take white list
  export_real <- f %>% 
    filter(!(Base_Provider %in% MPS_format$provider_blacklist))
  
  export_staff <- f %>% 
    filter(Base_Provider %in% MPS_format$staff)
  
  ifelse(staff, export <- export_staff, export <- export_real)
  
  export <- export %>%
    select(all_of(selected_col)) %>% 
    rename(MPS_format$var_dictionary[col_index]) %>% 
    type_adjust()
  
  return(export)
}

# select_if(temp, ~ all(!is.na(.)))

# save spss output
# haven::write_sav(export, fname)

#############################
# Single pipeline
#############################

load_musc <- function(fromExported, andSaveItAs) {
  dat <- fromExported %>% convert() %>% clean() %>% prepare_export()
  haven::write_sav(dat, andSaveItAs)
}

#############################
# Export singolo centro
#############################
# 
# temp <- clean(convert("~/Downloads/report (10).csv"))
# 
# names(temp)
# 
# temp[temp$Base_Provider == "Cinzia Cordioli",] %>% 
#   mutate() %>% 
#   select(upid, contains("Base"), 
#          contains("Demography"), 
#          contains("MS history"), 
#          contains("Comorbidity"),
#          contains("Surgery"),
#          contains("COVID 19 - Sign"),
#          contains("COVID19 - Diagnosis"),
#          contains("COVID 19 - Detailed"),
#          contains("COVID 19 - Radiological"),
#          contains("COVID 19 - Laboratory"),
#          contains("COVID 19 - Follow"),
#          contains("COVID19 - Complication"),
#          -ends_with("_Updated at"),
#          -ends_with("_Created at"),
#          -`Demography_Site Code`,
#          -`Demography_Patient Code`) %>%
#   mutate(upid = str_remove_all(upid, "DUPLICATE_|_\\d")) %>% 
#   arrange(upid) %>% 
#   select_if(function(x) {!all(is.na(x) | trimws(x) == "")}) %>% 
#   write.csv("~/Downloads/Cordioli.csv")
# 
# temp2 <- read_csv("~/Downloads/Cordioli.csv", 
#                   na = c("", "NA", " "))
# 
# openxlsx::write.xlsx(temp2, "~/Downloads/Cordioli.xlsx")

#############################
# fup
#############################

# f <- clean(convert("~/Downloads/report (10).csv"))
# 
# deaths <- select(f, upid, contains("outcome")) %>% 
#   gather("time", "outcome", -upid) %>% 
#   filter(!is.na(outcome)) %>% 
#   mutate(dead = outcome == "Death") %>% 
#   group_by(upid) %>% 
#   summarise(dead = any(dead, na.rm = TRUE))
# 
# require(lubridate)
# 
# # pick vars
# fup_times <- select(f, upid, 
#        provider = Base_Provider, 
#        first_visit = `Demography_Date of Visit`, 
#        contains("Follow-up_Date of Visit")) %>% 
#   
#   # remove empty
#   filter(!(trimws(first_visit) == "" | is.na(first_visit))) %>% 
#   select_if(function(x) {!all(is.na(x) | trimws(x) == "")}) %>% 
#   
#   # verticalize dates
#   gather("time", "first_date", names(.)[3:ncol(.)]) %>% 
#   filter(!is.na(first_date)) %>% 
#   mutate(first_date = lubridate::dmy(first_date)) %>% 
#   
#   # add info by upid
#   group_by(upid) %>% 
#   mutate(last_date = max(first_date, na.rm = TRUE),
#          first_date = min(first_date, na.rm = TRUE),
#          n_fup = n() - 1) %>% 
#   
#   # remove in between visits
#   filter(time == "first_visit") %>% 
#   select(-time) %>% 
#   
#   # add today
#   mutate(last_date = case_when(last_date == first_date ~ lubridate::today(), TRUE ~ last_date)) %>% 
#   mutate(last_contact = (first_date %--% last_date)/lubridate::days(1)) %>% 
#   
#   # consider deaths
#   left_join(deaths) %>% 
#   
#   # sort by last contact
#   arrange(desc(last_contact))
# 
# openxlsx::write.xlsx(fup_times, "~/Downloads/fup_times.xlsx")

#############################
# Fin.
