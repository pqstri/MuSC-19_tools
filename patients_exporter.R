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

#############################
# Format
#############################

headers <- c("Provider;", ";;Demography", ";;MS history", ";;COVID 19 - ", 
             ";;COVID19 - ", ";;Comorbidity", ";;Surgery", ";;Comment")

new_patient_tag <- "Provider;Patient;Sex;Created at;Updated at"

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
                         "Marta Ponzano", "temp", "Irene Schiavetti"),
  
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
    "SPUTUM_PROD"            = "COVID 19 - Sign and symptom_Other symptoms_ Sputum production", # dummy
    "SORE_THR"               = "COVID 19 - Sign and symptom_Other symptoms_ Sore throat", # dummy
    "HEADACHE"               = "COVID 19 - Sign and symptom_Other symptoms_ Headache", # dummy
    "BONE_JOINT"             = "COVID 19 - Sign and symptom_Other symptoms_ Bone or joint pain", # dummy 
    "SHORT_BR"               = "COVID 19 - Sign and symptom_Other symptoms_ Shortness of breath", # dummy
    "NAS_CONG"               = "COVID 19 - Sign and symptom_Other symptoms_ Nasal congestion", # dummy 
    "CHILLS"                 = "COVID 19 - Sign and symptom_Other symptoms_ Chills", # dummy
    "TASTE_LOSS"             = "COVID 19 - Sign and symptom_Other symptoms_ Loss of taste", # dummy   
    "SMELL_LOSS"             = "COVID 19 - Sign and symptom_Other symptoms_ Loss of smell", # dummy    
    "SYMPOTH"                = "COVID 19 - Sign and symptom_Other symptoms_ Other", # dummy   
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
    "FIRST_STREP"            = "COVID19 - Diagnosis, Treatment_First oropharyngeal/nasopharyngeal swabs",    
    "SECOND_STREP"           = "COVID19 - Diagnosis, Treatment_Second oropharyngeal/nasopharyngeal swabs",     
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
    "HOSPIT"                 = "COVID19 - Diagnosis, Treatment_Hospitalization",
    "FU1_DATE"               = "COVID 19 - Follow-up_Date of Visit (or mail or telephone contact)", 
    "FU1_OUTCOME"            = "COVID 19 - Follow-up_Outcome",    
    "FU1_RECOV_DATE"         = "COVID 19 - Follow-up_If recovered, please report date",       
    "FU1_DEATH_DATE"         = "COVID 19 - Follow-up_In case of death, report date",       
    "FU1_DEATH_CAUSE"        = "COVID 19 - Follow-up_Cause of death",        
    "FU1_HOSPIT"             = "COVID 19 - Follow-up_Hospitalized since previous contact",   
    "FU1_HOSPIT_DATE"        = "COVID 19 - Follow-up_If hospitalized, date",        
    "FU1_DISC"               = "COVID 19 - Follow-up_Discharged since previous contact", 
    "FU1_DISC_DATE"          = "COVID 19 - Follow-up_If discharged, date",      
    "Tampone"                = NA,
    "Tampone2"               = NA, 
    "COVID_DATE"             = NA,   
    "HOSPIT_DATE"            = NA,    
    "COVID_SEVERITY2"        = NA,        
    "RRvsProgressive"        = NA,        
    "COVID_SEVERITY3"        = NA,        
    "Death"                  = NA,
    "HOSPIT_overall"         = NA,       
    "HOSPIT_final"           = NA
  )
)

#############################
# Convert
#############################

convert <- function(file) {
  
  # read file
  f <- data.frame(line = readr::read_lines(file))
  f <- mutate(f, fields = stringr::str_count(line, ";"))
  
  # indentify patients
  f <- mutate(f, pt = line == new_patient_tag)
  
  # name patients
  f <- mutate(f, pt = cumsum(pt)) %>%
    filter(fields != 0)
  
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
  
  # condensate info
  f <- group_by(f, pt, is_header) %>%
    summarise(line = paste(line, collapse = ";"),
              fields = sum(fields))
  
  # integrity check
  f %>% group_by(pt) %>% summarise(ok = all(fields == fields[1])) %>% 
    filter(!ok) %>% .$pt
  
  # fix the ";" bug
  reimport <- function(line) {
    cat("--> handling errors on: ", line, "\n\n")
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
  f %>% group_by(pt) %>% summarise(ok = all(fields == fields[1])) %>% 
    filter(!ok) %>% .$pt
  
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
  cat("***", paste("The unique patient identifier in not uinque: ", 
             paste(excpt, collapse = ", ")))
  
  # handle duplicate ID
  f <- f %>% 
    group_by(upid) %>% 
    mutate(duplicates = row_number()) %>%
    ungroup() %>% 
    mutate(upid = ifelse(upid %in% excpt, 
                         yes = paste("DUPLICATE", upid, duplicates, sep = "_"), upid))
  
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
    rename(MPS_format$var_dictionary[col_index])
  
  return(export)
}

# save spss output
# haven::write_sav(export, fname)





