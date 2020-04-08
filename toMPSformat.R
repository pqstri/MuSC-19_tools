# ids
sprintf("%s-%s-%s", f$Demography_Country, f$`Demography_Site Code`, f$`Demography_Patient Code`)

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

  provider_blacklist = c("Base_Provider"),

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
