
# spss_doc <- haven::read_spss("~/Downloads/Estrazione25Maggio_.sav") %>% 
#   filter(`filter_$` == 1)

# f<-clean(convert("~/Downloads/report-2.csv"))
# ioe <- ioe(f)

# db <- filter(ioe, COUNTRY == "Italy")
# names(ioe)

# rescue clad
# db[grepl("clad", tolower(db$DMD_OTH)),]$NAME_DMD <- "Cladribine"


generate_table1 <- function(db) {
  
  # grouping
  db$COHAB_cat <- cut(db$COHAB, 
                      breaks = c(-1, 0.5, 1.5, 2.5, 3.5, 100),
                      labels = c("0", "1", "2", "3", "4+"))
  
  db$COHAB_CHILD_cat <- cut(db$COHAB_CHILD, 
                      breaks = c(-1, 0.5, 1.5, 100),
                      labels = c("0", "1", "2+"))
  
  db$COVID_COHAB_cat <- cut(db$COVID_COHAB, 
                            breaks = c(-1, 0.5, 1.5, 100),
                            labels = c("0", "1", "2+"))
  
  # swab mix
  table(first = addNA(db$FIRST_STREP_oldmethod), 
        second = addNA(db$SECOND_STREP_oldmethod)) %>% addmargins()
  
  table(first = addNA(db$FIRST_STREP), 
        second = addNA(db$SECOND_STREP)) %>% addmargins()
  
  db$positives <- 
    (!is.na(db$FIRST_STREP) & db$FIRST_STREP == "Positive") | 
    (!is.na(db$SECOND_STREP) & db$SECOND_STREP == "Positive")
  # |
  #   (!is.na(db$any_serology) & db$any_serology == "Positive")
  
  table(db$positives) %>% addmargins()
  
  # coms
  db <- mutate_at(db, vars(contains("com_"), ABUSE), ~ ifelse(. == "", "No", .))
  
  # pregn
  db <- mutate(db, PREGN = case_when(PREGN == "" & SEX == "F" ~ "No",
                                     PREGN == "" & SEX == "M" ~ "N.A. (male)",
                                     TRUE ~ PREGN))
  
  fact_var <- c(
    "CONTACT_TYPE"       ,
    "SEX"                ,
    "PREGN"              ,
    "EMPL"               ,
    "COHAB_cat"              ,
    "COHAB_CHILD_cat"        ,
    "COVID_COHAB_cat"        ,
    "HEALTHCARE_JOB"     ,
    "SMOKE"              ,
    "SMOKE_TYPE"         ,
    "ALCOHOL"            ,
    "ABUSE"              ,
    "MS_TYPE"            ,
    "TREAT"              ,
    "LAST_DMD"           ,
    "NAME_DMD"           ,
    "ONG_DMD"            ,
    "DMD_INTERR"         ,
    "METH_GLUC"          ,
    "WBC_RESULT"         ,
    "LYMPH_RESULT"       ,
    "FEVER"              ,
    "COUGH"              ,
    "FATIGUE"            ,
    "SPUTUM_PROD"        ,
    "SORE_THR"           ,
    "HEADACHE"           ,
    "BONE_JOINT"         ,
    "SHORT_BR"           ,
    "NAS_CONG"           ,
    "CHILLS"             ,
    "TASTE_LOSS"         ,
    "SMELL_LOSS"         ,
    "SYMPOTH"            ,
    "LYMPH_NOD"          ,
    "TONS_SWELL"         ,
    "THR_CONG"           ,
    "RASH"               ,
    "SIGNSOTH"           ,
    "EXPOSURE"           ,
    "WBC"                ,
    "RBC"                ,
    "LYMPH"              ,
    "PLATELETS"          ,
    "HEMOGL"             ,
    "RATIO"              ,
    "CHLOROQ"            ,
    "HYDROX"             ,
    "IV_ANTIB"           ,
    "SYST_GLUC"          ,
    "OXYG_THER"          ,
    "IVIMGL"             ,
    "OTHER"              ,
    "VENT"               ,
    "VENT_TYPE"          ,
    "com_can"            ,
    "com_dep"            ,
    "com_hae"            ,
    "com_hiv"            ,
    "com_hbv"            ,
    "com_dbt"            ,
    "com_chd"            ,
    "com_cld"            ,
    "com_ckd"            ,
    "com_hts"            ,
    "com_cvd"            ,
    "death_event"        ,
    "hospi_event"        ,
    "icu_event"          ,
    "pneum_event"        ,
    "sever_event"     
  )
  
  voi <- c(
    "CONTACT_TYPE"       ,
    "AGE"                ,
    "SEX"                ,
    "BMI"                ,
    "com_can"            ,
    "com_dep"            ,
    "com_hae"            ,
    "com_hiv"            ,
    "com_hbv"            ,
    "com_dbt"            ,
    "com_chd"            ,
    "com_cld"            ,
    "com_ckd"            ,
    "com_hts"            ,
    "com_cvd"            ,
    "ETHN"               ,
    # "ETHN_OTH"           ,
    "PREGN"              ,
    "SMOKE"              ,
    "SMOKE_TYPE"         ,
    "ALCOHOL"            ,
    "ABUSE"              ,
    "COHAB_cat"              ,
    "COHAB_CHILD_cat"        ,
    "COVID_COHAB_cat"        ,
    "EMPL"               ,
    # "EMPL_OTH"           ,
    "HEALTHCARE_JOB"     ,
    "MS_TYPE"            ,
    "msh_disdur"         ,
    "LAST_EDSS"          ,
    "TREAT"              ,
    "LAST_DMD"           ,
    "NAME_DMD"           ,
    # "DMD_OTH"            ,
    "ONG_DMD"            ,
    "DMD_INTERR"         ,
    # "DMD_INTER_OTH"      ,
    "METH_GLUC"          ,
    "WBC_RESULT"         ,
    "LYMPH_RESULT"       ,
    "FEVER"              ,
    "FEVER_VALUE"        ,
    "COUGH"              ,
    "FATIGUE"            ,
    "SPUTUM_PROD"        ,
    "SORE_THR"           ,
    "HEADACHE"           ,
    "BONE_JOINT"         ,
    "SHORT_BR"           ,
    "NAS_CONG"           ,
    "CHILLS"             ,
    "TASTE_LOSS"         ,
    "SMELL_LOSS"         ,
    "SYMPOTH"            ,
    # "SYMPOTHR_SP"        ,
    "LYMPH_NOD"          ,
    "TONS_SWELL"         ,
    "THR_CONG"           ,
    "RASH"               ,
    "SIGNSOTH"           ,
    # "SIGNSOTH_SP"        ,
    "EXPOSURE"           ,
    "WBC"                ,
    "WBC_VALUE_Covid"    ,
    "RBC"                ,
    "RBC_VALUE"          ,
    "LYMPH"              ,
    "LYMPH_VALUE_Covid"  ,
    "PLATELETS"          ,
    "PLATELETS_VALUE"    ,
    "HEMOGL"             ,
    "HEMOGL_VALUE"       ,
    "RATIO"              ,
    "RATIO_VALUE"        ,
    "CHLOROQ"            ,
    "HYDROX"             ,
    "IV_ANTIB"           ,
    "SYST_GLUC"          ,
    "OXYG_THER"          ,
    "IVIMGL"             ,
    "OTHER"              ,
    "VENT"               ,
    "VENT_TYPE"          ,
    # "COVID_SEVERITY"     ,
    "death_event"        ,
    "hospi_event"        ,
    "icu_event"          ,
    "pneum_event"        ,
    "sever_event"     
    )
  
  nonnorm <- c(
    "msh_disdur",
    "HEMOGL_VALUE",
    "RATIO_VALUE",
    "PLATELETS_VALUE",
    "RBC_VALUE",
    "WBC_VALUE_Covid",
    "LYMPH_VALUE_Covid"
  )
  
  tbl1_db <- 
    tableone::CreateTableOne(vars = voi, 
                             strata = "positives",
                           data = db, 
                           addOverall = TRUE,
                           factorVars = fact_var, 
                           includeNA = TRUE) %>% 
    print(togglePrint = FALSE, nonnorm = nonnorm, varLabels = TRUE) %>%
    {names_ <<- rownames(.); .} %>%
    as_tibble() %>%
    mutate(Name = names_) %>% 
    select(Name, Overall,
           Suspected = `FALSE`, 
           Confirmed = `TRUE`,
           `P value` = p) %>% 
    mutate_at(vars(Confirmed, Suspected), ~ as.character(trimws(.))) %>% 
    mutate(Name = str_replace(Name, " = 1", "")) %>% 
    mutate(Name = str_replace(Name, " \\(mean \\(SD\\)\\)", ", Mean (SD)")) %>% 
    mutate(Name = str_replace(Name, "= Yes", "")) %>% 
    mutate(Name = gsub(Name, pattern = " .median .IQR..", replacement = ", Median (IQR)")) %>% 
    mutate_at(vars(Confirmed, Suspected), ~ gsub(., pattern = "\\[(.*), (.*)\\]", replacement = "(\\1-\\2)")) %>% 
    mutate_at(vars(Confirmed, Suspected), ~ gsub(., pattern = "\\.00", replacement = "")) %>% 
    mutate_at(vars(Confirmed, Suspected), ~ gsub(., pattern = "\\(\\s*(\\S)", replacement = "(\\1")) %>% 
    mutate(Name = gsub(Name, pattern = "= (.*)", replacement = "\\1")) %>% 
    mutate(Name = gsub(Name, pattern = " \\(\\%\\)", replacement = " － no. (%)"))
  
  return(tbl1_db)
}

