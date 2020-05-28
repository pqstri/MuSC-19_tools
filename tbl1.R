
# spss_doc <- haven::read_spss("~/Downloads/Estrazione25Maggio_.sav") %>% 
#   filter(`filter_$` == 1)

# f<-clean(convert("~/Downloads/report-2.csv"))
# ioe <- ioe(f)

# db <- filter(ioe, COUNTRY == "Italy")
# names(ioe)

# rescue clad
# db[grepl("clad", tolower(db$DMD_OTH)),]$NAME_DMD <- "Cladribine"


generate_table1 <- function(db) {
  
  db$`Age` <- cut(db$AGE, 
                    breaks = c(-Inf, 39.5, 59.5, 79.5, Inf),
                    labels = c("<40 yr", "40–59 yr", "60–79 yr", "≥80"))
  
  db$`Female sex` <- db$SEX == "Female"
  
  # db$`Current smoking` <- if_else(db$SMOKE == "Current smoker", TRUE, FALSE, NA)
  
  attr(db$SMOKE, "label") <- "Smoking"
  attr(db$msh_disdur, "label") <- "MS disease duration"
  attr(db$LAST_EDSS, "label") <- "EDSS"
  attr(db$NAME_DMD, "label") <- "MS Treatment"
  
  
  # db$msh_disdur2 <- db$msh_disdur
  # attr(db$msh_disdur2, "label") <- ""
  
  db$`   Cancer` <- if_else(db$com_can == "Yes", TRUE, FALSE, FALSE)
  db$`   Major depressive disorder` <- if_else(db$com_dep == "Yes", TRUE, FALSE, FALSE)
  db$`   Haematological disease` <- if_else(db$com_hae == "Yes", TRUE, FALSE, FALSE)
  db$`   HIV infection` <- if_else(db$com_hiv == "Yes", TRUE, FALSE, FALSE)
  db$`   HBV infection` <- if_else(db$com_hbv == "Yes", TRUE, FALSE, FALSE)
  db$`   Diabetes` <- if_else(db$com_dbt == "Yes", TRUE, FALSE, FALSE)
  db$`   Hypertension` <- if_else(db$com_hts == "Yes", TRUE, FALSE, FALSE)
  db$`   Coronary heart disease` <- if_else(db$com_chd == "Yes", TRUE, FALSE, FALSE)
  db$`   Chronic liver disease` <- if_else(db$com_cld == "Yes", TRUE, FALSE, FALSE)
  db$`   Chronic kidney disease` <- if_else(db$com_ckd == "Yes", TRUE, FALSE, FALSE)
  db$`   Cerebrovascular disease` <- if_else(db$com_cvd == "Yes", TRUE, FALSE, FALSE)
  
  
  db$`   Fever` <- if_else(db$FEVER == "Yes", TRUE, FALSE, FALSE)
  db$`   Cough` <- if_else(db$COUGH == 1, TRUE, FALSE, FALSE)
  db$`   Fatigue` <- if_else(db$FATIGUE == 1, TRUE, FALSE, FALSE)
  db$`   Sputum production` <- if_else(db$SPUTUM_PROD == 1, TRUE, FALSE, FALSE)
  db$`   Sore throat` <- if_else(db$SORE_THR == 1, TRUE, FALSE, FALSE)
  db$`   Nasal congestion` <- if_else(db$NAS_CONG == 1, TRUE, FALSE, FALSE)
  db$`   Chills` <- if_else(db$CHILLS == 1, TRUE, FALSE, FALSE)
  db$`   Loss of taste` <- if_else(db$`TASTE_LOSS` == 1, TRUE, FALSE, FALSE)
  db$`   Loss of smell` <- if_else(db$`SMELL_LOSS` == 1, TRUE, FALSE, FALSE)
  db$`   Lymph nodes enlargement` <- if_else(db$LYMPH_NOD == 1, TRUE, FALSE, FALSE)
  db$`   Tonsils swelling` <- if_else(db$TONS_SWELL == 1, TRUE, FALSE, FALSE)
  db$`   Throat congestion` <- if_else(db$THR_CONG == 1, TRUE, FALSE, FALSE)
  db$`   Rash` <- if_else(db$RASH == 1, TRUE, FALSE, FALSE)
  
  db$`MS phenotype` <- str_remove(db$MS_TYPE, " .. ......$")
  
  

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
    "Age"            ,
    "Female sex"         ,
    # "PREGN"              ,
    "EMPL"               ,
    "COHAB_cat"              ,
    "COHAB_CHILD_cat"        ,
    "COVID_COHAB_cat"        ,
    "HEALTHCARE_JOB"     ,
    "SMOKE"              ,
    "SMOKE_TYPE"         ,
    "ALCOHOL"            ,
    "ABUSE"              ,
    "MS phenotype"            ,
    "TREAT"              ,
    "LAST_DMD"           ,
    "NAME_DMD"           ,
    "ONG_DMD"            ,
    "DMD_INTERR"         ,
    "METH_GLUC"          ,
    "WBC_RESULT"         ,
    "LYMPH_RESULT"       ,
    "   Fever",
    "   Cough",
    "   Fatigue",
    "   Sputum production",
    "   Sore throat",
    "   Nasal congestion",
    "   Chills",
    "   Loss of taste`",
    "   Loss of smell`",
    "   Lymph nodes enlargement",
    "   Tonsils swelling",
    "   Throat congestion",
    "   Rash",
    "SIGNSOTH"           ,
    "EXPOSURE"           
    # "WBC"                ,
    # "RBC"                ,
    # "LYMPH"              ,
    # "PLATELETS"          ,
    # "HEMOGL"             ,
    # "RATIO"              ,
    # "CHLOROQ"            ,
    # "HYDROX"             ,
    # "IV_ANTIB"           ,
    # "SYST_GLUC"          ,
    # "OXYG_THER"          ,
    # "IVIMGL"             ,
    # "OTHER"              ,
    # "VENT"               ,
    # "VENT_TYPE"          ,
    # "   Cancer"            ,
    # "   Major depressive disorder"            ,
    # "   Haematological disease"            ,
    # "   HIV infection"            ,
    # "   HBV infection"            ,
    # "   Diabetes"            ,
    # "   Hypertension"            ,
    # "   Coronary heart disease"            ,
    # "   Chronic liver disease"            ,
    # "   Chronic kidney disease"            ,
    # "   Cerebrovascular disease"            ,
    # "death_event"        ,
    # "hospi_event"        ,
    # "icu_event"          ,
    # "pneum_event"        ,
    # "sever_event"     
  )
  
  voi <- c(
    "Age"            ,
    "Female sex"         ,
    "BMI"                ,
    "SMOKE"              ,
    "   Hypertension"            ,
    "   Major depressive disorder"            ,
    "   Haematological disease"            ,
    "   Diabetes"            ,
    "   Cancer"            ,
    # "   HIV infection"            ,
    "   Coronary heart disease"            ,
    "   Cerebrovascular disease"            ,
    "   HBV infection"            ,
    "   Chronic liver disease"            ,
    "   Chronic kidney disease"            ,
    # "PREGN"              ,
    # "SMOKE_TYPE"         ,
    # "ALCOHOL"            ,
    # "ABUSE"              ,
    # "COHAB_cat"              ,
    # "COHAB_CHILD_cat"        ,
    # "COVID_COHAB_cat"        ,
    # "EMPL"               ,
    # "EMPL_OTH"           ,
    # "HEALTHCARE_JOB"     ,
    "MS phenotype"            ,
    "msh_disdur"         ,
    # "msh_disdur2"         ,
    "LAST_EDSS"          ,
    # "TREAT"              ,
    # "LAST_DMD"           ,
    "NAME_DMD"           ,
    # "DMD_OTH"            ,
    # "ONG_DMD"            ,
    # "DMD_INTERR"         ,
    # "DMD_INTER_OTH"      ,
    # "METH_GLUC"          ,
    # "WBC_RESULT"         ,
    # "LYMPH_RESULT"       ,
    "   Fever",
    "   Cough",
    "   Fatigue",
    "   Sputum production",
    "   Sore throat",
    "   Nasal congestion",
    "   Chills",
    "   Loss of taste`",
    "   Loss of smell`",
    "   Lymph nodes enlargement",
    "   Tonsils swelling",
    "   Throat congestion",
    "   Rash"
    # "SIGNSOTH_SP"        ,
    # "EXPOSURE"           ,
    # "WBC"                ,
    # "WBC_VALUE_Covid"    ,
    # "RBC"                ,
    # "RBC_VALUE"          ,
    # "LYMPH"              ,
    # "LYMPH_VALUE_Covid"  ,
    # "PLATELETS"          ,
    # "PLATELETS_VALUE"    ,
    # "HEMOGL"             ,
    # "HEMOGL_VALUE"       ,
    # "RATIO"              ,
    # "RATIO_VALUE"        ,
    # "CHLOROQ"            ,
    # "HYDROX"             ,
    # "IV_ANTIB"           ,
    # "SYST_GLUC"          ,
    # "OXYG_THER"          ,
    # "IVIMGL"             ,
    # "OTHER"              ,
    # "VENT"               ,
    # "VENT_TYPE"          ,
    # "COVID_SEVERITY"     ,
    # "death_event"        ,
    # "hospi_event"        ,
    # "icu_event"          ,
    # "pneum_event"        ,
    # "sever_event"     
    )
  
  nonnorm <- c(
    "msh_disdur",
    "LAST_EDSS",
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
    print(togglePrint = FALSE, nonnorm = nonnorm, 
          varLabels = TRUE, showAllLevels = FALSE) %>%
    {names_ <<- rownames(.); .} %>%
    as_tibble() %>%
    mutate(Characteristic = names_) %>% 
    select(Characteristic, 
           Overall,
           Suspected = `FALSE`, 
           Confirmed = `TRUE`#, `P value` = p
           ) %>% 
    mutate_at(vars(Overall, Confirmed, Suspected), ~ as.character(trimws(.))) %>% 
    mutate(Characteristic = str_replace(Characteristic, " = 1", "")) %>% 
    mutate(Characteristic = str_replace(Characteristic, "(.) \\(mean \\(SD\\)\\)", "\\1 － Mean (SD)")) %>% 
    mutate(Characteristic = str_replace(Characteristic, "= Yes", "")) %>% 
    mutate(Characteristic = gsub(Characteristic, pattern = "(.) .median .IQR..", replacement = "\\1 － Median (IQR)")) %>% 
    mutate(Characteristic = ifelse(Characteristic == "   NA", "   Missing data", Characteristic)) %>% 
    mutate(Characteristic = str_replace(Characteristic, " TRUE", "")) %>% 
    mutate_at(vars(Overall, Confirmed, Suspected), ~ gsub(., pattern = "\\[(.*), (.*)\\]", replacement = "(\\1-\\2)")) %>% 
    mutate_at(vars(Overall, Confirmed, Suspected), ~ gsub(., pattern = "\\.00", replacement = "")) %>% 
    mutate_at(vars(Overall, Confirmed, Suspected), ~ gsub(., pattern = "\\(\\s*(\\S)", replacement = "(\\1")) %>% 
    mutate(Characteristic = gsub(Characteristic, pattern = "= (.*)", replacement = "\\1")) %>% 
    mutate(Characteristic = gsub(Characteristic, pattern = " \\(\\%\\)", replacement = " － no. (%)")) %>% 
    add_row(Characteristic = "Past diagnoses － no. (%)", Overall = "", Confirmed = "", Suspected = "", .after = 13) %>%
    add_row(Characteristic = "Sympthoms － no. (%)", Overall = "", Confirmed = "", Suspected = "", .after = 44) %>%
    {.[1, 1] <- ""; names(.) <- str_replace(paste(names(.), " (N=", .[1,], ")", sep = ""), "\\s+", " "); .} %>% 
    {names(.)[1] <- "Characteristic"; .} %>% 
    .[-1,]
  
  tbl1_db[c(31:44),] <- tbl1_db[c(31:40, 42, 43, 41, 44),]
  
  return(tbl1_db)
}


# generate_table1(ioized) %>% View()
