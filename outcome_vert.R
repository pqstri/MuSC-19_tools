compute_outcomes <- function(f, debug = FALSE, check_original = FALSE) {
  
  require(lubridate)
  
  minimal <- select(f, upid,
                    # age = Demography_Age,
                    # sex = Base_Sex,
                    com_can = `Comorbidity_MALIGNANT TUMOR`,
                    com_dep = `Comorbidity_MAJOR DEPRESSIVE DISORDER`,
                    com_hae = `Comorbidity_HAEMATOLOGICAL DISEASE`,
                    com_hiv = Comorbidity_HIV,
                    com_hbv = Comorbidity_HBV,
                    com_dbt = Comorbidity_DIABETES,
                    com_chd = `Comorbidity_CORONARY HEART DISEASE`,
                    com_cld = `Comorbidity_CHRONIC LIVER DISEASE`,
                    com_ckd = `Comorbidity_CHRONIC KIDNEY DISEASE`,
                    com_hts = Comorbidity_HYPERTENSION,
                    com_cvd = `Comorbidity_CEREBROVASCULAR DISEASE`,
                    `ORIGINAL_Demography_Date of Visit` = `Demography_Date of Visit`,
                    `ORIGINAL_COVID 19 - Sign and symptom_Date of first symptoms` = `COVID 19 - Sign and symptom_Date of first symptoms`,
                    `ORIGINAL_COVID19 - Diagnosis, Treatment_Mechanical ventilation` = `COVID19 - Diagnosis, Treatment_Mechanical ventilation`,
                    `ORIGINAL_COVID19 - Diagnosis, Treatment_If ventilation is mechanical, please specify` = `COVID19 - Diagnosis, Treatment_If ventilation is mechanical, please specify`,
                    `ORIGINAL_COVID19 - Diagnosis, Treatment_Hospitalization` = `COVID19 - Diagnosis, Treatment_Hospitalization`,
                    `ORIGINAL_COVID19 - Diagnosis, Treatment_If patient is hospitalized, please report date of hospitalization` = `COVID19 - Diagnosis, Treatment_If patient is hospitalized, please report date of hospitalization`,
                    `ORIGINAL_COVID 19 - Radiological data_PRESENCE OF PNEUMONIA` = `COVID 19 - Radiological data_PRESENCE OF PNEUMONIA`,
                    `ORIGINAL_COVID 19 - Radiological data_Finding` = `COVID 19 - Radiological data_Finding`, 
                    `ORIGINAL_COVID 19 - Radiological data_Finding ` = `COVID 19 - Radiological data_Finding `, 
                    `ORIGINAL_COVID 19 - Radiological data_Finding_2` = `COVID 19 - Radiological data_Finding_2`, 
                    `ORIGINAL_COVID 19 - Radiological data_Chest CT date` = `COVID 19 - Radiological data_Chest CT date`, 
                    `ORIGINAL_COVID 19 - Radiological data_Chest radiograph date` = `COVID 19 - Radiological data_Chest radiograph date`, 
                    `ORIGINAL_COVID 19 - Radiological data_Chest ultrasound date` = `COVID 19 - Radiological data_Chest ultrasound date`, 
                    `ORIGINAL_COVID19 - Diagnosis, Treatment_Severity (for COVID positive patients)` = `COVID19 - Diagnosis, Treatment_Severity (for COVID positive patients)`, 
                    `ORIGINAL_COVID 19 - Follow-up_Outcome` = `COVID 19 - Follow-up_Outcome`,
                    `ORIGINAL_COVID 19 - Follow-up_Outcome_2` = `COVID 19 - Follow-up_Outcome_2`,
                    `ORIGINAL_COVID 19 - Follow-up_Outcome_3` = `COVID 19 - Follow-up_Outcome_3`,
                    `ORIGINAL_COVID 19 - Follow-up_Outcome_4` = `COVID 19 - Follow-up_Outcome_4`,
                    `ORIGINAL_COVID 19 - Follow-up_Outcome_5` = `COVID 19 - Follow-up_Outcome_5`,
                    `ORIGINAL_COVID 19 - Follow-up_Outcome_6` = `COVID 19 - Follow-up_Outcome_6`,
                    `ORIGINAL_COVID 19 - Follow-up_Hospitalized in Intensive Care Unit (ICU)` = `COVID 19 - Follow-up_Hospitalized in Intensive Care Unit (ICU)`,
                    `ORIGINAL_COVID 19 - Follow-up_Hospitalized in Intensive Care Unit (ICU)_2` = `COVID 19 - Follow-up_Hospitalized in Intensive Care Unit (ICU)_2`,     
                    `ORIGINAL_COVID 19 - Follow-up_Hospitalized in Intensive Care Unit (ICU)_3` = `COVID 19 - Follow-up_Hospitalized in Intensive Care Unit (ICU)_3`,
                    `ORIGINAL_COVID 19 - Follow-up_Hospitalized in Intensive Care Unit (ICU)_4` = `COVID 19 - Follow-up_Hospitalized in Intensive Care Unit (ICU)_4`,
                    `ORIGINAL_COVID 19 - Follow-up_Hospitalized in Intensive Care Unit (ICU)_5` = `COVID 19 - Follow-up_Hospitalized in Intensive Care Unit (ICU)_5`,
                    `ORIGINAL_COVID 19 - Follow-up_If hospitalized in Intensive Care Unit (ICU), date` = `COVID 19 - Follow-up_If hospitalized in Intensive Care Unit (ICU), date`,
                    `ORIGINAL_COVID 19 - Follow-up_Hospitalized since previous contact` = `COVID 19 - Follow-up_Hospitalized since previous contact`,
                    `ORIGINAL_COVID 19 - Follow-up_Hospitalized since previous contact_2` = `COVID 19 - Follow-up_Hospitalized since previous contact_2`,
                    `ORIGINAL_COVID 19 - Follow-up_Hospitalized since previous contact_3` = `COVID 19 - Follow-up_Hospitalized since previous contact_3`,
                    `ORIGINAL_COVID 19 - Follow-up_Hospitalized since previous contact_4` = `COVID 19 - Follow-up_Hospitalized since previous contact_4`,
                    `ORIGINAL_COVID 19 - Follow-up_Hospitalized since previous contact_5` = `COVID 19 - Follow-up_Hospitalized since previous contact_5`,
                    `ORIGINAL_COVID 19 - Follow-up_Hospitalized since previous contact_6` = `COVID 19 - Follow-up_Hospitalized since previous contact_6`,
                    `ORIGINAL_COVID 19 - Follow-up_If hospitalized, date` = `COVID 19 - Follow-up_If hospitalized, date`,
                    `ORIGINAL_COVID 19 - Follow-up_If hospitalized, date_2` = `COVID 19 - Follow-up_If hospitalized, date_2`,
                    `ORIGINAL_COVID 19 - Follow-up_If hospitalized, date_4` = `COVID 19 - Follow-up_If hospitalized, date_4`,
                    `ORIGINAL_COVID 19 - Follow-up_If hospitalized, date_5` = `COVID 19 - Follow-up_If hospitalized, date_5`,
                    `ORIGINAL_COVID 19 - Follow-up_Cause of death` = `COVID 19 - Follow-up_Cause of death`,
                    `ORIGINAL_COVID 19 - Follow-up_In case of death, report date` = `COVID 19 - Follow-up_In case of death, report date`,
                    `ORIGINAL_COVID 19 - Follow-up_In case of death, report date_2` = `COVID 19 - Follow-up_In case of death, report date_2`,
                    `ORIGINAL_COVID 19 - Follow-up_Date of Visit (or mail or telephone contact)` = `COVID 19 - Follow-up_Date of Visit (or mail or telephone contact)`,
                    `ORIGINAL_COVID 19 - Follow-up_Date of Visit (or mail or telephone contact)_2` = `COVID 19 - Follow-up_Date of Visit (or mail or telephone contact)_2`,
                    `ORIGINAL_COVID 19 - Follow-up_Date of Visit (or mail or telephone contact)_3` = `COVID 19 - Follow-up_Date of Visit (or mail or telephone contact)_3`,
                    `ORIGINAL_COVID 19 - Follow-up_Date of Visit (or mail or telephone contact)_4` = `COVID 19 - Follow-up_Date of Visit (or mail or telephone contact)_4`,
                    `ORIGINAL_COVID 19 - Follow-up_Date of Visit (or mail or telephone contact)_5` = `COVID 19 - Follow-up_Date of Visit (or mail or telephone contact)_5`,
                    `ORIGINAL_COVID 19 - Follow-up_Date of Visit (or mail or telephone contact)_6` = `COVID 19 - Follow-up_Date of Visit (or mail or telephone contact)_6`,
                    `ORIGINAL_COVID 19 - Follow-up_If discharged, date` = `COVID 19 - Follow-up_If discharged, date`,
                    `ORIGINAL_COVID 19 - Follow-up_If discharged, date_2` = `COVID 19 - Follow-up_If discharged, date_2`,
                    `ORIGINAL_COVID 19 - Follow-up_If discharged, date_3` = `COVID 19 - Follow-up_If discharged, date_3`,
                    `ORIGINAL_COVID 19 - Follow-up_If discharged, date_4` = `COVID 19 - Follow-up_If discharged, date_4`,
                    `ORIGINAL_COVID 19 - Follow-up_If recovered, report date` = `COVID 19 - Follow-up_If recovered, report date`,
                    `ORIGINAL_COVID 19 - Follow-up_If recovered, report date_2` = `COVID 19 - Follow-up_If recovered, report date_2`,
                    `ORIGINAL_COVID 19 - Follow-up_If recovered, report date_3` = `COVID 19 - Follow-up_If recovered, report date_3`,
                    `ORIGINAL_COVID 19 - Follow-up_If recovered, report date_4` = `COVID 19 - Follow-up_If recovered, report date_4`,
                    `ORIGINAL_COVID 19 - Follow-up_Presence of Pneumonia` = `COVID 19 - Follow-up_Presence of Pneumonia`,
                    `ORIGINAL_COVID 19 - Follow-up_Presence of Pneumonia_2` = `COVID 19 - Follow-up_Presence of Pneumonia_2`,
                    `ORIGINAL_COVID 19 - Follow-up_Presence of Pneumonia_3` = `COVID 19 - Follow-up_Presence of Pneumonia_3`,
                    `ORIGINAL_COVID 19 - Follow-up_Presence of Pneumonia_4` = `COVID 19 - Follow-up_Presence of Pneumonia_4`,
                    `ORIGINAL_COVID 19 - Follow-up_Presence of Pneumonia_5` = `COVID 19 - Follow-up_Presence of Pneumonia_5`,
                    `ORIGINAL_COVID 19 - Follow-up_Presence of Pneumonia_6` = `COVID 19 - Follow-up_Presence of Pneumonia_6`,
                    `ORIGINAL_COVID 19 - Follow-up_Severity of COVID` = `COVID 19 - Follow-up_Severity of COVID`,
                    `ORIGINAL_COVID 19 - Follow-up_Severity of COVID_2` = `COVID 19 - Follow-up_Severity of COVID_2`,
                    `ORIGINAL_COVID 19 - Follow-up_Severity of COVID_3` = `COVID 19 - Follow-up_Severity of COVID_3`,
                    `ORIGINAL_COVID 19 - Follow-up_Severity of COVID_4` = `COVID 19 - Follow-up_Severity of COVID_4`,
                    # msh_tp_bin  = `MS history_In Treatment`,
                    # mdh_tp_line = `MS history_If in treatment, Type of DMD`,
                    # mdh_tp_name = `MS history_If in treatment, Name of DMD`,
                    msh_disdur  = `MS history_Date of MS Diagnosis`,) %>% 
    mutate(msh_disdur = (dmy("1/4/2020") %--% dmy(msh_disdur)) / years(1)) %>% 
    mutate_if(is.character, ~ ifelse(. == "", NA, .))
    
  # tools
  is_datable <- function(x) {
    
    if (class(x) == "Date") { return(FALSE) }
    
    # threshold percentage
    threshold <- 0.2
    suppressWarnings(dmyzed <- lubridate::dmy(x))
    mean(!is.na(dmyzed)) > threshold
  }
  
  # base
  base <- select(f, upid,
                fsympt_date = `COVID 19 - Sign and symptom_Date of first symptoms`,
                fvisit_date = `Demography_Date of Visit`) 
  
  base_vert <- select(base,  upid, `first sympthom` = fsympt_date, visit_date = fvisit_date) %>% 
    gather("update", "date", -upid, -visit_date)
  
  # one-shot info hosp and icu
  osi <- select(f, upid,
                visit_date = `Demography_Date of Visit`,
                vent_mech = `COVID19 - Diagnosis, Treatment_Mechanical ventilation`,
                vent_type = `COVID19 - Diagnosis, Treatment_If ventilation is mechanical, please specify`,
                hosp_yn = `COVID19 - Diagnosis, Treatment_Hospitalization`,
                date = `COVID19 - Diagnosis, Treatment_If patient is hospitalized, please report date of hospitalization`) %>% 
    
    # define ventilation
    mutate(ventilation = vent_mech == "Yes" | vent_type == "Invasive") %>% 
    unite(details, c(vent_mech, vent_type), sep = " mechanical ventilation; type: ") %>% 
    
    # define hosp
    mutate(hospitalization = hosp_yn == "Yes" | !is.na(date) | ventilation,
           icu = hospitalization & ventilation) %>% 
    
    # clean used var
    select(-hosp_yn, -ventilation) %>% 
    
    # verticalize and clean
    gather("update", "litter", hospitalization, icu) %>% 
    filter(litter) %>% 
    select(-litter)
    
  # one-shot info pneumoni
  imag <- select(f, upid, pneu_yn = `COVID 19 - Radiological data_PRESENCE OF PNEUMONIA`,
                 matches("^COVID 19 - Radiological.*date$"),
                 matches("^COVID 19 - Radiological.*finding"),
                 visit_date = `Demography_Date of Visit`,
                 contains("Treatment_Severity"),
                 -contains("further")) %>%
    
    # gether imaging tech
    rename(ct_res  = `COVID 19 - Radiological data_Finding`, 
           rx_res  = `COVID 19 - Radiological data_Finding `, 
           us_res  = `COVID 19 - Radiological data_Finding_2`, 
           ct_date = `COVID 19 - Radiological data_Chest CT date`, 
           rx_date = `COVID 19 - Radiological data_Chest radiograph date`,
           us_date = `COVID 19 - Radiological data_Chest ultrasound date`,
           severity = `COVID19 - Diagnosis, Treatment_Severity (for COVID positive patients)`) %>%
    unite(ct, ct_res, ct_date, sep = "_") %>% 
    unite(rx, rx_res, rx_date, sep = "_") %>% 
    unite(us, us_res, us_date, sep = "_") %>% 
    gather("img_tech", "res_date", -upid, -pneu_yn, -visit_date, -severity) %>% 
    
    # clean
    separate(res_date, into = c("img_res", "date"), sep = "_") %>% 
    mutate_all(function(x) {ifelse(trimws(x) == "" | x == "NA", NA, x)}) %>% 
    
    # keep relevant only
    filter(pneu_yn == "Yes" | !(is.na(date) & is.na(img_res))) %>% 
    
    # filter out empty tech for pneumonia cases
    group_by(upid) %>% 
    mutate(any_date = any(!is.na(date))) %>% 
    filter(!(any_date == TRUE & pneu_yn == "Yes" & is.na(date))) %>% 
    filter(!(pneu_yn == "Yes" & img_res == "Normal")) %>% 
    ungroup() %>% 
    
    # define pneumonia outcome
    mutate(update = if_else(img_res == "Abnormal" | pneu_yn == "Yes", 
                            "pneumonia", "imaging", "imaging")) %>% 
    
    # create severe pneumonia outcome
    mutate(multiplier = if_else(severity == "Critical" | severity == "Severe", 2, 1, 1)) %>% 
    uncount(multiplier) %>% 
    group_by(upid, date, img_tech) %>% 
    mutate(update = ifelse(row_number() > 1, "severe pneumonia", update)) %>% 
    ungroup() %>% 
    
    # clean
    mutate(details = sprintf("Pneumonia explicit: %s; Imaging at %s: %s; severity: %s", 
                             pneu_yn, img_tech, img_res, severity)) %>% 
    select(-img_res, -pneu_yn, -img_tech, -any_date, - severity)
  
  
  # repeated measurements info
  
  # set parms
  fixed_non_missing <- 2 # (upid, order)
  
  # pick vars
  rmi <- select(f, upid, 
                contains("outcome"),
                contains("ICU"),
                matches("Follow.*hosp"),
                contains("death"),
                matches("follow.*date"), 
                matches("follow.*pneumonia"),
                -contains("updated"),
                -contains("created"),
                matches("follow.*Seve")) %>% 
    
    # verticalize and clean names
    gather("var", "val", -upid) %>% 
    mutate(var = str_remove(var, "^.*?_")) %>% 
    separate(var, c("var", "order"), sep = "_") %>% 
    mutate(order = ifelse(is.na(order), 1, order)) %>% 
    
    # re-horizontalize per type but vert per order
    spread(var, val) %>% 
    
    # rename
    rename(visit_date = `Date of Visit (or mail or telephone contact)`) %>% 
    
    # eliminate empty rows
    filter(rowSums(is.na(.)) < ncol(.) - fixed_non_missing)
  
  
  # Deaths
  # set parms
  fixed_non_missing <- 2 # (upid, visit_date)
  
  death_db <- select(rmi, upid, visit_date, contains("death"), Outcome) %>% 
    
    mutate(Outcome = ifelse(Outcome == "Death", "Death", NA)) %>% 
    
    # rename
    rename(date = `In case of death, report date`, 
           death = contains("cause")) %>%
    filter(rowSums(!is.na(.)) > fixed_non_missing) %>%
    mutate(date = ifelse(is.na(date) & is_datable(death), death, date)) %>% 
    
    # verticalize and clean
    gather("update", "details", death) %>% 
    select(-Outcome)
  
  # Hosp
  # set parms
  fixed_non_missing <- 2 # (upid, visit_date)
  
  hosp_db <- select(rmi, upid, contains("hosp"), -contains("icu"), visit_date) %>% 
    rename(hospitalization = `Hospitalized since previous contact`,
           date =`If hospitalized, date`) %>% 
    filter(!is.na(date) | hospitalization == "Yes")  %>% 
    
    # verticalize
    gather("update", "details", hospitalization)
  
  # icu
  # set parms
  fixed_non_missing <- 2 # (upid, visit_date)
  
  icu_db <- select(rmi, upid, contains("icu"), visit_date) %>% 
    rename(icu = `Hospitalized in Intensive Care Unit (ICU)`,
           date = `If hospitalized in Intensive Care Unit (ICU), date`) %>% 
    filter(!is.na(date) | icu == "Yes")  %>% 
    
    # verticalize
    gather("update", "details", icu) 
  
  
  pneum_db <- select(rmi, upid, pneumonia = `Presence of Pneumonia`, 
         severity = `Severity of COVID`, visit_date) %>% 
    mutate(date = visit_date) %>% 
    gather("update", "val", pneumonia) %>% 
    filter(severity == "Severe" | severity == "Critical" | val == "Yes") %>%
    unite("details", val, severity, sep = " explicit pneumonia with severity: ")
  
  sev_db <- select(rmi, upid, `severe pneumonia` = `Presence of Pneumonia`, 
                     severity = `Severity of COVID`, visit_date) %>% 
    mutate(date = visit_date) %>% 
    gather("update", "val", `severe pneumonia`) %>% 
    filter(severity == "Severe" | severity == "Critical") %>%
    unite("details", val, severity, sep = " explicit pneumonia with severity: ")
  
  
  last_fup_vert <- select_if(f, is_datable) %>% 
    bind_cols(select(f, upid)) %>% 
    gather("last contact", "date", -upid) %>% 
    gather("update", "details", `last contact`) %>% 
    mutate(date = lubridate::dmy(date)) %>% 
    filter(!is.na(date)) %>% 
    group_by(upid) %>% 
    arrange(date) %>% 
    filter(row_number() == n()) %>% 
    ungroup() %>% 
    left_join(transmute(rmi, upid, date = lubridate::dmy(visit_date), details2 = Outcome)) %>% 
    mutate(details = ifelse(is.na(details2), details, paste(details2, details, sep = "-"))) %>% 
    select(-details2)
  
  last_fup <- select(last_fup_vert, upid, last_contact = date)
  
  basic_db <- full_join(base, last_fup)
  
  
  outcome_dbs <- list(death_db, icu_db, sev_db, pneum_db, hosp_db, imag, osi)
  all_dbs <- c(outcome_dbs, list(base_vert, last_fup_vert))
  
  purrr::map_int(outcome_dbs, ~ dim(.)[1]) %>% sum()
  nrow(base_vert)
  
  
  all_vert <- purrr::map(all_dbs, ~ mutate_if(., is_datable, lubridate::dmy)) %>% 
    purrr::reduce(bind_rows) %>% 
    arrange(upid, date) %>% 
    mutate_if(is_datable, lubridate::dmy)
  
  mixed <- purrr::reduce(outcome_dbs, bind_rows) %>% 
    mutate_if(is_datable, lubridate::dmy) %>% 
    arrange(upid, date) %>% 
    full_join(basic_db)
  
  output <- select(mixed, -details) %>% 
    group_by(upid, update) %>% 
    arrange(date) %>% 
    summarise(fsympt_date  = first(fsympt_date), 
              fvisit_date  = first(fvisit_date),
              last_contact = last(last_contact),
              last = last(update), date = last(date)) %>% 
    ungroup() %>% 
    select(-update) 
  
  output_date <- output %>% 
    spread(last, date) %>% 
    select(-imaging, -`<NA>`) %>% 
    filter(!is.na(fsympt_date)) %>% 
    mutate_if(is_datable, lubridate::dmy)
  
  output_event <- output %>% 
    mutate(event = 1) %>% 
    mutate(last = paste(last, "event", sep = "_")) %>% 
    select(-date) %>% 
    spread(last, event) %>% 
    select(-imaging_event, -NA_event) %>% 
    filter(!is.na(fsympt_date)) %>% 
    mutate_if(is_datable, lubridate::dmy)
  
  output <- full_join(output_event, output_date)
  
  require(lubridate)
  outcomes <- output %>% 
    mutate(fsympt_date = case_when(fsympt_date < lubridate::dmy("1-09-2019") ~ fsympt_date + years(1), TRUE ~ fsympt_date)) %>% 
    
    # death
    mutate(death_event = ifelse(!is.na(death) | !is.na(death_event), 1, 0),
           death_time  = case_when(!is.na(death)        ~ (fsympt_date %--% death)/days(1),
                                   !is.na(last_contact) ~ (fsympt_date %--% last_contact)/days(1),
                                   TRUE                 ~ -9),
           death_time = ifelse(death_time <= 0 | is.na(death_time), 
                               median(death_time, na.rm = TRUE), death_time)) %>% 
    select(everything(), -death, death_date = death) %>% 
    
    # `severe pneumonia`
    rename(sever_event = `severe pneumonia_event`) %>% 
    mutate(sever_event = ifelse(!is.na(`severe pneumonia`) | !is.na(sever_event), 1, 0),
           sever_time  = case_when(!is.na(`severe pneumonia`) ~ (fsympt_date %--% `severe pneumonia`)/days(1),
                                   !is.na(last_contact)   ~ (fsympt_date %--% last_contact)/days(1),
                                   TRUE                   ~ -9),
           sever_time = ifelse(sever_time <= 0 | is.na(sever_time), median(sever_time, na.rm = TRUE), sever_time)) %>% 
    select(everything(), -`severe pneumonia`, sever_date = `severe pneumonia`) %>% 
  
    # pneumonia
    rename(pneum_event = pneumonia_event) %>% 
    mutate(pneum_event = ifelse(!is.na(pneumonia) | !is.na(pneum_event), 1, 0),
           pneum_time  = case_when(!is.na(pneumonia) ~ (fsympt_date %--% pneumonia)/days(1),
                                  !is.na(last_contact)   ~ (fsympt_date %--% last_contact)/days(1),
                                  TRUE                   ~ -9),
           pneum_time = ifelse(pneum_time <= 0 | is.na(pneum_time), median(pneum_time, na.rm = TRUE), pneum_time)) %>% 
    select(everything(), -pneumonia, pneum_date = pneumonia) %>% 
    
    # hospi
    rename(hospi_event = hospitalization_event) %>% 
    mutate(hospi_event = ifelse(!is.na(hospitalization) | !is.na(hospi_event), 1, 0),
           hospi_time  = case_when(!is.na(hospitalization) ~ (fsympt_date %--% hospitalization)/days(1),
                                   !is.na(last_contact)   ~ (fsympt_date %--% last_contact)/days(1),
                                   TRUE                   ~ -9),
           hospi_time = ifelse(hospi_time <= 0 | is.na(hospi_time), median(hospi_time, na.rm = TRUE), hospi_time)) %>% 
    select(everything(), -hospitalization, hospi_date = hospitalization) %>% 
      
    # icu
    mutate(icu_event = ifelse(!is.na(icu) | !is.na(icu_event), 1, 0),
           icu_time  = case_when(!is.na(icu) ~ (fsympt_date %--% icu)/days(1),
                                   !is.na(last_contact)   ~ (fsympt_date %--% last_contact)/days(1),
                                   TRUE                   ~ -9),
           icu_time = ifelse(icu_time <= 0 | is.na(icu_time), median(icu_time, na.rm = TRUE), icu_time)) %>% 
    select(everything(), -icu, icu_date = icu) %>% 
    
    # fix inconsist imputation
    mutate(icu_time   = ifelse(icu_time   > death_time, death_time, icu_time)) %>% 
    mutate(hospi_time = ifelse(hospi_time > icu_time,   icu_time,   hospi_time)) %>% 
    mutate(pneum_time = ifelse(pneum_time > sever_time, sever_time, pneum_time)) %>% 
  
    # composite 2
    mutate(composite2_event = ifelse(icu_event == 1 | death_event == 1, 1, 0)) %>% 
    rowwise() %>% 
    mutate(composite2_time  = ifelse(composite2_event == 1, 
                                 min(icu_time, death_time),
                                 max(icu_time, death_time))) %>% 
    
    # composite 3
    mutate(composite3_event = ifelse(icu_event == 1 | 
                                       death_event == 1 |
                                       sever_event == 1, 1, 0)) %>% 
    rowwise() %>% 
    mutate(composite3_time  = case_when(icu_event   == 1 & sever_event == 1 & death_event == 1 ~ min(icu_time, death_time, sever_time),
                                        icu_event   == 1 & sever_event == 1 ~ min(icu_time, sever_time),
                                        icu_event   == 1 & death_event == 1 ~ min(icu_time, death_time),
                                        sever_event == 1 & death_event == 1 ~ min(death_time, sever_time),
                                        icu_event   == 1 ~ icu_time,
                                        sever_event == 1 ~ sever_time,
                                        death_event == 1 ~ death_time, 
                                        TRUE ~ max(icu_time, death_time, sever_time))) %>% 
    
    # composite 4
    mutate(composite4_event = ifelse(icu_event == 1 | 
                                       death_event == 1 |
                                       sever_event == 1 |
                                       pneum_event == 1, 1, 0)) %>% 
    rowwise() %>% 
    mutate(composite4_time  = case_when(pneum_event == 1 & icu_event   == 1 & sever_event == 1 & death_event == 1 ~ min(icu_time, death_time, sever_time, pneum_time),
                                        icu_event   == 1 & sever_event == 1 & death_event == 1 ~ min(icu_time, death_time, sever_time),
                                        pneum_event == 1 & sever_event == 1 & death_event == 1 ~ min(death_time, sever_time, pneum_time),
                                        pneum_event == 1 & icu_event   == 1 & death_event == 1 ~ min(icu_time, death_time, pneum_time),
                                        pneum_event == 1 & icu_event   == 1 & sever_event == 1 ~ min(icu_time, sever_time, pneum_time),
                                        sever_event == 1 & death_event == 1 ~ min(death_time, sever_time),
                                        icu_event   == 1 & death_event == 1 ~ min(icu_time, death_time),
                                        icu_event   == 1 & sever_event == 1 ~ min(icu_time, sever_time),
                                        pneum_event == 1 & death_event == 1 ~ min(icu_time, sever_time),
                                        pneum_event == 1 & sever_event == 1 ~ min(sever_time, pneum_time),
                                        pneum_event == 1 & icu_event   == 1  ~ min(icu_time, pneum_time),
                                        death_event == 1 ~ death_time, 
                                        icu_event   == 1 ~ icu_time,
                                        sever_event == 1 ~ sever_time,
                                        pneum_event == 1 ~ pneum_time,
                                        TRUE ~ max(icu_time, death_time, sever_time, pneum_time))) %>% 
    
    # composite 5
    mutate(composite5_event = ifelse(hospi_event == 1 | composite4_event == 1, 1, 0)) %>% 
    rowwise() %>% 
    mutate(composite5_time  = case_when(composite4_event == 1 & hospi_event   == 1 ~ min(composite4_time, hospi_time),                       sever_event == 1 & death_event == 1 ~ min(death_time, sever_time),
                                        hospi_event   == 1 ~ hospi_time,
                                        composite4_event == 1 ~ composite4_time,
                                        TRUE ~ max(icu_time, death_time, sever_time, pneum_time)))
  
  outAndCovs <- left_join(minimal, outcomes)
  
  if (debug) {
    return(list(check_original = outAndCovs, events = all_vert))
  }
  if (check_original) {
    return(outAndCovs)
  }
  return(select(outAndCovs, -contains("ORIGINAL")))
  
  # openxlsx::write.xlsx(outcomes, "~/Downloads/outcomes.xlsx")
  
  # select(outcomes, matches("composite._event")) %>% 
  #   gather() %>% 
  #   table() %>% 
  #   addmargins()
  
  # openxlsx::write.xlsx(output, "~/Downloads/date.xlsx")
  # openxlsx::write.xlsx(all_vert, "~/Downloads/verifica.xlsx")
  # 
  # ggVennDiagram::ggVennDiagram(list(
  #   death              = mixed[mixed$update == "death",]$upid,
  #   hospitalization    = mixed[mixed$update == "hospitalization",]$upid,
  #   `severe pneumonia` = mixed[mixed$update == "severe pneumonia",]$upid,
  #   icu                = mixed[mixed$update == "icu",]$upid)
  #   # pneumonia          = mixed[mixed$update == "pneumonia",]$upid,
  #   # imaging            = mixed[mixed$update == "imaging",]$upid,
  #   # none               = mixed[is.na(mixed$update,]$upid)
  #   ) + 
  #   guides(fill = FALSE)

}




