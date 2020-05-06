compute_outcomes <- function(f) {
  
  #############################
  # Base DBs
  #############################
  
  cat("finding first nad last contacts...\n")
  
  # select useful variables
  fup_db <- select(f, upid, provider = Base_Provider, contains("Follow-up_Date of Visit"), 
                   fsympt_date = `COVID 19 - Sign and symptom_Date of first symptoms`,
                   fvisit_date = `Demography_Date of Visit`) %>%
    
    # impute first contact if missing
    mutate(fsympt_date = ifelse(is.na(fsympt_date), fvisit_date, fsympt_date)) %>% 

    # verticalize
    gather("time", "fup_date", -upid, -fsympt_date, -provider, -fvisit_date) %>% 
    
    # cast appropriate data format
    mutate_at(vars(ends_with("_date")), lubridate::dmy) %>%
    
    # for each patient
    group_by(upid, provider) %>% 
    
    # select the first contact and the latest of the other contacts
    summarise(fsympt_date = first(fsympt_date),
              fvisit_date = first(fvisit_date),
              last_fup = lubridate::ymd(max(fup_date, na.rm = TRUE))) %>% 
    
    # if no other contacts the latest is equal to the first
    mutate(last_fup = case_when(is.na(last_fup) ~ fsympt_date, TRUE ~ last_fup)) %>% 
    
    # clean
    filter(!is.na(fsympt_date))
  
  # group all info containing dates
  # dates_db <- select(f, -contains("_Updated"), -contains("_Created")) %>%
  #   mutate_all(~ifelse(trimws(.) == "", NA, .)) %>%
  #   mutate_at(vars(-upid), lubridate::dmy) %>%
  #   select_if(~!all(is.na(.)))
  
  dates_db <- select(f, matches("follow.*date"), 
                     `COVID 19 - Sign and symptom_Date of first symptoms`, 
                     `Demography_Date of Visit`)
  
  # age, comorbidities, DMT, EDSS
  require(lubridate)
  minimal <- select(f, upid,
                    # age = Demography_Age,
                    # sex = Base_Sex,
                    # com_can = `Comorbidity_MALIGNANT TUMOR`,
                    # com_dep = `Comorbidity_MAJOR DEPRESSIVE DISORDER`,
                    # com_hae = `Comorbidity_HAEMATOLOGICAL DISEASE`,
                    # com_hiv = Comorbidity_HIV,
                    # com_hbv = Comorbidity_HBV,
                    # com_dbt = Comorbidity_DIABETES,
                    # com_chd = `Comorbidity_CORONARY HEART DISEASE`,
                    # com_cld = `Comorbidity_CHRONIC LIVER DISEASE`,
                    # com_ckd = `Comorbidity_CHRONIC KIDNEY DISEASE`,
                    # com_hts = Comorbidity_HYPERTENSION,
                    # com_cvd = `Comorbidity_CEREBROVASCULAR DISEASE`,
                    # msh_tp_bin  = `MS history_In Treatment`,
                    # mdh_tp_line = `MS history_If in treatment, Type of DMD`,
                    # mdh_tp_name = `MS history_If in treatment, Name of DMD`,
                    msh_disdur  = `MS history_Date of MS Diagnosis`,) %>% 
    mutate(msh_disdur = (dmy("1/4/2020") %--% dmy(msh_disdur)) / years(1)) %>% 
    mutate_if(is.character, ~ ifelse(. == "", NA, .))
  
  #############################
  # Outcome components
  #############################
  ##########
  # Death
  ##########
  
  cat("finding deaths...\n")
  
  # select fup outcomes
  death <- select(f, upid, contains("outcome"), 
                     death_date = `COVID 19 - Follow-up_In case of death, report date`) %>%
    
    # verticalize and clean
    gather("time", "outcome", -upid, -death_date) %>% 
    mutate(outcome = outcome == "Death",
           death_date = ifelse(death_date == "", NA, death_date)) %>%
    
    # for each patient
    group_by(upid) %>% 
    
    # set event=1 if any death event in fup, and take first death date, 
    # flag an error if there are multiple death dates
    summarise(death_event = any(outcome), 
              death_time = first(na.omit(death_date)),
              death_errorFlag = length(unique(death_date)) != 1) %>% 
    
    # merge to viste dates
    left_join(fup_db) %>% 
    filter(!is.na(fsympt_date)) %>% 
    
    # set missing event date to last fup
    mutate(death_date = case_when(!is.na(death_time) & death_event == 1  ~ lubridate::dmy(death_time), TRUE ~ last_fup),
           
           # count days from first symptom to event
           death_time = death_date - fsympt_date,
           
           # set who do not have events to event = 0
           death_event = ifelse(death_event & !is.na(death_event), 1, 0)) %>% 
    
    # clean
    select(upid, death_event, death_time)
  
  ##########
  # ICU
  ##########
  
  cat("finding icu...\n")
  
  # check if event already occurred at time 0
  icu_baseline <- select(f, upid, 
           mech = `COVID19 - Diagnosis, Treatment_Mechanical ventilation`,
           type = `COVID19 - Diagnosis, Treatment_If ventilation is mechanical, please specify`) %>%
    
    # event = 1 if meccanical or invasive ventilation
    mutate(t0event = mech == "Yes" | type == "Invasive") %>% 
    
    # clean
    select(upid, t0event)

  
  # search in follow-up events
  icu_fup <- select(f, upid, contains("ICU"), matches("follow.*Date of Visit")) %>% 
    
    # verticalize
    gather("var", "val", -upid) %>% 
    
    # clean var meaning
    mutate(var = str_remove(var, "^.*?_")) %>% 
    separate(var, c("var", "order"), sep = "_") %>% 
    mutate(order = ifelse(is.na(order), 1, order)) %>% 
    
    # horizonalize only types 
    spread(var, val) %>% 
    
    # take only events
    filter(`Hospitalized in Intensive Care Unit (ICU)` == "Yes") %>% 
    
    # rename
    select(upid, event_date = contains(", date"), 
           fup_date = contains("Date of visit"), 
           fup_event = `Hospitalized in Intensive Care Unit (ICU)`) %>% 
    
    # cast types
    mutate(fup_event = fup_event == "Yes",
           event_date = lubridate::dmy(event_date),
           fup_date = lubridate::dmy(fup_date))
    
    # TODO: avoid duplicates?

  
  # merge fup to baseline to dates
  icu <- purrr::reduce(list(fup_db, icu_fup, icu_baseline), full_join) %>% 

    # event=1 if baseline or fup event
    mutate(icu_event = as.numeric(fup_event | t0event),
           
           # otherwise event=0
           icu_event = replace_na(icu_event, 0),
           
           # date is first symptom (?) if event at baseline, 
           # event date if fup event and present, fupdate if missing
           # otherwise last fup
           icu_date = case_when(t0event == 1 ~ fsympt_date,
             icu_event == 1 & !is.na(event_date) ~ event_date,
             icu_event == 1 & !is.na(fup_date) ~ fup_date,
             TRUE ~ last_fup)) %>% 
    mutate(icu_time = icu_date - fsympt_date) %>% 
    group_by(upid) %>% 
    summarise(icu_event = first(icu_event), 
              icu_time = first(icu_time)) %>% 
    select(upid, icu_event, icu_time)
  
  ##########
  # Hosp
  ##########
  cat("finding hospitalization...\n")
  hosp_baseline <- select(f, upid, 
                         t0event = `COVID19 - Diagnosis, Treatment_Hospitalization`,
                         t0date = `COVID19 - Diagnosis, Treatment_If patient is hospitalized, please report date of hospitalization`) %>%
    mutate(t0event = t0event == "Yes",
           t0date = lubridate::dmy(t0date))
  
  hosp_fup <- f %>% 
    select(upid, matches("Follow.*hosp"), -contains("icu")) %>% 
    gather("var", "val", -upid) %>% 
    mutate(var = str_remove(var, "^.*?_")) %>% 
    separate(var, c("var", "order"), sep = "_") %>% 
    mutate(order = ifelse(is.na(order), 1, order)) %>% 
    spread(var, val) %>%
    select(upid, fup_date = contains("date"), 
           fup_event = `Hospitalized since previous contact`) %>% 
    mutate(fup_event = fup_event == "Yes",
           fup_date = lubridate::dmy(fup_date)) %>%
    group_by(upid) %>% 
    summarise(fup_event = any(fup_event),
              fup_date = min(fup_date, na.rm = TRUE))
  
  hosp <- purrr::reduce(list(fup_db, hosp_fup, hosp_baseline), left_join) %>% 
    mutate(hosp_event = as.numeric(fup_event | t0event),
           hosp_event = replace_na(hosp_event, 0),
           hosp_date = case_when(t0event & fup_event & !is.na(fup_date) & !is.na(t0date) ~ min(t0date, fup_date, na.rm = TRUE),
                                 t0event & !is.na(t0date) ~ t0date,
                                 t0event ~ fsympt_date,
                                hosp_event == 1 & !is.na(fup_date) ~ fup_date,
                                hosp_event == 1 & !is.na(fup_date) ~ fup_date,
                                TRUE ~ last_fup)) %>% 
    mutate(hosp_time = hosp_date - fsympt_date) %>% 
    select(upid, hosp_event, hosp_time)
  
  ##########
  # Pneumonia
  ##########
  cat("finding pneumonia...\n")
  pneumonia_baseline <- select(f, upid, pneumonia = `COVID 19 - Radiological data_PRESENCE OF PNEUMONIA`,
                               matches("^COVID 19 - Radiological.*date$"),
                               matches("^COVID 19 - Radiological.*finding"),
                               -contains("further")) %>%
    # gether imaging tech
    rename(ct_res  = 3, rx_res  = 4, us_res  = 5, 
           ct_date = 6, rx_date = 7, us_date = 8) %>%
    unite(ct, ct_res, ct_date, sep = "_") %>% 
    unite(rx, rx_res, rx_date, sep = "_") %>% 
    unite(us, us_res, us_date, sep = "_") %>% 
    gather("tech", "date_res", -upid, -pneumonia) %>% 
    
    # clean
    separate(date_res, into = c("date", "res"), sep = "_") %>% 
    mutate_all(function(x) {ifelse(trimws(x) == "" | x == "NA", NA, x)}) %>% 
    
    # add baseline date
    left_join(fup_db) %>% 
    
    group_by(upid) %>% 
    
    # get date of abnormality detection or any exam in pneumonia
    mutate(date = dmy(date),
           img_date = case_when(any(!is.na(date)) ~ first(na.omit(date)), 
                                 any(pneumonia == "Yes") ~ min(date, na.rm = TRUE),
                                 TRUE ~ max(date, na.rm = TRUE))) %>% 
    
    ungroup() %>% 
    
    # look for pneumonia or abnormal
    mutate(t0event = pneumonia == "Yes" | res == "Abnormal")
  
  
  pneumonia_fup <- f %>% 
    select(upid, pneumonia = `COVID 19 - Radiological data_PRESENCE OF PNEUMONIA`,
           matches("presence of pneumonia"), 
           matches("follow.*Date of Visit")) %>% 
    gather("var", "val", -upid, -pneumonia) %>% 
    mutate(var = str_remove(var, "^.*?_")) %>% 
    separate(var, c("var", "order"), sep = "_") %>% 
    mutate(order = ifelse(is.na(order), 1, order)) %>% 
    spread(var, val) %>%
    select(upid, fup_date = contains("date"), 
           fup_event = `Presence of Pneumonia`) %>% 
    mutate(fup_event = fup_event == "Yes",
           fup_date = lubridate::dmy(fup_date))
  
  
  pneumonia <- purrr::reduce(list(fup_db, pneumonia_fup, pneumonia_baseline), left_join) %>% 
    mutate(pneumonia_event = as.numeric(fup_event | t0event),
           pneumonia_event = replace_na(pneumonia_event, 0),
           img_date = ymd(img_date),
           fup_event = replace_na(fup_event, FALSE),
           nodate = is.na(date) & is.na(img_date) & is.na(fup_date),
           pneumonia_date = case_when(t0event & fup_event & !nodate ~ min(date, fup_date, img_date, na.rm = TRUE),
                                      t0event & nodate ~ fsympt_date,
                                 fup_event ~ fup_date,
                                 res == "Abnormal" & !is.na(date) ~ date,
                                 res == "Abnormal" & !is.na(img_date) ~ img_date,
                                 res == "Abnormal" & is.na(img_date) ~ fsympt_date,
                                 t0event & !is.na(img_date) ~ img_date,
                                 t0event & !is.na(img_date) ~ fsympt_date)) %>% 
    group_by(upid) %>% 
    summarise(pneumonia_event = any(pneumonia_event == 1, na.rm = TRUE), 
              fsympt_date = first(fsympt_date),
              pneumonia_date = case_when(pneumonia_event == 1 ~ min(pneumonia_date, na.rm = TRUE),
                                         TRUE ~ first(na.omit(last_fup)))) %>% 
    mutate(pneumonia_time = as.numeric(pneumonia_date - fsympt_date),
           pneumonia_event = as.numeric(pneumonia_event)) %>% 
    select(upid, pneumonia_event, pneumonia_time)
  
  
  # pneumonia <- f %>% 
  #   select(upid, pneumonia = `COVID 19 - Radiological data_PRESENCE OF PNEUMONIA`,
  #          matches("^COVID 19 - Radiological.*date$"),
  #          matches("^COVID 19 - Radiological.*finding"),
  #          -contains("further")) %>% 
  #   
  #   # gether imaging tech
  #   rename(ct_res  = 3, rx_res  = 4, us_res  = 5, 
  #          ct_date = 6, rx_date = 7, us_date = 8) %>%
  #   unite(ct, ct_res, ct_date, sep = "_") %>% 
  #   unite(rx, rx_res, rx_date, sep = "_") %>% 
  #   unite(us, us_res, us_date, sep = "_") %>% 
  #   gather("tech", "date_res", -upid, -pneumonia) %>% 
  #   
  #   # clean
  #   separate(date_res, into = c("date", "res"), sep = "_") %>% 
  #   mutate_all(function(x) {ifelse(trimws(x) == "" | x == "NA", NA, x)}) %>% 
  #   
  #   # look for pneumonia or abnormal
  #   mutate(date = dmy(date),
  #          outcome = pneumonia == "Yes" | res == "Abnormal",
  #          abn_date = hablar::if_else_(res == "Abnormal", date, NA)) %>% 
  #   group_by(upid) %>% 
  #   
  #   # get date of abnormality detection or any exam in pneumonia
  #   mutate(best_date = case_when(any(!is.na(abn_date)) ~ first(na.omit(abn_date)), 
  #                                any(pneumonia == "Yes") ~ min(date, na.rm = TRUE),
  #                                TRUE ~ max(date, na.rm = TRUE))) %>% 
  #   
  #   # shape the outcome
  #   summarise(event = first(outcome),
  #             date = first(best_date)) %>% 
  #   mutate(event = as.numeric(replace_na(event, FALSE))) %>% 
  #   
  #   # coumpute days
  #   left_join(fup_db) %>%
  #   mutate(mixed_date = case_when(event == 1 & !is.na(date) ~ date,
  #                                 event == 1 & is.na(date) ~ dmy("01/01/0000"),
  #                                 event == 0 ~ last_fup)) %>% 
  #   View()
  #   mutate(time = mixed_date - fsympt_date,
  #          time = ifelse(is.infinite(time), NA, time)) %>% 
  #   View()
  #   select(upid, event, time)
  
  
  ##########
  # Severe disease
  ##########
  cat("finding severity...\n")
  sp_baseline <- select(f, upid, date0 = `COVID 19 - Sign and symptom_Date of first symptoms`,
                        severity0 = contains("Treatment_Severity")) %>% 
    mutate(severity0 = factor(severity0, levels = c("Covid negative", "Mild", "Severe", "Critical"), ordered = TRUE))
  
  
  sp_fup <- f %>% 
    select(upid, matches("follow.*Date of Vis"),
           matches("follow.*Seve")) %>% 
    gather("var", "val", -upid) %>% 
    mutate(var = str_remove(var, "^.*?_")) %>% 
    separate(var, c("var", "order"), sep = "_") %>% 
    mutate(order = ifelse(is.na(order), 1, order)) %>% 
    spread(var, val) %>%
    select(upid, fup_date = contains("date"), 
           fup_event = contains("Seve")) %>% 
    mutate(fup_date = lubridate::dmy(fup_date)) %>% 
    # add_row(upid = "Italy-07-01", fup_date = lubridate::dmy("19/03/2020"), fup_event = "Critical") %>% 
    mutate(fup_event = factor(fup_event, levels = c("Covid negative", "Mild", "Severe", "Critical"), ordered = TRUE))
  
  
  sp <- left_join(sp_fup, sp_baseline) %>%
    mutate(event = fup_event > severity0) %>% 
    mutate(date = if_else(event, fup_date, lubridate::dmy(""), lubridate::dmy(""))) %>% 
    group_by(upid) %>% 
    filter(event) %>% 
    summarise(event = any(event, na.rm = TRUE),
              fup_date = min(fup_date, na.rm = TRUE)) %>% 
    right_join(fup_db) %>% 
    mutate(date = if_else(event, fup_date, last_fup, last_fup)) %>% 
    mutate(sp_event = as.numeric(replace_na(event, FALSE)),
           sp_time = as.numeric(date - fsympt_date)) %>% 
    select(upid, sp_event, sp_time)
  
  
  
  
  #############################
  # Outcome composition
  #############################
  
  ##########
  # Outcome 1 - Death
  ##########
  cat("combining outcome 1...\n")
  outcome1 <- death %>% 
    rename(event = death_event, time = death_time)
  
  ##########
  # Outcome 2 - Intensive care admission or death 
  ##########
  cat("combining outcome 2...\n")
  outcome2 <- full_join(death, icu) %>% 
    mutate_at(vars(ends_with("_time")), as.numeric) %>% 
    rowwise() %>% 
    mutate(event = as.numeric(death_event | icu_event),
           time = ifelse(event == "1", 
                         min(death_time, icu_time),
                         max(death_time, icu_time))) %>% 
    select(upid, event, time)
  
  ##########
  # Outcome 3 - Severe pneumonia, or intensive care admission, or death
  ##########
  cat("combining outcome 3...\n")
  outcome3 <- purrr::reduce(list(sp, icu, death), full_join) %>% 
    mutate_at(vars(ends_with("_time")), as.numeric) %>% 
    rowwise() %>% 
    mutate(event = as.numeric(death_event | icu_event | sp_event),
           time = ifelse(event == "1", 
                         min(death_time, icu_time, sp_time),
                         max(death_time, icu_time, sp_time))) %>% 
    select(upid, event, time)
  
  ##########
  # Outcome 4 - Pneumonia, or intensive care admission, or death
  ##########
  cat("combining outcome 4...\n")
  outcome4 <- purrr::reduce(list(pneumonia, sp, icu, death), full_join) %>% 
    mutate_at(vars(ends_with("_time")), as.numeric) %>% 
    rowwise() %>% 
    mutate(event = as.numeric(death_event | icu_event | sp_event | pneumonia_event),
           time = ifelse(event == "1", 
                         min(death_time, icu_time, sp_time, pneumonia_time),
                         max(death_time, icu_time, sp_time, pneumonia_time))) %>% 
    select(upid, event, time)
  
  ##########
  # Outcome 5 - Hospitalization, or pneumonia, or intensive care admission, or death
  ##########
  cat("combining outcome 5...\n")
  outcome5 <- purrr::reduce(list(hosp, pneumonia, sp, icu, death), full_join) %>% 
    mutate_at(vars(ends_with("_time")), as.numeric) %>% 
    rowwise() %>% 
    mutate(event = as.numeric(death_event | icu_event | sp_event| pneumonia_event | hosp_event),
           time = ifelse(event == "1", 
                         min(death_time, icu_time, sp_time, pneumonia_time, hosp_time),
                         max(death_time, icu_time, sp_time, pneumonia_time, hosp_time))) %>% 
    select(upid, event, time)
  
  #############################
  # Merge outcomes
  #############################
  cat("assembling results...\n")
  outcomes_list <- list(outcome2, outcome1, outcome3, outcome4, outcome5)
  
  db <- purrr::reduce(outcomes_list, left_join) %>% 
    select(-matches("upid\\d+"), -event, -time) %>% 
    left_join(minimal)
  
  # haven::write_sav(db, "~/Downloads/minimal_outcomes.sav")
  
  return(list(db = db, hosp = hosp, pneumoni = pneumonia, 
              severe = sp, icu = icu, death = death, base = fup_db))
}

# file <- "~/Downloads/report-4.csv"
# outcomes$dates <- dates_db
# all_out <- purrr::reduce(outcomes, left_join)
# prepare_export(clean(convert(file)))

# #############################
# # Test
# #############################
# 
# require(survival)
# require(survminer)
# 
# db$ocre <- db$mdh_tp_name == "Ocrelizumab"
# db$rtx <- db$mdh_tp_name == "Rituximab"
# db$cd20 <- db$ocre | db$rtx
# db$age <- as.numeric(db$age)
# db$center <- str_extract(db$upid, "-..-")
# 
# surv <- Surv(time = db$time5, event = db$event5)
# 
# coxph(surv ~ cd20 + sex + age, data = db) 
# 
# ggsurvplot(survfit(surv ~ ocre, data = db))
# ggsurvplot(survfit(surv ~ rtx, data = db))
# ggsurvplot(survfit(surv ~ cd20, data = db))
# ggsurvplot(survfit(surv ~ mdh_tp_name, data = db))
# ggsurvplot(survfit(surv ~ sex, data = db))
# ggsurvplot(survfit(surv ~ ntile(age, 3), data = db))
# ggsurvplot(survfit(surv ~ db$com_hts, data = db))
# ggsurvplot(survfit(surv ~ ntile(db$msh_disdur, 3), data = db))
# 
# 
# 
# #############################
# # da spostare
# #############################
# 
# count(f, Base_Provider, sort = TRUE)
# count(f, Demography_Country, sort = TRUE)
# 
# today_updates <- f %>%
#   select(upid, Base_Provider, Demography_Country,
#        contains("updated at"), contains("created at")) %>%
#   gather("operation", "datetime", -upid, -Base_Provider, -Demography_Country) %>%
#   mutate(datetime = lubridate::parse_date_time(datetime, "dmyHM")) %>%
#   mutate(today = lubridate::date(datetime) == lubridate::today()) %>%
#   mutate(operation = str_extract(operation, "(?<=_).*(?= at)")) %>%
#   filter(today) %>%
#   group_by(upid, Base_Provider, Demography_Country) %>%
#   arrange(datetime) %>%
#   summarise_all(last) %>%
#   ungroup()
# 
# count(today_updates, operation, sort = TRUE)
# count(today_updates, Base_Provider, sort = TRUE)
# count(today_updates, operation, Demography_Country, sort = TRUE)
# 
# 
# operations <- f %>%
#   filter(Base_Provider != "Fake") %>% 
#   select(upid, Base_Provider, Demography_Country,
#          contains("updated at"), contains("created at")) %>%
#   gather("operation", "datetime", -upid, -Base_Provider, -Demography_Country) %>%
#   mutate(datetime = lubridate::parse_date_time(datetime, "dmyHM")) %>%
#   mutate(today = lubridate::date(datetime) == lubridate::today()) %>%
#   mutate(operation = str_extract(operation, "(?<=_).*(?= at)"))
# 
# operations_d <- operations %>% 
#   mutate(day = lubridate::date(datetime), n = 1) %>% 
#   # group_by(day, operation) %>% #, Base_Provider, Demography_Country) %>%
#   group_by(day, operation, Base_Provider, Demography_Country) %>% 
#   arrange(day) %>% 
#   summarise(n = sum(n)) %>% 
#   na.omit()
# 
# ggplot(filter(operations_d, Demography_Country == "Italy"), 
#        aes(day, n, col = operation)) +
#   geom_point(alpha = 0.5) +
#   geom_line() +
#   facet_wrap(~ Base_Provider) +
#   scale_x_date(date_breaks = "1 week", date_labels = "%d %B") +
#   theme(axis.text.x = element_text(angle = 90))
# 
# ggsave("~/Downloads/temp.pdf", width = 50, height = 40, units = "cm")

# #############################
# # da spostare
# #############################
# single center procedure
# centro24<-clean(convert("~/Downloads/report (16).csv"))
# out <- compute_outcomes(centro24)
# exp <- prepare_export(centro24)
# c24 <- left_join(exp, select(out, PAT_ID = upid, contains("time"), contains("event")))
# centr_24 <- c24 %>% 
#   filter(grepl("Italy-24", PAT_ID)) %>% 
#   mutate_all(~ ifelse(trimws(.) == "", NA, .)) %>%
#   select_if(~ !(all(is.na(.))))
# centr_24 <- mutate_at(centr_24, vars(AGE,	HEIGHT,	WEIGHT, COHAB,	COHAB_CHILD	,COVID_COHAB, LAST_EDSS, FEVER_VALUE, matches("time\\d")), as.numeric)
# centr_24 <- mutate_at(centr_24, vars(FEVER_ONSET, LAB_DATA, START_DMD, STOP_DMD, contains("_date")), lubridate::dmy)
# openxlsx::write.xlsx(centr_24, "~/Downloads/Site24.xlsx")







