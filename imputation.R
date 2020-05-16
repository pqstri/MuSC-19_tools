#' age sex EDSS disdur comorbidities e BMI
#' tipo di malattia (RR vs Progressive)
#' age e sex ci sono per tutti
#' BMI manca per parecchi
#' ma si può imputare
#' da eta sesso e diabete

library("mice")

# impute-outcome-export
ioe <- function(f, debug = FALSE, check_original = FALSE) {
  
  qmiss <- function(df) {
    df %>% 
      mutate_all(is.na) %>% 
      summarise_all(sum) %>% 
      gather() %>% 
      mutate(perc = value/nrow(j))
  }
  
  outcomes_db <- compute_outcomes(f, debug = debug, check_original = check_original)
  exported_db <- prepare_export(f)
  
  if (debug) {return(outcomes_db$events)}
  
  j <- left_join(exported_db, outcomes_db, by = c("PAT_ID" = "upid"))
  
  j$MS_TYPE <- factor(j$MS_TYPE)
  
  j[!is.na(j$HEIGHT) & j$HEIGHT < 2,]$HEIGHT <-
    j[!is.na(j$HEIGHT) & j$HEIGHT < 2,]$HEIGHT * 100
  
  j[!is.na(j$HEIGHT) & j$HEIGHT < 100,]$HEIGHT <-
    j[!is.na(j$HEIGHT) & j$HEIGHT < 100,]$HEIGHT + 100
  
  j$BMI <- j$WEIGHT / ((j$HEIGHT/100)^2)
  
  # cor(select_if(j, is.numeric), use = "pairwise.complete.obs") %>% 
  #   data.frame() %>% 
  #   rownames_to_column("var1") %>% 
  #   as.tibble() %>% 
  #   gather("var2", "r", -var1) %>% 
  #   filter(var1 != var2) %>% 
  #   arrange(r)
  
  ##
  # imputation of others
  cat("Imputing...")
  minimal_vara <- c("AGE", "SEX", "HEIGHT", "WEIGHT", "BMI", "MS_TYPE", 
                    "COHAB_CHILD", "msh_disdur", "com_dbt", "com_hts")
  
  minimal <- select(j, all_of(minimal_vara))
  
  # mice::md.pattern(minimal)
  
  # We run the mice code with 0 iterations 
  imp <- mice::mice(minimal, maxit = 0)
  
  # Extract predictorMatrix and methods of imputation 
  predM = imp$predictorMatrix
  meth = imp$method
  
  # predM[, c("AGE")]
  # predM[, c("SEX")]
  # predM[, c("HEIGHT")]
  # predM[, c("WEIGHT")]
  # predM[, c("BMI")]
  # predM[, c("MS_TYPE")]
  # predM[, c("COHAB_CHILD")]
  # predM[, c("msh_disdur")]
  # predM[, c("com_dbt")]
  # predM[, c("com_hts")]
  
  # Unordered categorical variable 
  poly_u <- c("MS_TYPE")
  
  # Turn their methods matrix into the specified imputation models
  meth[poly_u] = "polyreg"
  
  imp2 <- mice::mice(minimal, maxit = 15, predictorMatrix = predM, 
                     method = meth, print = FALSE)
  
  filled <- mice::complete(imp2, 1)
  
  
  # minimal %>% qmiss()
  # filled %>% qmiss()
  
  # map(minimal_vara, ~ summary(minimal[,.]))
  # map(minimal_vara, ~ summary(filled[,.]))
  
  
  # compareDF::compare_df(rownames_to_column(anesimp_long, "id"), 
  #                       rownames_to_column(minimal, "id"), 
  #                       group_col = "id") %>% 
  #   compareDF::create_output_table(limit = 50)
  
  temp <- bind_cols(filled, select(j, -minimal_vara)) %>% 
    mutate(msh_disdur = abs(msh_disdur)) %>% 
    select(names(j))
  
  # openxlsx::write.xlsx(temp, "~/Downloads/imputed.xlsx")
  
  return(temp)
  
}




