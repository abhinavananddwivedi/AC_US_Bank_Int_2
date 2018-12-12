t_0 <- Sys.time()

########################################################
##### Panel Estimation: Integration Among US Banks #####
########################################################

# Declare libraries
library(tidyverse)
library(moments)
library(zoo)
library(plm)
library(knitr)
library(broom)
library(lmtest)

# Preprocess then compute bank integration results
source('Int_Banks_US_results_analysis.R', echo = F)
#includes two other nested script files

# Postprocessing data 

int_US_bank_long <- int_US_bank_long %>%
  dplyr::rename(., "year_qtr" = Date)

data_Cstat_expl <- data_Cstat_expl %>%
  dplyr::rename(., "year_qtr" = datacqtr, "Banks" = conm)

# Merge the two dataframes together to create the panel
panel_US_bank_int <- dplyr::full_join(int_US_bank_long,
                                      data_Cstat_expl,
                                      by = c("Banks", "year_qtr")
                                      ) %>%
  dplyr::select(c(Banks, year_qtr, Integration, 
                  size, DE_ratio_1, NIM, T1_ratio, DFR, STFR,
                  T1_T2_ratio, com_eq_ratio, fhlbq, mbshsq,
                  everything()
                  )
                )


##################################################################
############## Panel Estimation Begins ###########################
##################################################################

formula_full <- Integration ~ size + DE_ratio_1 + NIM + 
  T1_T2_ratio + DFR 

formula_alt <- Integration ~ size + com_eq_ratio + NIM +
  T1_T2_ratio + DFR + fhlbq + mbshsq

func_panel_est <- function(formula, panel_data)
{
  # This function takes in the regression formula and dataset
  # and returns panel estimation with pooled and fixed effects.
  # Heteroskedasticity is taken into account and clustering is
  # done at the group (bank) level.
  temp_pool <- plm::plm(formula, 
                        model = "pooling", 
                        data = panel_data)
  
  temp_pool_summ <- summary(temp_pool)
  
  tidy_pool <- broom::tidy(lmtest::coeftest(temp_pool, 
                          vcov=vcovHC(temp_pool, 
                                    type="HC0", 
                                    cluster="group")))
  
  temp_fixed <- plm::plm(formula, 
                         model = "within", 
                         data = panel_data)
  
  temp_fixed_summ <- summary(temp_fixed)
  
  tidy_fixed <- broom::tidy(lmtest::coeftest(temp_fixed, 
                                            vcov=vcovHC(temp_fixed, 
                                                        type="HC0", 
                                                        cluster="group")))
  
  return(list(tidy_fixed, temp_fixed_summ))
}

panel_est_full <- func_panel_est(formula_full, panel_US_bank_int)
names(panel_est_full) <- c("tidy", "summary")

panel_est_alt <- func_panel_est(formula_alt, panel_US_bank_int)
names(panel_est_alt) <- c("tidy", "summary")

# For systemic banks
panel_US_sys <- panel_US_bank_int %>%
  dplyr::filter(Banks %in% name_systemic)

panel_est_full_sys <- func_panel_est(formula_full, panel_US_sys)
names(panel_est_full_sys) <- c("tidy", "summary")

panel_est_alt_sys <- func_panel_est(formula_alt, panel_US_sys)
names(panel_est_alt_sys) <- c("tidy", "summary")

###############################
## Testing for fixed effects ##
###############################

#pFtest(panel_est_full$Pool, panel_est_full$Fixed)

##################################################################

t_1 <- Sys.time()
print(t_1 - t_0)
