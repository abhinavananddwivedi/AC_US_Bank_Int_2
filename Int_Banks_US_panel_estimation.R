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
                  size, lev_ratio, debt_ST, net_inc, NIM, T1_ratio, DFR,
                  everything()
                  )
                )


##################################################################
############## Panel Estimation Begins ###########################
##################################################################

formula_full <- Integration ~ size + lev_ratio + debt_ST + 
  net_inc + NIM + T1_ratio + DFR

func_panel_est <- function(formula, panel_data)
{
  # This function takes in the regression formula and dataset
  # and returns panel estimation with pooled and fixed effects.
  # Heteroskedasticity is taken into account and clustering is
  # done at the group (bank) level.
  temp_pool <- plm::plm(formula, 
                        model = "pooling", 
                        data = panel_data)
  
  tidy_pool <- broom::tidy(lmtest::coeftest(temp_pool, 
                          vcov=vcovHC(temp_pool, 
                                    type="HC0", 
                                    cluster="group")))
  
  temp_fixed <- plm::plm(formula, 
                         model = "within", 
                         data = panel_data)
  
  tidy_fixed <- broom::tidy(lmtest::coeftest(temp_fixed, 
                                            vcov=vcovHC(temp_fixed, 
                                                        type="HC0", 
                                                        cluster="group")))
  
  panel_est_tidy <- list(tidy_pool, tidy_fixed)
  
  return(panel_est_tidy)
}

panel_est_full <- func_panel_est(formula_full, panel_US_bank_int)
names(panel_est_full) <- c("Pool", "Fixed")


###############################
## Testing for fixed effects ##
###############################

#pFtest(panel_est_full$Pool, panel_est_full$Fixed)

##################################################################

t_1 <- Sys.time()
print(t_1 - t_0)
