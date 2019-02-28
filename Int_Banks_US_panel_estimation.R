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
library(modelr)

####################################################
####################################################

# Preprocess then compute bank integration results
#includes two nested scripts
source('Int_Banks_US_results_analysis.R', echo = F) 

####################################################
####################################################

# Postprocessing data 

data_Cstat_expl <- data_Cstat_expl %>%
  dplyr::rename(., "Date" = datacqtr, "Banks" = conm)

# Merge the two dataframes together to create the panel
panel_US_bank_int <- dplyr::inner_join(int_US_bank_long_3,
                                      data_Cstat_expl,
                                      by = c("Banks", "Date")
                                      ) %>%
  dplyr::select(c(Banks, Date, Integration, 
                  size, eq_ratio, NIM, T1_T2_ratio, DFR, 
                  GR, EZ, Qtr_num,
                  everything()
                  )
                )

# Nested panel full
nest_panel_full <- panel_US_bank_int %>%
  dplyr::group_by(Banks) %>%
  tidyr::nest(.)
# Nest panel first half
nest_panel_H1 <- panel_US_bank_int %>%
  dplyr::filter(Date %in% year_qtr_H1) %>%
  dplyr::group_by(Banks) %>%
  tidyr::nest(.)
# Nest panel second half
nest_panel_H2 <- panel_US_bank_int %>%
  dplyr::filter(Date %in% year_qtr_H2) %>%
  dplyr::group_by(Banks) %>%
  tidyr::nest(.)
# Nest systemic banks 
nest_panel_systemic <- nest_panel_full %>% 
  dplyr::filter(Banks %in% name_systemic)
# Nest top 50 most integrated banks
nest_panel_top_50 <- nest_panel_full %>%
  dplyr::filter(Banks %in% name_bank_top_50)
# Nest top 50 least integrated banks
nest_panel_top_50 <- nest_panel_full %>%
  dplyr::filter(Banks %in% name_bank_bot_50)

##################################################################
############## Panel Estimation Begins ###########################
##################################################################

formula_full <- Integration ~ size + eq_ratio + NIM + 
  T1_T2_ratio + DFR 

func_panel_est <- function(formula, 
                           panel_data, 
                           mdl = "within"
                           )
{
  # Panel estimation with fixed effects
  temp_fixed <- plm::plm(formula, 
                         data = panel_data,
                         model = mdl, 
                         type = "HC0",
                         effect = "individual"
                         )
  
  # Robust, clustered standard errors
  temp_vcov_err <- plm::vcovDC(temp_fixed) #Double clustering
  
  temp_fixed_rob <- lmtest::coeftest(temp_fixed, 
                                     vcov. = temp_vcov_err)
  
  test_out <- summary(temp_fixed)
  
  # Include robust clustered errors
  test_out$coefficients <- unclass(temp_fixed_rob) 
  
  return(test_out)
}

panel_est_full <- func_panel_est(formula_full, panel_US_bank_int)

###########################################
###### Subsample Panel Regressions ########
###########################################

## For systemic banks ##

panel_US_sys <- panel_US_bank_int %>%
  dplyr::filter(Banks %in% name_systemic)

panel_est_full_sys <- func_panel_est(formula_full, panel_US_sys)

###############################
## Testing for fixed effects ##
###############################

#pFtest(panel_est_full$Pool, panel_est_full$Fixed)

###this works fine by the way###
temp_twoway <- summary(plm::plm(formula_full, 
                                panel_US_bank_int, 
                                model = "within", 
                                effect = "twoways", 
                                type = "HC0"
                                )
                       )
###robust errors destroy result###

##################################################################

t_1 <- Sys.time()
print(t_1 - t_0)
