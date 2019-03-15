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

### Correlation matrix ###

temp_cor_matrix <- panel_US_bank_int %>% 
  dplyr::select(Integration, 
                size, 
                eq_ratio, 
                NIM, 
                T1_T2_ratio, 
                DFR) %>% 
  cor(., use = "pairwise.complete.obs")

#knitr::kable(temp_temp, format = 'latex', digits = 3)

func_summ_stat <- function(vec)
{
  temp <- data.frame('min' = min(vec, na.rm = T),
                     'max' = max(vec, na.rm = T),
                     'mean' = mean(vec, na.rm = T),
                     'med' = median(vec, na.rm = T),
                     'std' = sd(vec, na.rm = T),
                     'iqr' = IQR(vec, na.rm = T),
                     'skew' = moments::skewness(vec, na.rm = T),
                     'kurt' = moments::kurtosis(vec, na.rm = T)
                     )
  return(temp)
}

summ_integration <- panel_US_bank_int %>%
  dplyr::select(Integration) %>%
  purrr::map(., func_summ_stat)
summ_size <- panel_US_bank_int %>%
  dplyr::select(size) %>%
  purrr::map(., func_summ_stat)
summ_eq_ratio <- panel_US_bank_int %>%
  dplyr::select(eq_ratio) %>%
  purrr::map(., func_summ_stat)
summ_NIM <- panel_US_bank_int %>%
  dplyr::select(NIM) %>%
  purrr::map(., func_summ_stat)
summ_T1_T2_ratio <- panel_US_bank_int %>%
  dplyr::select(T1_T2_ratio) %>%
  purrr::map(., func_summ_stat)
summ_DFR <- panel_US_bank_int %>%
  dplyr::select(DFR) %>%
  purrr::map(., func_summ_stat)


# knitr::kable(rbind(summ_integration$Integration,
#                    summ_size$size,
#                    summ_eq_ratio$eq_ratio,
#                    summ_NIM$NIM,
#                    summ_T1_T2_ratio$T1_T2_ratio,
#                    summ_DFR$DFR),
#              format = 'latex',
#              digits = 3)


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

### Panel estimation: Full ###
panel_est_full <- func_panel_est(formula_full, panel_US_bank_int)

### Panel estimation: Systemic Banks ###
panel_est_sys <- func_panel_est(formula_full, 
                                dplyr::filter(panel_US_bank_int, 
                                              Banks %in% name_systemic))

### Panel estimation: Pre 2005 ###
panel_est_H1 <- func_panel_est(formula_full,
                               dplyr::filter(panel_US_bank_int,
                                             Date %in% year_qtr_H1))

### Panel estimation: Post 2005 ###
panel_est_H2 <- func_panel_est(formula_full,
                               dplyr::filter(panel_US_bank_int,
                                             Date %in% year_qtr_H2))


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
###robust errors destroy some result###

##################################################################

t_1 <- Sys.time()
print(t_1 - t_0)
