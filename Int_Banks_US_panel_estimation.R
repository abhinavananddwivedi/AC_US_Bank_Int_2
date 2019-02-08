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

####################################################
####################################################

# Preprocess then compute bank integration results

source('Int_Banks_US_results_analysis.R', echo = F) #includes two nested scripts

####################################################
####################################################

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
                  size, eq_ratio, NIM, T1_T2_ratio, DFR, 
                  STFR, com_eq_ratio, fhlbq, MBS, 
                  DE_ratio_1, T1_ratio,
                  everything()
                  )
                )

##################################################################
#### Special subsamples: Bank-wise and Time-wise #################
##################################################################

temp_year_repeat <- rep(year_min:year_max, each = 4)
temp_qtr_repeat <- rep(c("Q1","Q2","Q3","Q4"), num_years)
year_qtr_full <- paste0(temp_year_repeat, temp_qtr_repeat)

year_qtr_len <- length(year_qtr_full)

### Sample period division into 2 halves ###

year_qtr_H1 <- year_qtr_full[1:(floor(year_qtr_len/2))]
year_qtr_H2 <- year_qtr_full[(floor(year_qtr_len/2) + 1): year_qtr_len]

### Bank sample division into highest and lowest integrated ###

func_top_bot_decile <- function(vec)
{
  temp_top <- quantile(vec, 0.90, na.rm = T)
  temp_bot <- quantile(vec, 0.10, na.rm = T)
  
  temp_temp <- data.frame("top" = temp_top, "bot" = temp_bot)
  
  return(tibble::as_tibble(temp_temp))
}




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
  
  temp_fixed_rob <- lmtest::coeftest(temp_fixed, vcov. = temp_vcov_err)
  
  test_out <- summary(temp_fixed)
  test_out$coefficients <- unclass(temp_fixed_rob) #Include robust clustered errors
  
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

## For NBER recessionary periods ##

rec_NBER_1 <- paste0("2001Q", 1:4) #Dot com bust
rec_NBER_2 <- c("2007Q4", paste0("2008Q", 1:4), "2009Q1", "2009Q2") #The Great
rec_NBER <- c(rec_NBER_1, rec_NBER_2)

panel_rec_NBER <- panel_US_bank_int %>% 
  dplyr::filter(year_qtr %in% rec_NBER)

panel_rec_NBER_1 <- panel_US_bank_int %>% 
  dplyr::filter(year_qtr %in% rec_NBER_1)

panel_rec_NBER_2 <- panel_US_bank_int %>% 
  dplyr::filter(year_qtr %in% rec_NBER_2)

## Recessionary Explanatory Variables ##

panel_est_rec_full <- func_panel_est(formula_full, panel_rec_NBER)
panel_est_rec_full_1 <- func_panel_est(formula_full, panel_rec_NBER_1)
panel_est_rec_full_2 <- func_panel_est(formula_full, panel_rec_NBER_2)

## For systemic banks only ##

panel_rec_NBER_sys <- dplyr::filter(panel_rec_NBER, Banks %in% name_systemic)
panel_rec_NBER_1_sys <- dplyr::filter(panel_rec_NBER_1, Banks %in% name_systemic)
panel_rec_NBER_2_sys <- dplyr::filter(panel_rec_NBER_2, Banks %in% name_systemic)

panel_est_rec_full_sys <- func_panel_est(formula_full, panel_rec_NBER_sys)
panel_est_rec_full_1_sys <- func_panel_est(formula_full, panel_rec_NBER_1_sys)
panel_est_rec_full_2_sys <- func_panel_est(formula_full, panel_rec_NBER_2_sys)

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
