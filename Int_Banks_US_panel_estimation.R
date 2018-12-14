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
############## Panel Estimation Begins ###########################
##################################################################

formula_full <- Integration ~ size + eq_ratio + NIM + 
  T1_T2_ratio + DFR

formula_alt <- Integration ~ size + com_eq_ratio + NIM +
  T1_T2_ratio + DFR + STFR + MBS + func_log10(fhlbq) 

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

###########################################
###### Subsample Panel Regressions ########
###########################################

## For systemic banks ##

panel_US_sys <- panel_US_bank_int %>%
  dplyr::filter(Banks %in% name_systemic)

panel_est_full_sys <- func_panel_est(formula_full, panel_US_sys)
names(panel_est_full_sys) <- c("tidy", "summary")

panel_est_alt_sys <- func_panel_est(formula_alt, panel_US_sys)
names(panel_est_alt_sys) <- c("tidy", "summary")

## For banks with MBS data ##

panel_US_MBS <- panel_US_bank_int %>%
  dplyr::filter(MBS != 0) %>%
  dplyr::filter(!is.na(MBS))

# ggplot(panel_US_MBS, aes(year_qtr, MBS, color = Banks)) +
#   geom_point() +
#   ylab("Mortgage Backed Securities Held for Sale") +
#   xlab("Years") +
#   coord_flip() +
#   theme_bw()

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
panel_est_rec_full_1 <- func_panel_est(formula_full, panel_rec_NBER_2)
panel_est_rec_full_2 <- func_panel_est(formula_full, panel_rec_NBER_2)

panel_est_rec_alt <- func_panel_est(formula_alt, panel_rec_NBER)
panel_est_rec_alt_1 <- func_panel_est(formula_alt, panel_rec_NBER_2)
panel_est_rec_alt_2 <- func_panel_est(formula_alt, panel_rec_NBER_2)

## For systemic banks only ##

panel_rec_NBER_sys <- dplyr::filter(panel_rec_NBER, Banks %in% name_systemic)
panel_rec_NBER_1_sys <- dplyr::filter(panel_rec_NBER_1, Banks %in% name_systemic)
panel_rec_NBER_2_sys <- dplyr::filter(panel_rec_NBER_2, Banks %in% name_systemic)

panel_est_rec_full_sys <- func_panel_est(formula_full, panel_rec_NBER_sys)
panel_est_rec_full_1_sys <- func_panel_est(formula_full, panel_rec_NBER_1_sys)
panel_est_rec_full_2_sys <- func_panel_est(formula_full, panel_rec_NBER_2_sys)

panel_est_rec_alt_sys <- func_panel_est(formula_alt, panel_rec_NBER_sys)
panel_est_rec_alt_1_sys <- func_panel_est(formula_alt, panel_rec_NBER_1_sys)
panel_est_rec_alt_2_sys <- func_panel_est(formula_alt, panel_rec_NBER_2_sys)

###############################
## Testing for fixed effects ##
###############################

#pFtest(panel_est_full$Pool, panel_est_full$Fixed)

##################################################################

t_1 <- Sys.time()
print(t_1 - t_0)
