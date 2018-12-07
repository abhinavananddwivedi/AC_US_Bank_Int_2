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
  dplyr::select(c(Banks, year_qtr, Integration, size, lev_ratio, 
                  everything()
                  )
                )


##################################################################
############## Panel Estimation Begins ###########################
##################################################################

formula_full <- Integration ~ size + lev_ratio

func_panel_est <- function(formula, panel_data)
{
  # This function takes in the regression formula and data
  # and returns three different panel specifications: pooled,
  # fixed effects and random effects
  temp_pool <- plm::plm(formula, model = "pooling", data = panel_data)
  
  tidy_pool <- broom::tidy(lmtest::coeftest(temp_pool, 
                          vcov=vcovHC(temp_pool, 
                                    type="HC0", 
                                    cluster="group")))
  
  #temp_fixed <- summary(plm::plm(formula, "within", panel_data))
  
  return(tidy_pool)
}

### Pooling ###

panel_est_full_pool <- plm::plm(formula_full,
                                model = "pooling",
                                data = panel_US_bank_int
                                )
tidy_pool <- broom::tidy(lmtest::coeftest(panel_est_full_pool, 
                              vcov=vcovHC(panel_est_full_pool, 
                                          type="HC0", 
                                          cluster="group")))


#knitr::kable(tidy_pool, digits = 2, caption = "Pooled Model")

### Fixed Effects ###

panel_est_full_fixed <- plm::plm(formula_full,
                                model = "within",
                                data = panel_US_bank_int)

tidy_fixed <- broom::tidy(lmtest::coeftest(panel_est_full_fixed, 
                                          vcov=vcovHC(panel_est_full_fixed, 
                                                      type="HC0", 
                                                      cluster="group")))
knitr::kable(tidy_fixed, digits = 2, caption = "Fixed Effects Model")

###############################
## Testing for fixed effects ##
###############################

pFtest(panel_est_full_pool, panel_est_full_fixed)

##################################################################

t_1 <- Sys.time()
print(t_1 - t_0)