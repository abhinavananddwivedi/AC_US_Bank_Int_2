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

### Pooled panel estimates ###

panel_est_pooled <- plm::plm(formula = formula_full,
                             data = panel_US_bank_int,
                             model = "pooling",
                             type = "HC0") %>% summary(.)


##############################################################
### Testing Dodd-Frank's effects #############################
##############################################################

# Attach dodd frank dummy: qtrs 71--100
panel_US_dodd_frank <- panel_US_bank_int %>% 
  dplyr::mutate(dodd_frank = case_when(Qtr_num >= 71 ~ 1, 
                                       Qtr_num < 71 ~ 0)) %>%
  dplyr::select(Banks, Date, Qtr_num, 
                Integration, T1_T2_ratio, dodd_frank)

# Median systemic bank's T1T2
panel_sys_med_T1T2_df <- panel_US_dodd_frank %>%
  dplyr::select(Banks, Qtr_num, T1_T2_ratio) %>%
  dplyr::filter(Banks %in% name_systemic) %>%
  dplyr::group_by(Qtr_num) %>%
  dplyr::summarise(med_T1T2 = median(T1_T2_ratio, na.rm = T))

# Plotting median sytemic bank's T1 T2 ratios
plot_sys_med_T1T2_df <- ggplot(panel_sys_med_T1T2_df, 
                               aes(Qtr_num, med_T1T2)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = 71, linetype = 'twodash') +
  geom_vline(xintercept = 63, linetype = 'dashed') +
  theme_bw() +
  scale_x_continuous(breaks = x_breaks, #x_breaks and x_labesl from ...analysis.R
                     labels = x_labels) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 16)) +
  theme(axis.text.y = element_text(size = 18)) +
  labs(x = NULL) +
  labs(y = "Median systemic bank's combined tier 1 and 2 capital ratio") +
  theme(axis.title.y = element_text(size = 18))

# 

## Integration levels pre and post Dodd-Frank ##

# Post Dodd Frank
# stat_sys_int_post_df <- T1_T2_sys_post_df %>%
#   dplyr::select(Banks, Integration) %>%
#   dplyr::group_by(Banks) %>%
#   dplyr::summarise(mean_int_post_df = mean(Integration, na.rm = T),
#                    median_int_post_df = median(Integration, na.rm = T),
#                    sd_int_post_df = sd(Integration, na.rm = T),
#                    iqr_int_post_df = IQR(Integration, na.rm = T))
#    
# ## Pre Dodd Frank
# stat_sys_int_pre_df <- T1_T2_sys_pre_df %>%
#   dplyr::select(Banks, Integration) %>%
#   dplyr::group_by(Banks) %>%
#   dplyr::summarise(mean_int_pre_df = mean(Integration, na.rm = T),
#                    median_int_pre_df = median(Integration, na.rm = T),
#                    sd_int_pre_df = sd(Integration, na.rm = T),
#                    iqr_int_pre_df = IQR(Integration, na.rm = T))

#############################################################################

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
