time_start <- Sys.time()

########################################################
### Results and Analysis: Integration Among US Banks ###
########################################################

# Declare libraries
library(tidyverse)
library(moments)
library(zoo)

# Read, tidy and preprocess the datasets from CRSP and Compustat 
source('Int_Banks_US.R', echo = F)
#includes another nested script file for preprocessing raw data

func_neg_to_zero <- function(vec)
{
  # This function converts -ve values to 0
  temp <- which(vec < 0)
  vec[temp] <- 0
  return(vec)
}
  
# Bank integration data in long form (panel format)
int_US_bank_long <- int_mat_qtrly_out_long
temp_int_long <- func_neg_to_zero(int_US_bank_long$Integration)
int_US_bank_long$Integration <- temp_int_long 

# Bank integration data in wide form (matrix format)
int_US_bank_wide <- int_US_bank_long %>%
  tidyr::spread(., key = "Banks", value = "Integration")

##########################
### Summary Statistics ###
##########################

## By Quarter
int_summ_stat_qrtrly <- int_US_bank_long %>%
  dplyr::group_by(Date) %>%
  dplyr::summarise(., 'minimum' = min(Integration, na.rm = T), 
                   'maximum' = max(Integration, na.rm = T), 
                   'avg' = mean(Integration, na.rm = T), 
                   'med' = median(Integration, na.rm = T), 
                   'std_dev' = sd(Integration, na.rm = T), 
                   'iqr' = IQR(Integration, na.rm = T),
                   'skew' = moments::skewness(Integration, na.rm = T),
                   'kurt' = moments::kurtosis(Integration, na.rm = T)
                   )
readr::write_csv(int_summ_stat_qrtrly, 
                 "US_Bank_Intgration_Summary_Quarterly.csv")

## By Banks ##
int_summ_stat_bank <- int_US_bank_long %>%
  dplyr::group_by(Banks) %>%
  dplyr::summarise(., 'minimum' = min(Integration, na.rm = T), 
                   'maximum' = max(Integration, na.rm = T), 
                   'avg' = mean(Integration, na.rm = T), 
                   'med' = median(Integration, na.rm = T), 
                   'std_dev' = sd(Integration, na.rm = T), 
                   'iqr' = IQR(Integration, na.rm = T),
                   'skew' = moments::skewness(Integration, na.rm = T),
                   'kurt' = moments::kurtosis(Integration, na.rm = T)
                   )
readr::write_csv(int_summ_stat_bank, "US_Bank_Intgration_Summary_Bankwise.csv")

##################################################################
#### Special subsamples: Bank-wise and Time-wise #################
##################################################################

######################
### Time subsample ###
######################

temp_year_repeat <- rep(year_min:year_max, each = 4)
temp_qtr_repeat <- rep(c("Q1","Q2","Q3","Q4"), num_years)
year_qtr_full <- paste0(temp_year_repeat, temp_qtr_repeat)
year_qtr_len <- length(year_qtr_full) #length 

num_bank <- int_US_bank_long %>% 
  dplyr::select(Banks) %>% 
  dplyr::distinct(.) %>%
  dplyr::count(.)

temp_qtr_num_repeat <- rep(qtr_grid[-1], num_bank$n)

int_US_bank_long_2 <- int_US_bank_long %>%
  dplyr::mutate("Qtr_num" = temp_qtr_num_repeat)
# Use the nest technique 
nest_bank_int_US_full <- int_US_bank_long_2 %>%
  dplyr::group_by(Banks) %>%
  tidyr::nest(.)

### Sample period division into 2 halves ###
qtr_num_H1 <- 1:(floor(year_qtr_len/2)) #Quarter numbers 1 to 50
qtr_num_H2 <- (floor(year_qtr_len/2) + 1):year_qtr_len #Quarter numbers 51 to 100
year_qtr_H1 <- year_qtr_full[qtr_num_H1]
year_qtr_H2 <- year_qtr_full[qtr_num_H2]

# Separate the data for the first and the second half #
int_US_bank_long_H1 <- int_US_bank_long_2 %>%
  dplyr::filter(Date %in% year_qtr_H1) 
int_US_bank_long_H2 <- int_US_bank_long_2 %>%
  dplyr::filter(Date %in% year_qtr_H2)
# Nested dataframe
nest_bank_int_US_H1 <- int_US_bank_long_H1 %>%
  dplyr::group_by(Banks) %>%
  tidyr::nest(.)
nest_bank_int_US_H2 <- int_US_bank_long_H2 %>%
  dplyr::group_by(Banks) %>%
  tidyr::nest(.)

################################################
### Linear trend with robust standard errors ###
################################################

func_trend_HC <- function(df)
{
  # This function computes the linear trend and reports
  # heteroskedasticity and autocorrelation consistent errors
  temp_lm <- lm(Integration ~ Qtr_num, data = df)
  temp_summ <- summary(temp_lm)
  temp_summ$coefficients <- unclass(lmtest::coeftest(temp_lm,
                                                     vcov. = vcovHC)
                                    )
  
  return(temp_summ)
}

# Linear Trends for the full set of banks #

# Compute linear trends and HC errors
nest_bank_int_US_full <- nest_bank_int_US_full %>%
  dplyr::mutate("Missing" = purrr::map(data, func_missing)) %>%
  dplyr::mutate("Trend" = purrr::map(data, func_trend_HC))

# Display main statistics
nest_bank_int_US_full <- nest_bank_int_US_full %>%
  dplyr::mutate("Model_Summary" = map(Trend, broom::glance)) %>%
  tidyr::unnest(Model_Summary)

# Filter banks with significant trends
trend_bank_sig_full <- nest_bank_int_US_full %>%
  dplyr::select(Banks, p.value) %>%
  dplyr::filter(p.value <= 0.10)

# Linear Trends pre 2005 #

nest_bank_int_US_H1 <- nest_bank_int_US_H1 %>%
  dplyr::mutate("Missing" = purrr::map(data, func_missing))

nest_bank_int_US_H1 <- nest_bank_int_US_H1 %>%
  dplyr::filter(Missing <= 40) %>% #ignore if more than 40(/50) missing obs
  dplyr::mutate("Trend" = purrr::map(data, func_trend_HC)) %>%
  dplyr::mutate("Model_Summary" = map(Trend, broom::glance)) %>%
  tidyr::unnest(Model_Summary)

trend_bank_sig_H1 <- nest_bank_int_US_H1 %>%
  dplyr::select(Banks, p.value) %>%
  dplyr::filter(p.value <= 0.10)

# Linear Trends post 2005 #

nest_bank_int_US_H2 <- nest_bank_int_US_H2 %>%
  dplyr::mutate("Missing" = purrr::map(data, func_missing))

nest_bank_int_US_H2 <- nest_bank_int_US_H2 %>%
  dplyr::filter(Missing <= 40) %>% 
  dplyr::mutate("Trend" = purrr::map(data, func_trend_HC)) %>%
  dplyr::mutate("Model_Summary" = map(Trend, broom::glance)) %>%
  tidyr::unnest(Model_Summary)

trend_bank_sig_H2 <- nest_bank_int_US_H2 %>%
  dplyr::select(Banks, p.value) %>%
  dplyr::filter(p.value <= 0.10)

## Linear trend for the median bank ##

int_median_US_bank <- int_US_bank_long_2 %>%
  dplyr::group_by(Qtr_num) %>%
  dplyr::summarise('Integration' = median(Integration, na.rm = T))
  
trend_median_US_bank <- func_trend_HC(int_median_US_bank)

# Linear trend for for the median bank: pre and post 2005 #

# Pre 2005
int_median_US_bank_H1 <- int_US_bank_long_2 %>%
  dplyr::filter(Qtr_num <= 50) %>%
  dplyr::group_by(Qtr_num) %>%
  dplyr::summarise('Integration' = median(Integration, na.rm = T))

trend_median_US_bank_H1 <- func_trend_HC(int_median_US_bank_H1)

# Post 2005
int_median_US_bank_H2 <- int_US_bank_long_2 %>%
  dplyr::filter(Qtr_num > 50) %>%
  dplyr::group_by(Qtr_num) %>%
  dplyr::summarise('Integration' = median(Integration, na.rm = T))

trend_median_US_bank_H2 <- func_trend_HC(int_median_US_bank_H2)

## Plotting median bank's integration trend ##

# Full
plot_trend_median <- ggplot(int_median_US_bank,
                            aes(Qtr_num, Integration)) +
  geom_line() +
  geom_smooth(method = "lm", 
              linetype = "dashed", 
              color = "black") +
  theme_bw()

# First Half
plot_trend_median_H1 <- ggplot(int_median_US_bank_H1,
                            aes(Qtr_num, Integration)) +
  geom_line() +
  geom_smooth(method = "lm", 
              linetype = "dashed", 
              color = "black") +
  theme_bw()

# Second Half
plot_trend_median_H2 <- ggplot(int_median_US_bank_H2,
                            aes(Qtr_num, Integration)) +
  geom_line() +
  geom_smooth(method = "lm", 
              linetype = "dashed", 
              color = "black") +
  theme_bw()

################################################################
########## Quintile Based Integration Time Series ##############
################################################################

func_quint <- function(vec)
{
  # This function returns 20th, 40th, 60th
  # and 80th percentiles of a given vector
  # after ignoring missing values
  
  temp_temp <- quantile(vec, c(0.20, 0.40,
                               0.60, 0.80),
                        na.rm = T)

  return(temp_temp)
}

quint_bank_int <- apply(int_US_bank_wide[, -1], 1, func_quint) 
quint_bank_int <- t(quint_bank_int) #Transpose
colnames(quint_bank_int) <- c("Perc_20", "Perc_40", "Perc_60", "Perc_80")

matplot(quint_bank_int, 
        type = "l", 
        lwd = 1,
        lty = 1:ncol(quint_bank_int), 
        xlab = "Years", 
        ylab = "Bank Integration Quintiles"
        )

 
# legend(75, 0.3, 
#       legend = colnames(quint_bank_int), 
#       col = 1:ncol(quint_bank_int), 
#       lty = 1:ncol(quint_bank_int), 
#       lwd = 0.5,
#       cex = 0.5
#       )


##########################################
### Top 25 Banks by Median Integration ###
##########################################

col_missing <- apply(integration_matrix_qtrly_out[, -1], 2, func_missing)
col_admissible <- which(col_missing <= 50)

col_med_admissible <- apply(integration_matrix_qtrly_out[, col_admissible[-1]], 
                            2, func_med) 
names(col_med_admissible) <- name_bank_US$comnam[col_admissible[-1]]

med_admissible_banks <- tibble::as_tibble(col_med_admissible) %>% 
  tibble::rownames_to_column() 

med_top_25_admissible <- med_admissible_banks %>%
  dplyr::filter(rank(desc(value)) <= 25) %>%
  dplyr::arrange(desc(value))

barplot_top_25 <- ggplot(med_top_25_admissible, aes(rowname, value)) +
  geom_bar(stat = "identity", show.legend = T) + 
  labs(x = "Banks", y = "Median Integration") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=60, hjust=1))

###################################
###### Integration Boxplots #######
###################################

# Yearly Integration Boxplots

year_seq <- paste0(seq(year_min, year_max, 1), "Q4")

data_boxplot <- int_US_bank_long %>%
  dplyr::filter(Date %in% year_seq)

boxplot_int_yearly <- ggplot(data = data_boxplot %>% dplyr::group_by(Date),
                              mapping = aes(x = Date, y = Integration)) +
    geom_boxplot(na.rm = T) +
    theme_bw() + 
    theme(axis.text.x=element_text(angle=60, hjust=1)) 

# Quarterly Integration Boxplots

boxplot_int_qtrly <- ggplot(data = int_US_bank_long,
                             mapping = aes(x = Date, y = Integration)) +
  geom_boxplot(na.rm = T) +
#  scale_x_yearqtr(format="%YQ%q", n=25) +
  theme_bw() + 
  theme(axis.text.x=element_text(angle=60, hjust=1)) 

###

#########################################
### Explanatory power of eigenvectors ###
#########################################

func_len_max_NA_add <- function(temp_list)
{ # This function takes a list with elements of differing lengths,
  # then finds the maximum length in the list,
  # then generates NAs equal to the difference between 
  # the individual element lengths and the maximum length
  # For example if element 1 has length 10 and the maximum 
  # length is 100, the returned object will have 90 NAs
  # corresponding to the first element
  len_max <- max(sapply(temp_list, length))
  
  func_vec_NA_add <- function(vec)
  {
    if (length(vec) < len_max)
    {
      NA_add <- rep(NA, len_max - length(vec))
    }
  }
  
  temp_NA_add <- sapply(temp_list, func_vec_NA_add)
  
  return(temp_NA_add)
}

var_share_NA_add <- func_len_max_NA_add(var_share)

# Merge the list var_share and NA list to be appended, convert to data frame
var_share_df <- apply(cbind(var_share, var_share_NA_add), 1, unlist) %>%
  as.data.frame(.)

expl_power_eig_med <- apply(var_share_df, 1, func_med)

## The explanatory power of the top thirty eigenvectors ##

#box_expl_eig <- boxplot(t(var_share_df[1:30, ]))
#bar_expl_eig_med <- barplot(expl_power_eig_med[1:30])


#######################
### GSIBs and DSIBs ###
#######################

name_GSIB <- name_cusip$comnam[c(28, 33, 36, 73, 183, 184, 304, 305, 347)]
name_DSIB <- name_cusip$comnam[c(14, 87, 114, 173, 187, 188, 195, 223, 245, 
                                 246, 247, 277, 278, 311, 335, 361)]

name_systemic <- c(name_GSIB, name_DSIB)

int_US_systemic_wide <- int_US_bank_long %>% 
  dplyr::filter(Banks %in% name_systemic)  %>% 
  tidyr::spread(., key = Banks, value = "Integration")

readr::write_csv(int_US_systemic_wide, "Systemic_bank_int.csv")

int_US_sys_med <- apply(int_US_systemic_wide[, -1], 1, func_med)

plot_data_aug <- plot_data %>%
  tibble::add_column(med_bank_int_sys = int_US_sys_med) 

plot_data_aug_long <- plot_data_aug %>% 
  tidyr::gather(c(med_bank_int, med_bank_int_sys), 
                key = "median", value = "Integration")

plot_systemic <- ggplot(data = plot_data_aug_long, 
                        mapping = aes(x = as.yearqtr(date), 
                                      y = Integration, 
                                      color = median)) +
  geom_line() +
  scale_x_yearqtr(format="%YQ%q", n=25) +
  theme_bw() +
  labs(x = "Years", y = "Median Integration Level") +
  theme(axis.text.x=element_text(angle=60, hjust=1))

###################################
### Relation to NBER recessions ###
###################################

# NBER claims recessionary periods from 
# 2001Q1--2001Q4 and 2007Q4--2009Q2, i.e.,
# quarter numbers 32:35 and 59:65

dummy_recession <- rep(0, length(qtrs))
dummy_recession[c(32:35, 59:65)] <- 1

int_dummy_recession <- summary(lm(temp_int_med_bank ~ qtrs + dummy_recession))

#################################################
### Bank Integration Variation with SIC Codes ###
#################################################

name_bank_SIC_1 <- name_cusip_sic %>%
  dplyr::filter(., siccd %in% ind_comm_banks) %>%
  dplyr::select(comnam) %>%
  dplyr::distinct(.)

# Apply row median function to banks in group SIC commercial banks
# This will be the median commercial bank's integration
bank_int_SIC_1_med <- int_US_bank_long %>%
  dplyr::filter(Banks %in% name_bank_SIC_1$comnam) %>%
  tidyr::spread(., key = "Banks", value = "Integration") %>%
  dplyr::select(-Date) %>%
  apply(., 1, func_med)

name_bank_SIC_2 <- name_cusip_sic %>%
  dplyr::filter(., siccd %in% ind_credit_union) %>%
  dplyr::select(comnam) %>%
  dplyr::distinct(.)

bank_int_SIC_2_med <- int_US_bank_long %>%
  dplyr::filter(Banks %in% name_bank_SIC_2$comnam) %>%
  tidyr::spread(., key = "Banks", value = "Integration") %>%
  dplyr::select(-Date) %>%
  apply(., 1, func_med)

name_bank_SIC_3 <- name_cusip_sic %>%
  dplyr::filter(., siccd %in% ind_saving_inst) %>%
  dplyr::select(comnam) %>%
  dplyr::distinct(.)

bank_int_SIC_3_med <- int_US_bank_long %>%
  dplyr::filter(Banks %in% name_bank_SIC_3$comnam) %>%
  tidyr::spread(., key = "Banks", value = "Integration") %>%
  dplyr::select(-Date) %>%
  apply(., 1, func_med)

name_bank_SIC_4 <- name_cusip_sic %>%
  dplyr::filter(., siccd %in% ind_bank_hold) %>%
  dplyr::select(comnam) %>%
  dplyr::distinct(.)

bank_int_SIC_4_med <- int_US_bank_long %>%
  dplyr::filter(Banks %in% name_bank_SIC_4$comnam) %>%
  tidyr::spread(., key = "Banks", value = "Integration") %>%
  dplyr::select(-Date) %>%
  apply(., 1, func_med)

bank_int_SIC_med <- data.frame(SIC_1 = bank_int_SIC_1_med,
                               SIC_2 = bank_int_SIC_2_med,
                               SIC_3 = bank_int_SIC_3_med,
                               SIC_4 = bank_int_SIC_4_med)

func_summ <- function(vec)
{
  # This function computes summary stats of a vector
  temp_summ <- data.frame(minimum = min(vec, na.rm = T), 
                          maximum = max(vec, na.rm = T),
                          avg = mean(vec, na.rm = T),
                          med = median(vec, na.rm =T),
                          std = sd(vec, na.rm = T),
                          iqr = IQR(vec, na.rm = T),
                          skew = moments::skewness(vec, na.rm = T),
                          kurt = moments::kurtosis(vec, na.rm = T)
                          )
  
  return(temp_summ)
}

# Summary stats of median bank integration for each SIC class
bank_SIC_summ_stat <- apply(bank_int_SIC_med, 2, func_summ)

# Boxplot of above
box_int_SIC_Categ <- boxplot(bank_int_SIC_med)

# Plots of median bank integration by SIC code

# matplot(bank_int_SIC_med, 
#         type = "l", 
#         lty = 1, 
#         lwd = 1, 
#         col = 1:4, 
#         xlab = "Quarters", 
#         ylab = "Median bank Intgeration"
#         )
# 
# legend(65, 0.4, 
#        legend = c("Comm", "Cred_Uni", "Saving", "Holding"), 
#        col = 1:4, 
#        lty = 1, 
#        cex=0.6
#        )

#################################################
### Changes in Bank Integration Levels ##########
#################################################

func_diff <- function(vec)
{
  #This function accepts a vector and returns
  #its first differenced vector with first term NA
  
  temp_diff <- diff(vec)
  return(c(NA, temp_diff))
}

# Store the matrix whose columns are differenced integration columns
int_diff_bank_wide <- apply(int_US_bank_wide[, -1], 2, func_diff)

int_diff_sys <- apply(int_US_systemic_wide[, -1], 2, func_diff)

int_US_sys_gsib <- int_US_bank_long %>% 
  dplyr::filter(Banks %in% name_GSIB)  %>% 
  tidyr::spread(., key = Banks, value = "Integration")

int_diff_sys_gsib <- apply(int_US_sys_gsib[, -1], 2, func_diff)

# matplot(int_diff_sys_gsib, 
#         type = "l", 
#         lty = 1, 
#         col = 1:ncol(int_diff_sys_gsib),
#         xlab = "Quarters",
#         ylab = "GSIBs Differenced Integration"
#         )
# 
# legend(60, -0.4, 
#        legend = name_GSIB, 
#        col = 1:length(name_GSIB), 
#        lty = 1, 
#        cex = 0.25
#        )



###

time_end <- Sys.time()

message("Tables and figures computed. Total time taken = ",
        round(time_end - time_start, 2),
        " min")