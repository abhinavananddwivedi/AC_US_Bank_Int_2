time_start <- Sys.time()

########################################################
### Results and Analysis: Integration Among US Banks ###
########################################################

# Declare libraries
library(tidyverse)
library(moments)
library(zoo)
library(lmtest)
library(sandwich)

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
  dplyr::summarise(., 
                   'minimum' = min(Integration, na.rm = T), 
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

func_trend_NW <- function(df)
{
  # This function computes the linear trend and reports
  # heteroskedasticity and autocorrelation consistent errors
  # according to Newey West
  temp_lm <- lm(Integration ~ Qtr_num, data = df)
  temp_summ <- summary(temp_lm)
  temp_vcov_err <- sandwich::NeweyWest(temp_lm, 
                                       lag = 2, 
                                       prewhite = F,
                                       adjust = T)
  temp_summ$coefficients <- unclass(lmtest::coeftest(temp_lm,
                                                     vcov. = temp_vcov_err)
                                    )
  
  return(temp_summ)
}

func_missing_col_int <- function(data_frame)
{ # This function counts the number of missing values of
  # the column corresponding to integration in some data
  # frame. It uses the previously defined function "func_missing(vec)"
  return(func_missing(data_frame$Integration))
}

# Linear Trends for the full set of banks #

# Compute linear trends and NW errors
nest_bank_int_US_full <- nest_bank_int_US_full %>%
  dplyr::mutate("Missing" = purrr::map(data, func_missing_col_int)) %>%
  dplyr::mutate("Trend" = purrr::map(data, func_trend_NW))

# Display trend results
nest_bank_int_US_full_results <- nest_bank_int_US_full %>%
  dplyr::mutate("Model_Summary" = map(Trend, broom::tidy)) %>%
  tidyr::unnest(Model_Summary) %>%
  dplyr::select(-term) %>%
  dplyr::slice(., seq(2, nrow(nest_bank_int_US_full), by = 2)) 

# Filter banks with significant trends
trend_bank_sig_full <- nest_bank_int_US_full_results %>%
  dplyr::select(Banks, p.value) %>%
  dplyr::filter(p.value <= 0.10)

# Linear Trends pre 2005 #

nest_bank_int_US_H1 <- nest_bank_int_US_H1 %>%
  dplyr::mutate("Missing" = purrr::map(data, func_missing_col_int))

nest_bank_int_US_H1_results <- nest_bank_int_US_H1 %>%
  dplyr::filter(Missing <= 40) %>% #ignore if more than 40(/50) missing obs
  dplyr::mutate("Trend" = purrr::map(data, func_trend_NW)) %>%
  dplyr::mutate("Model_Summary" = map(Trend, broom::tidy)) %>%
  tidyr::unnest(Model_Summary) %>%
  dplyr::select(-term) %>%
  dplyr::slice(., seq(2, nrow(nest_bank_int_US_full), by = 2)) 

trend_bank_sig_H1 <- nest_bank_int_US_H1_results %>%
  dplyr::select(Banks, p.value) %>%
  dplyr::filter(p.value <= 0.10)

# Linear Trends post 2005 #

nest_bank_int_US_H2 <- nest_bank_int_US_H2 %>%
  dplyr::mutate("Missing" = purrr::map(data, func_missing_col_int))

nest_bank_int_US_H2_results <- nest_bank_int_US_H2 %>%
  dplyr::filter(Missing <= 40) %>% #ignore if more than 40(/50) missing obs
  dplyr::mutate("Trend" = purrr::map(data, func_trend_NW)) %>%
  dplyr::mutate("Model_Summary" = map(Trend, broom::tidy)) %>%
  tidyr::unnest(Model_Summary) %>%
  dplyr::select(-term) %>%
  dplyr::slice(., seq(2, nrow(nest_bank_int_US_full), by = 2)) 

trend_bank_sig_H2 <- nest_bank_int_US_H2_results %>%
  dplyr::select(Banks, p.value) %>%
  dplyr::filter(p.value <= 0.10)

## Linear trend for the median bank ##

int_median_US_bank <- int_US_bank_long_2 %>%
  dplyr::group_by(Qtr_num) %>%
  dplyr::summarise('Integration' = median(Integration, na.rm = T))
  
trend_median_US_bank <- func_trend_NW(int_median_US_bank)

# Linear trend for for the median bank: pre and post 2005 #

# Pre 2005
int_median_US_bank_H1 <- int_US_bank_long_2 %>%
  dplyr::filter(Qtr_num <= 50) %>%
  dplyr::group_by(Qtr_num) %>%
  dplyr::summarise('Integration' = median(Integration, na.rm = T))

trend_median_US_bank_H1 <- func_trend_NW(int_median_US_bank_H1)

# Post 2005
int_median_US_bank_H2 <- int_US_bank_long_2 %>%
  dplyr::filter(Qtr_num > 50) %>%
  dplyr::group_by(Qtr_num) %>%
  dplyr::summarise('Integration' = median(Integration, na.rm = T))

trend_median_US_bank_H2 <- func_trend_NW(int_median_US_bank_H2)

## Plotting median bank's integration trend ##

# Full
plot_trend_median <- ggplot(int_median_US_bank,
                                aes(Qtr_num, Integration)) +
  geom_line() +
  geom_smooth(method = "lm", 
              linetype = "dashed", 
              color = "black") +
  theme_bw() +
  theme(text = element_text(size = 20))

x_breaks <- seq(qtr_grid[4], qtr_grid[100], by = 8)
x_labels <- paste0(seq(1993, 2017, by = 2), "Q4")

## High quality plot used in the paper ##
plot_trend_median_HQ <- plot_trend_median +
  geom_point() +
  scale_x_continuous(breaks = x_breaks,
                     labels = x_labels) +
  labs(x = "") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

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

## Plot showing pre- versus post-2005 ##
int_median_US_bank_H1H2 <- int_median_US_bank %>%
  mutate("Period" = c(rep("H1", 49), rep("H2", 50)))

ggplot(int_median_US_bank_H1H2, aes(Qtr_num, Integration)) +
  geom_point() +
  geom_line() +
  geom_smooth(aes(group = Period),
              method = "lm",
              linetype = "dashed",
              color = "black") +
  theme_bw() +
  scale_x_continuous(breaks = x_breaks,
                     labels = x_labels) +
  labs(x = "Years", size = 14) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 12))


#######################
### Bank subsamples ###
#######################

####################################
### Systemically important banks ###
####################################

name_GSIB <- name_cusip$comnam[c(28, 33, 36, 73, 183, 184, 304, 305, 347)]
name_DSIB <- name_cusip$comnam[c(14, 87, 114, 173, 187, 188, 195, 223, 245, 
                                 246, 247, 277, 278, 311, 335, 361)]

name_systemic <- c(name_GSIB, name_DSIB)

# Systemic bank trends full sample
int_US_systemic_trend <- nest_bank_int_US_full_results %>%
  dplyr::filter(Banks %in% name_systemic)

# Systemic bank trends: first and second half
int_US_systemic_trend_H1 <- nest_bank_int_US_H1_results %>%
  dplyr::filter(Banks %in% name_systemic)
int_US_systemic_trend_H2 <- nest_bank_int_US_H2_results %>%
  dplyr::filter(Banks %in% name_systemic)

# Median systemic bank
int_median_US_bank_systemic <- int_US_bank_long_2 %>%
  dplyr::filter(Banks %in% name_systemic) %>%
  dplyr::group_by(Qtr_num) %>%
  dplyr::summarise('Integration' = median(Integration, na.rm = T))

trend_median_US_bank_systemic <- func_trend_NW(int_median_US_bank_systemic)

## Plot used in paper: Median systemic vs median US bank integration ##
temp_temp <- int_median_US_bank %>%
  tibble::add_column("Integration_sys" = int_median_US_bank_systemic$Integration)
temp_temp_long <- temp_temp %>%
  dplyr::rename("Median_Bank" = Integration,
                "Median_Systemic_Bank" = Integration_sys) %>%
  tidyr::gather(c(Median_Bank, Median_Systemic_Bank),
                key = "Bank",
                value = "Integration")

plot_med_US_med_sys <- ggplot(temp_temp_long, aes(Qtr_num,
                                                  Integration)) +
  geom_point(aes(shape = Bank)) +
  geom_line(aes(linetype = Bank)) +
  theme_bw() +
  scale_x_continuous(breaks = x_breaks, labels = x_labels) +
  labs(y = "Median Integration", x = NULL) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  theme(text = element_text(size = 20))


# Linear trend for for the median systemic bank: pre and post 2005 #

# Pre 2005
int_median_US_bank_systemic_H1 <- int_US_bank_long_2 %>%
  dplyr::filter(Banks %in% name_systemic) %>%
  dplyr::filter(Qtr_num <= 50) %>%
  dplyr::group_by(Qtr_num) %>%
  dplyr::summarise('Integration' = median(Integration, na.rm = T))

trend_median_US_bank_systemic_H1 <- func_trend_NW(int_median_US_bank_systemic_H1)

# Post 2005
int_median_US_bank_systemic_H2 <- int_US_bank_long_2 %>%
  dplyr::filter(Banks %in% name_systemic) %>%
  dplyr::filter(Qtr_num > 50) %>%
  dplyr::group_by(Qtr_num) %>%
  dplyr::summarise('Integration' = median(Integration, na.rm = T))

trend_median_US_bank_systemic_H2 <- func_trend_NW(int_median_US_bank_systemic_H2)

## Plotting trends of median systemic bank ##

# Full
plot_trend_median_sys <- ggplot(int_median_US_bank_systemic,
                                aes(Qtr_num, Integration)) +
  geom_line() +
  geom_smooth(method = "lm", 
              linetype = "dashed", 
              color = "black") +
  theme_bw()

# First Half
plot_trend_median_sys_H1 <- ggplot(int_median_US_bank_systemic_H1,
                                   aes(Qtr_num, Integration)) +
  geom_line() +
  geom_smooth(method = "lm", 
              linetype = "dashed", 
              color = "black") +
  theme_bw()

# Second Half
plot_trend_median_sys_H2 <- ggplot(int_median_US_bank_systemic_H2,
                                   aes(Qtr_num, Integration)) +
  geom_line() +
  geom_smooth(method = "lm", 
              linetype = "dashed", 
              color = "black") +
  theme_bw()


### Top and bottom 50 banks based on median integration ###
## Arrange banks from top to bottom ##

summ_stat_med_bank <- int_summ_stat_bank %>%
  dplyr::select(Banks, med) %>%
  dplyr::rename("Median_Integration" = med) %>%
  dplyr::mutate("Rank" = rank(desc(Median_Integration))) %>%
  dplyr::arrange(Rank)

summ_stat_med_bank_top_50 <- summ_stat_med_bank %>%
  dplyr::filter(Rank <= 50) #top 50 most integrated banks
name_bank_top_50 <- summ_stat_med_bank_top_50 %>%
  dplyr::select(Banks)
summ_stat_med_bank_bot_50 <- summ_stat_med_bank %>%
  dplyr::filter(Rank >= num_bank$n - 50)
name_bank_bot_50 <- summ_stat_med_bank_bot_50 %>%
  dplyr::select(Banks)

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
    labs(x = NULL) +
    theme(axis.text.x=element_text(angle=60, hjust=1)) +
    theme(text = element_text(size = 20))

# Quarterly Integration Boxplots

boxplot_int_qtrly <- ggplot(data = int_US_bank_long_2,
                             mapping = aes(x = Date, y = Integration)) +
  geom_boxplot(na.rm = T) +
  # scale_x_continuous(breaks = seq(qtr_min, qtr_max, by = 8)) +
  theme_bw() + 
  theme(axis.text.x=element_text(angle=60, hjust=1)) 

# Quarterly integration post 2005 boxplots


temp_date <- as.factor(unique(int_US_bank_long_2$Date))
temp_date_post_05 <- temp_date[-c(1:49)]

df_rect_crises <- data.frame(x_l = c(60, 70),
                           x_r = c(66, 78),
                           y_l = c(0, 0),
                           y_r = c(1, 1))
# 
# df_rect_crises <- data.frame(x_l = c("2007Q4", "2010Q2"),
#                              x_r = c("2009Q2", "2012Q2"),
#                              y_l = c(0, 0),
#                              y_r = c(1, 1))


temp_plot_post_05 <- ggplot(filter(int_US_bank_long_2, 
                                   Date %in% temp_date_post_05),
                            aes(x = Date, y = Integration)) +
  geom_boxplot(na.rm = T) +
  labs(x = NULL) +
  theme_bw() + 
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  theme(text = element_text(size = 20))

###
# temp_temp <- int_US_bank_long_2 %>%
#   mutate('Qtr_num_2' = as.factor(Qtr_num))
# 
# geom_rect(data = df_rect_crises, 
#           mapping = aes(xmin = x_l, 
#                         xmax = x_r, 
#                         ymin = y_l, 
#                         ymax = y_r),
#           color = "grey",
#           alpha = 0.2) +

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

# boxplot(t(var_share_df[1:30, ]), 
#         xlab = "Number of principal components",
#         ylab = "Proportion of variance explained",
#         cex.lab=1.5, 
#         cex.axis=1.5, 
#         cex.main=1.5, 
#         cex.sub=1.5)

###################################
### Relation to Crises ############
###################################

# The Great Recession: Q4 2007--Q2 2009, Quarters 60:66

dummy_GR <- rep(0, qtr_max) 
dummy_GR[60:66] <- 1
  
# The Eurozone crisis: Q2 2010--Q2 2012: Quarters 70--78

dummy_EZ <- rep(0, qtr_max) 
dummy_EZ[70:78] <- 1

temp_GR_rep <- rep(dummy_GR[-1], num_bank$n)
temp_EZ_rep <- rep(dummy_EZ[-1], num_bank$n)

int_US_bank_long_3 <- int_US_bank_long_2 %>%
  tibble::add_column("GR" = temp_GR_rep) %>%
  tibble::add_column("EZ" = temp_EZ_rep)

nest_bank_int_US_full_crisis <- int_US_bank_long_3 %>%
  dplyr::group_by(Banks) %>%
  tidyr::nest(.)

func_trend_NW_crisis <- function(df)
{
  # This function computes the linear trend and reports
  # heteroskedasticity and autocorrelation consistent errors
  # according to Newey West during crises
  temp_lm <- lm(Integration ~ Qtr_num + GR + EZ, data = df)
  temp_summ <- summary(temp_lm)
  temp_vcov_err <- sandwich::NeweyWest(temp_lm,
                                       lag = 2,
                                       prewhite = F,
                                       adjust = T)
  temp_summ$coefficients <- unclass(lmtest::coeftest(temp_lm,
                                                     vcov. = temp_vcov_err)
  )

  return(temp_summ)
}

# Compute linear trends and NW errors
nest_bank_int_US_full_crisis_results <- nest_bank_int_US_full_crisis %>%
  dplyr::mutate("Missing" = purrr::map(data, func_missing_col_int)) %>%
  dplyr::filter(Missing <= 80) %>%
  dplyr::mutate("Trend" = purrr::map(data, func_trend_NW_crisis)) %>%
  dplyr::mutate("Model_Summary" = map(Trend, broom::tidy)) %>%
  tidyr::unnest(Model_Summary) %>%
  dplyr::filter(term != "(Intercept)")

## The Great Recession ##
# Isolate the coefficients for GR
trend_bank_full_GR <- nest_bank_int_US_full_crisis_results %>%
  dplyr::filter(term == "GR")
# Isolate systemic banks
trend_bank_sys_GR <- trend_bank_full_GR %>%
  dplyr::filter(Banks %in% name_systemic)

# Banks with significant trends during GR
trend_bank_sig_full_GR <- trend_bank_full_GR %>%
  dplyr::filter(p.value <= 0.10)

## The Eurozone Crisis ##
# Isolate the coefficients for GR
trend_bank_full_EZ <- nest_bank_int_US_full_crisis_results %>%
  dplyr::filter(term == "EZ")
# Isolate systemic banks
trend_bank_sys_EZ <- trend_bank_full_EZ %>%
  dplyr::filter(Banks %in% name_systemic)

# Banks with significant trends during GR
trend_bank_sig_full_EZ <- trend_bank_full_EZ %>%
  dplyr::filter(p.value <= 0.10)

### Linear trend of the median bank during crisis ###

int_median_US_bank_crisis <- int_median_US_bank %>%
  tibble::add_column("GR" = dummy_GR[-1]) %>%
  tibble::add_column("EZ" = dummy_EZ[-1])

trend_median_US_bank_crisis <- func_trend_NW_crisis(int_median_US_bank_crisis)


# ### Linear trend of the median systemic bank during crisis ###
# int_median_US_bank_sys_crisis <- int_median_US_bank_systemic %>%
#   tibble::add_column("GR" = dummy_GR[-1]) %>%
#   tibble::add_column("EZ" = dummy_EZ[-1])
# 
# trend_median_US_bank_sys_crisis <- func_trend_NW_crisis(int_median_US_bank_sys_crisis)

### Spread between median US and median US systemic bank

int_spread_med <- int_median_US_bank %>%
  dplyr::mutate("Integration_Med_Sys" = int_median_US_bank_systemic$Integration) %>%
  dplyr::mutate("Spread" = Integration_Med_Sys - Integration)

plot_spread <- ggplot(int_spread_med, aes(Qtr_num, Spread)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dotdash") +
  labs(x = NULL) +
  theme_bw() +
  scale_x_continuous(breaks = x_breaks, labels = x_labels) +
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  theme(text = element_text(size = 20))
  
##########################################################################
############### Policy Implications ######################################
##########################################################################
  
int_US_bank_crises <- int_US_bank_long_3 %>%
  dplyr::mutate("Crisis_Ind" = temp_GR_rep + temp_EZ_rep) %>%
  dplyr::filter(Crisis_Ind == 1) %>%
  dplyr::select(Integration)

int_US_bank_no_crises <- int_US_bank_long_3 %>% 
  dplyr::mutate("Crisis_Ind" = temp_GR_rep + temp_EZ_rep) %>%
  dplyr::filter(Crisis_Ind == 0) %>%
  dplyr::select(Integration)

## Test for equality of (pooled) variances for crises vs non-crises
## Note that we are winsorizing both samples before applying the test

# sample x: from the crisis episodes
temp_x <- int_US_bank_crises %>%
  dplyr::select(Integration) %>%
  dplyr::filter(!(is.na(.))) %>%
  robustHD::winsorize(.)

# sample y: from non crisis episodes
temp_y <- int_US_bank_no_crises %>%
  dplyr::select(Integration) %>%
  dplyr::filter(!(is.na(.))) %>%
  robustHD::winsorize(.)

var_test_policy <- var.test(temp_x, temp_y, alternative = "less")

## Test of equality of (pooled) means for crises vs non-crises
# T test (parametric)
mean_test_policy <- t.test(temp_x, temp_y, alternative = "greater")
# (Mann-Whitney) Wilcoxon test (nonparametric)
mean_test_policy_wilcox <- wilcox.test(temp_x, temp_y, alternative = "greater")
# KS test: checking if cdf of crisis integration dominates noncrisis
dist_test_ks <- ks.test(temp_x, temp_y, alternative = "less")

###########################################################
### Systemic vs ordinary banks in crises and non-crises ###
###########################################################

df_rect_crises <- data.frame(x_1 = c(60, 70),
                             x_2 = c(66, 78),
                             y_1 = c(0.3, 0.3),
                             y_2 = c(1, 1))


plot_med_US_med_sys_bar_rec <- ggplot() +
  geom_rect(data = df_rect_crises,
            mapping = aes(xmin = x_1, 
                          xmax = x_2, 
                          ymin = y_1, 
                          ymax = y_2),
            color = "grey",
            alpha = 0.1) +
  geom_line(data = temp_temp_long,
            mapping = aes(x = Qtr_num, 
                          y = Integration,
                          linetype = Bank)) +
  labs(x = NULL) +
  theme_bw() +
  scale_x_continuous(breaks = x_breaks, labels = x_labels) +
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  theme(text = element_text(size = 20))


##############################################################
## Plotting medians along with recession bars ################
##############################################################

temp <- int_spread_med %>%
  dplyr::mutate("Crisis_Ind" = dummy_GR[-1] + dummy_EZ[-1])

df_geom_rect <- data.frame(x_l = c(60, 70),
                           x_r = c(66, 78),
                           y_l = c(-.25, -.25),
                           y_r = c(.25, .25))

plot_spread_policy <- ggplot() + 
  geom_line(data = temp, 
            mapping = aes(x = Qtr_num, y = Spread)) +
  geom_rect(data = df_geom_rect, 
            mapping = aes(xmin = x_l, 
                          xmax = x_r, 
                          ymin = y_l, 
                          ymax = y_r),
            color = "grey",
            alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dotdash") +
  labs(x = NULL) +
  theme_bw() +
  scale_x_continuous(breaks = x_breaks, labels = x_labels) +
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  theme(text = element_text(size = 20))

##############################################################

###

time_end <- Sys.time()

message("Tables and figures computed. Total time taken = ",
        round(time_end - time_start, 2),
        " min")