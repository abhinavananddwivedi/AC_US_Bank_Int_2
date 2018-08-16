time_start <- Sys.time()

### Results and Analysis: Integration Among US Banks ###

# Declare libraries
library(tidyverse)
library(moments)

# Read, tidy and preprocess the datasets from CRSP and Compustat 
source('Int_Banks_US.R', echo = F)
#includes another nested script file for preprocessing raw data
#takes ~120 seconds to run on Ubuntu 18.04 with 16GB RAM
#processor: Intel Core i5-4570 CPU @ 3.20GHz × 4 

func_neg_to_zero <- function(vec)
{
  # This function converts -ve values to 0
  temp <- which(vec < 0)
  vec[temp] <- 0
  return(vec)
}

int_US_bank_wide <- apply(integration_matrix_qtrly_out, 
                          2, 
                          func_neg_to_zero) %>%
  tibble::as_tibble()

int_US_bank_long <- int_mat_qtrly_out_long
temp_int_long <- func_neg_to_zero(int_mat_qtrly_out_long$Integration)
int_US_bank_long$Integration <- temp_int_long 

### Summary Statistics ###

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
readr::write_csv(int_summ_stat_qrtrly, "US_Bank_Intgration_Summary_Quarterly")

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
readr::write_csv(int_summ_stat_qrtrly, "US_Bank_Intgration_Summary_Bankwise")

###

### Median US Bank Integration Time Series ###
func_med <- function(vec)
{
  # This function returns medians after
  # coercing to numeric and ignoring NAs
  return(median(as.numeric(vec), na.rm = T))
}

# int_med_US_bank_qrtly <- apply(int_US_bank_wide[, -1], 1, func_med)
# 
# plot_data_median_int <- data.frame(Date = )
# 
# plot_int_US_qrtly <-  plot(int_US_bank_wide$Date, int_med_US_bank_qrtly,
#                            type = "l",
#                            col = "blue",
#                            xlab = "Time",
#                            ylab = "Median Integration"
# )

### Top 20 Banks by Median Integration




### Two Yearly Integration Boxplots ###

year_seq <- paste0(seq(year_min, year_max, 2), "Q4")

data_boxplot <- int_US_bank_long %>%
  dplyr::filter(Date %in% year_seq)

(boxplot_int_yearly <- ggplot(data = data_boxplot %>% dplyr::group_by(Date),
                              mapping = aes(x = Date, y = Integration)) +
    geom_boxplot(na.rm = T) +
    coord_flip() +
    theme_bw())
###


time_end <- Sys.time()
print(time_end - time_start)
