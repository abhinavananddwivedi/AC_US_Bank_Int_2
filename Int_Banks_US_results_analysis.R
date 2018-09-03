time_start <- Sys.time()

### Results and Analysis: Integration Among US Banks ###

# Declare libraries
library(tidyverse)
library(moments)

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
  
int_US_bank_long <- int_mat_qtrly_out_long
temp_int_long <- func_neg_to_zero(int_US_bank_long$Integration)
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
readr::write_csv(int_summ_stat_qrtrly, "US_Bank_Intgration_Summary_Quarterly.csv")

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

###

### Median US Bank Integration Time Series ###
func_med <- function(vec)
{
  # This function returns medians after
  # coercing to numeric and ignoring NAs
  return(median(vec, na.rm = T))
}

## Data in Wide Format ##
## Median Integration Levels Each Quarter ##

temp_int_med_bank <- integration_matrix_qtrly_out %>%
  dplyr::select(-Date) %>%
  apply(., 2, func_neg_to_zero) %>%
  apply(., 1, func_med)

date_med <- integration_matrix_qtrly_out$Date
year_end <- paste0(years, "Q4")

plot_data <- data.frame(date = as.factor(date_med), 
                        med_bank_int = as.numeric(temp_int_med_bank)
                        )

plot_med_bank_int <- ggplot(data = plot_data, 
                            mapping = aes(x = as.character(date), y = med_bank_int)) +
  geom_line(group = 1) +
  theme_bw() +
  labs(x = "Years", y = "Median Integration Level") +
  theme(axis.text.x=element_text(angle=60, hjust=1))

### Top 25 Banks by Median Integration

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

### Yearly Integration Boxplots ###
year_seq <- paste0(seq(year_min, year_max, 1), "Q4")

data_boxplot <- int_US_bank_long %>%
  dplyr::filter(Date %in% year_seq)

boxplot_int_yearly <- ggplot(data = data_boxplot %>% dplyr::group_by(Date),
                              mapping = aes(x = Date, y = Integration)) +
    geom_boxplot(na.rm = T) +
    theme_bw() + 
    theme(axis.text.x=element_text(angle=60, hjust=1)) 
###


time_end <- Sys.time()
print(time_end - time_start)
#takes ~120 seconds to run on Ubuntu 18.04 with 16GB RAM
#processor: Intel Core i5-4570 CPU @ 3.20GHz × 4 