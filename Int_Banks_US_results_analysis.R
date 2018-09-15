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

int_US_bank_wide <- int_US_bank_long %>%
  tidyr::spread(., key = "Banks", value = "Integration")

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

### Histograms for Bank Integration ###

# By Banks
# hist_banks <- rep(list(NULL), ncol(int_US_bank_wide))
# 
# for (i in 1:(ncol(int_US_bank_wide)-1))
# {
#   hist_banks[[i]] <- hist(int_US_bank_wide[, i+1])
# }
# names(hist_banks) <- names(int_US_bank_wide[-1])
# 
# # By Quarters
# hist_qtr <- rep(list(NULL), nrow(int_US_bank_wide))
# 
# for (j in 1:(nrow(int_US_bank_wide)))
# {
#   temp_temp <- int_US_bank_wide[j, -1]
#   hist_qtr[[j]] <- hist(as.numeric(temp_temp))
# }

# Animate quarterly histograms---no evidence of special distribution
# for (i in 1:(qtr_max-1))
# {
#   plot(hist_qtr[[i]])
# }

### Median US Bank Integration Time Series ###
func_med <- function(vec)
{
  # This function returns medians after ignoring NAs
  return(median(vec, na.rm = T))
}

## Data in Wide Format ##
## Median Integration Levels Each Quarter ##

temp_int_med_bank <- integration_matrix_qtrly_out %>%
  dplyr::select(-Date) %>%
  apply(., 2, func_neg_to_zero) %>%
  apply(., 1, func_med)

qtrs <- qtr_grid[-1]

trend_linear_int <- summary(lm(temp_int_med_bank ~ qtrs))

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

### Explanatory power of eigenvectors ###

func_len_max_NA_add <- function(temp_list)
{ # This function takes a list of differing lengths
  # then finds the maximum length of some element in the list
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

## The explanatory power of the top thirty eigenvectors

#box_expl_eig <- boxplot(t(var_share_df[1:30, ]))
#bar_expl_eig_med <- barplot(expl_power_eig_med[1:30])

### Fitting a liner time trend to each bank's integration series ###

int_LHS <- integration_matrix_qtrly_out[, -1] 
int_LHS_missing <- apply(int_LHS, 2, func_missing)

tol_LHS <- 0.80

int_LHS <- int_LHS[, which(int_LHS_missing < tol_LHS*nrow(int_LHS))] %>%
  as.matrix(.)

qtr_RHS <- qtr_grid[-1]

func_lm <- function(vec)
{
  temp_lm <- lm(vec ~ qtr_RHS)
  return(summary(temp_lm))
}

trend_linear_int_banks <- apply(int_LHS, 2, func_lm)

## Extracting T stats and p values for bank time trends

trend_int_bank_coeff <- lapply(trend_linear_int_banks, `[[`, "coefficients")

func_subset <- function(matrix)
{
  return(matrix[-1, c(3,4)])
}

trend_T_p_val <- lapply(trend_int_bank_coeff, func_subset)

trend_T_p_val_df <- t(dplyr::bind_rows(trend_T_p_val)) %>% dplyr::as_tibble()
names(trend_T_p_val_df) <- c("T_stat", "p_val")

trend_T_p_val_df <- tibble::add_column(trend_T_p_val_df, 
                                       "Banks" = names(trend_T_p_val)) %>%
  dplyr::select(Banks, everything())
  

###

time_end <- Sys.time()
print(time_end - time_start)