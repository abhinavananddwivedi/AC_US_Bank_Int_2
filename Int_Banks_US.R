### Integration Among US Banks: A Reboot Using WRDS Data ###

library(tidyverse)

############################
### Directory_Management ###
############################

# For replication or reproduction of this script, change the address
# of the directories below to conform to their location in the host
# machine

###
time_start <- Sys.time()
###

data_folder_path <- "../Data_Bank_Int/"
file_name <- "SICCD_6020-6079_6710-6712_20171105.dta"
file_path <- paste0(data_folder_path, file_name)
  
### Read .dta file for US banks
data_US_full <- haven::read_dta(file_path)
# Note that since the data file is 2.7 GB, this step takes ~50 sec to run

data_US <- data_US_full %>% 
  dplyr::select(c(date, siccd, comnam, prc, ret)) %>%
  tibble::add_column(., qtr_num = NA) %>%
  dplyr::arrange(., comnam)

name_bank_full <- unique(data_US$comnam)

### Remove banks with fewer than median observations 
data_US_num_obs <- data_US %>%
  group_by(comnam) %>%
  count(.)

data_few_ind <- which(data_US_num_obs$n <= median(data_US_num_obs$n))
name_bank_few <- name_bank_full[data_few_ind]

data_US_bank <- data_US %>% 
  dplyr::filter(., comnam %in% name_bank_few == 0)

## Some summary statistics for bank returns ##

# Sample stats according to SICCD (industry classification)
summ_stat_siccd <- data_US_bank %>% 
  dplyr::group_by(siccd) %>% 
  dplyr::summarise(., 'minimum' = min(ret, na.rm = T), 
                   'maximum' = max(ret, na.rm = T), 
                   'avg' = mean(ret, na.rm = T), 
                   'med' = median(ret, na.rm = T), 
                   'std_dev' = sd(ret, na.rm = T), 
                   'iqr' = IQR(ret, na.rm = T)
                   )

# Sample stats according to comnam (bank name)
summ_stat_comnam <- data_US_bank %>% 
  dplyr::group_by(comnam) %>% 
  dplyr::summarise(., 'minimum' = min(ret, na.rm = T), 
                   'maximum' = max(ret, na.rm = T), 
                   'avg' = mean(ret, na.rm = T), 
                   'med' = median(ret, na.rm = T), 
                   'std_dev' = sd(ret, na.rm = T), 
                   'iqr' = IQR(ret, na.rm = T)
                   )

### Main script starts here onwards ##################################################

name_bank_US <- setdiff(name_bank_full, name_bank_few)
num_bank_US <- length(name_bank_US)

year_min <- min(lubridate::year(data_US_bank$date)) #year 1
year_max <- max(lubridate::year(data_US_bank$date)) #year end
num_years <- year_max - year_min + 1
years <- year_min:year_max

qtr_min <- 1
qtr_max <- 4*num_years
qtr_grid <- qtr_min:qtr_max

data_list_US <- rep(list(NULL), num_bank_US)

#these banks have inconsistent, duplicated data: ignore them
ind_bank_ignore <- c(57, 99, 128, 210, 217, 335, 388,
                     509, 559, 589, 597, 626, 934,
                     1051, 1300, 1361, 1679, 1691) 

ind_bank_use <- setdiff((1:num_bank_US), ind_bank_ignore)

for (i in ind_bank_use)
{
  temp <- data_US_bank %>% 
    dplyr::filter(comnam == name_bank_US[i]) %>%
    dplyr::distinct(.) #delete duplicate entries
  
  ### Quarterization of returns
  for (j in seq_along(years))
  {
    #Which rows belong to quarter number 1?
    q_1 <- which(lubridate::year(temp$date) == years[j] &
                      lubridate::month(temp$date) %in% c(1, 2, 3))
    # Which rows belong to quarter number 2?
    q_2 <- which(lubridate::year(temp$date) == years[j] &
                      lubridate::month(temp$date) %in% c(4, 5, 6))
    # Which rows belong to quarter number 3?
    q_3 <- which(lubridate::year(temp$date) == years[j] &
                      lubridate::month(temp$date) %in% c(7, 8, 9))
    # Which rows belong to quarter number 4?
    q_4 <- which(lubridate::year(temp$date) == years[j] &
                      lubridate::month(temp$date) %in% c(10, 11, 12))
    
    temp[q_1, "qtr_num"] <- 4*(j-1) + 1 
    temp[q_2, "qtr_num"] <- 4*(j-1) + 2
    temp[q_3, "qtr_num"] <- 4*(j-1) + 3
    temp[q_4, "qtr_num"] <- 4*(j-1) + 4
    # For year 1, quarters range from 1 to 4, for year 2 quarters range from 
    # 5 to 8, for year 3 quarters range from 9 to 12 etc. The above formula
    # qtr_num = 4*(year_num - 1) + 1, 2, 3, 4 etc. ensures correct ordering
  }
  
  data_list_US[[i]] <- temp
  
  ### List format to Data Matrix format conversion (for covariance matrices) ###
  
  # The following set of lines constructs a data matrix with banks as columns and
  # rows as daily returns
  if (i == 1) #for the first bank
  {
    temp_df <- data_list_US[[i]] %>% 
      dplyr::select(date, comnam, ret, qtr_num) %>%
      tidyr::spread(., comnam, ret)
  } else if (i >= 2) #for the other banks
  {
    temp_t <- data_list_US[[i]] %>% 
      dplyr::select(date, comnam, ret, qtr_num) %>%
      tidyr::spread(., comnam, ret)
    
    # Join to earlier frames repeatedly to get the full data matrix
    temp_df <- dplyr::full_join(temp_df, temp_t, by = c('date', 'qtr_num'))
  }
  
}

names(data_list_US) <- name_bank_US

data_list_US_df <- dplyr::bind_rows(data_list_US)

#####################################################################################
### Quarterly covariance matrices and their eigenvector computation #################
#####################################################################################

list_ret_banks <- rep(list(NULL), qtr_max)
list_cov <- rep(list(NULL), qtr_max)
list_eig_vec <- rep(list(NULL), qtr_max)
list_eig_val <- rep(list(NULL), qtr_max)
pc_out_of_sample <- rep(list(NULL), qtr_max)
var_share <- rep(list(NULL), qtr_max)

source('func_NA_killer.R', echo = F)

for (k in qtr_grid)
{
  # Isolate return matrix for that quarter
  temp_mat_q <- temp_df %>% 
    dplyr::filter(qtr_num == qtr_grid[k]) %>%
    dplyr::select(-c(date, qtr_num)) 
  
  temp_q <- func_NA_killer(temp_mat_q) #kill the full NA columns and rows
  
  ### Further Cleaning---Partial NA filling for covariance matrices ###
  
  # The number of NAs in each column is reported by the following line
  num_NA_col <- colSums(is.na(temp_q[, which(colSums(is.na(temp_q)) > 0)]) > 0)
  num_obs <- nrow(temp_q)
  num_miss_obs <- num_obs - num_NA_col
  
  temp_q[ , which(num_miss_obs >= num_obs/2)] <- NA
  
  temp_q <- func_NA_killer(temp_q)
  
  temp_q_remain_NA_col <- which(colSums(is.na(temp_q)) > 0)
  
  temp_med <- apply(temp_q[, temp_q_remain_NA_col], 2, median, na.rm = T)
  
  temp_stale_ret_col <- which(temp_med == 0)
  
  temp_q[ , temp_stale_ret_col] <- NA
  
  temp_q <- func_NA_killer(temp_q)

  
  ###
  
  # Store quarterly bank returns for quarterly regressions
  list_ret_banks[[k]] <- temp_q
  
  # Compute covariance matrices 
  list_cov[[k]] <- temp_q %>% 
    as.data.frame(.) %>% 
    cov(., use = "complete.obs")
  
  # Compute eigenvectors (already sorted top to bottom by default)
  list_eig_vec[[k]] <- eigen(list_cov[[k]], symmetric = T)$vectors
  
  # Compute eigenvalues (already sorted top to bottom by default)
  list_eig_val[[k]] <- eigen(list_cov[[k]], symmetric = T)$values
  
  # Variance contribution of first eigenvector is \lambda_1/(sum over \lambda)
  var_share[[k]] <- cumsum(list_eig_val[[k]])/sum(list_eig_val[[k]]) 
  names(var_share[[k]]) <- paste0("Lambda_", 1:ncol(list_eig_vec[[k]])) 
  
}



#####################################################################################
time_stop <- Sys.time()
#(time_stop - time_start)
