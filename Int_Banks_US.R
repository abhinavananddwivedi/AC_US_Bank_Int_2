### Integration Among US Banks: A Reboot Using WRDS Data ###

library(tidyverse)

### Function Declarations #####################################################################

# This function kills a data frame's full NA columns and rows, in that order.
# It take as input a data matrix and returns it after deleting (first) full NA 
# columns and (then) full NA rows.
func_full_NA_killer <- function(data_matrix)
{
  # full NA column killer
  temp_no_NA_col <- data_matrix[, colSums(is.na(data_matrix)) < nrow(data_matrix)]
  
  # full NA row killer
  temp_no_NA_col_row <- temp_no_NA_col[rowSums(is.na(temp_no_NA_col)) != 
                                         ncol(temp_no_NA_col), ]
  
  return(temp_no_NA_col_row) 
}

# This function returns the number of 0s in a vector ignoring NAs.
func_stale <- function(vec)
{
  temp_vec <- vec[!is.na(vec)]
  temp_sum <- sum(temp_vec == 0)
  return(temp_sum)
}

# This function replaces missing values of a vector with its median
func_part_NA_filler <- function(vec)
{
  vec_med <- median(vec, na.rm = T)
  vec_NA <- is.na(vec)
  vec[vec_NA] <- vec_med

  return(vec)
}

# This function takes a data matrix and removes columns with a high proportion
# of missing or stale entries. (Stale entries have return 0.) Any columns
# with leftover missing entries are replaced with their respective medians.
#
# The inputs are the data matrix and the critical threshold (in [0,1]) defining 
# "too high". The default is alpha = 0.5. This function depends on three other 
# self-defined functions func_stale(), func_NA_killer() and func_part_NA_filler()
func_high_stale_NA_filler <- function(temp_matrix, alpha = 0.5)
{ 
  # Location of columns with more than alpha proportion of missing observations
  col_high_NA <- which(colSums(is.na(temp_matrix)) > nrow(temp_matrix)*alpha)
  
  # Stale entries
  num_stale_col <- apply(temp_matrix, 2, func_stale) #number of 0s column-wise
  col_high_stale <- which(num_stale_col >= nrow(temp_matrix)*alpha) #highly stale columns
  
  temp_matrix[, c(col_high_NA, col_high_stale)] <- NA #ignore such columns
  
  temp_matrix <- func_full_NA_killer(temp_matrix) #kill full NA columns, then rows (if any)
  
  temp_matrix <- apply(temp_matrix, 2, func_part_NA_filler)
  
  return(temp_matrix)
}

### Function Declaration Over ###########################################################

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
# Note that since the data file is 2.7 GB, this step 
# takes ~50 sec to run on this desktop with 16GB RAM

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

# These banks have inconsistent, duplicated data: ignore them
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

data_list_US_df <- dplyr::bind_rows(data_list_US) #in panel format

#####################################################################################
### Quarterly covariance matrices and their eigenvector computation #################
#####################################################################################

list_ret_banks <- rep(list(NULL), qtr_max)
list_cov <- rep(list(NULL), qtr_max)
list_eig_vec <- rep(list(NULL), qtr_max)
list_eig_val <- rep(list(NULL), qtr_max)
pc_out_of_sample <- rep(list(NULL), qtr_max)
var_share <- rep(list(NULL), qtr_max)

for (k in qtr_grid)
{
  # Isolate return matrix for that quarter
  temp_mat_q <- temp_df %>% 
    dplyr::filter(qtr_num == qtr_grid[k]) %>%
    dplyr::select(-c(date, qtr_num)) 
  
  temp_q <- func_full_NA_killer(temp_mat_q) #kill the full NA columns and rows
  
  temp_q <- func_high_stale_NA_filler(temp_q) #stale killer, partial NA filler
  
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
  
  ### Out-of-sample Principal Component Computation #################################

  if (k < qtr_max) #for all quarters except the last one
  {
    # The returns for the subsequent quarter will be used to compute out of sample
    # principal components.
    temp_q_subsequent <- temp_df %>% dplyr::filter(qtr_num == qtr_grid[k+1]) %>%
      dplyr::select(-c(date, qtr_num))

    # Kill the NA columns and rows and save as matrix
    temp_q_subsequent <- func_full_NA_killer(temp_q_subsequent) %>% 
      func_high_stale_NA_filler(.) %>%
      as.matrix(.)

    # Current quarter's returns
    temp_q_current <- temp_df %>% dplyr::filter(qtr_num == qtr_grid[k]) %>%
      dplyr::select(-c(date, qtr_num))

    temp_q_current <- func_full_NA_killer(temp_q_current) %>% 
      func_high_stale_NA_filler(.) %>%
      as.matrix(.)

    # When number of usable banks is the same in two subsequent quarters
    if (ncol(temp_q_subsequent) == ncol(list_eig_vec[[k]]))
    {
      pc_out_of_sample[[k+1]] <- temp_q_subsequent%*%list_eig_vec[[k]]
      #(CHECK FOR NA PROLIFERATION ISSUES)
    } else if (ncol(temp_q_current) == ncol(list_eig_vec[[k]]))
    {
      pc_out_of_sample[[k+1]] <- temp_q_current%*%list_eig_vec[[k]]
    }
    ### CAN WE GET RID OF THIS WORKAROUND? THINK HARDER ###

  }

  
  
  ###################################################################################
  
}



#####################################################################################
time_stop <- Sys.time()
#(time_stop - time_start)
