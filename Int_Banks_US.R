### Integration Among US Banks: A Reboot Using WRDS Data ###

# Declare libraries
library(tidyverse)
library(Matrix)

# Read, tidy and preprocess the datasets from CRSP and Compustat 
source('Int_Banks_US_data_preprocessing.R', echo = F)
#takes ~90 seconds to run on Ubuntu 16.04 with 16GB RAM
#Processor: Intel Core i5-4570 CPU @ 3.20GHz × 4 

############################################################################
### The Main Script Starts Here ############################################
############################################################################

# US bank names in our sample
name_bank_US <- data_US_bank %>%
  dplyr::select(comnam) %>%
  dplyr::distinct(.)

# Number of banks
num_bank_US <- nrow(name_bank_US)

year_min <- min(lubridate::year(data_US_bank$date)) #year 1
year_max <- max(lubridate::year(data_US_bank$date)) #year end
num_years <- year_max - year_min + 1
years <- year_min:year_max

qtr_min <- 1
qtr_max <- 4*num_years
qtr_grid <- qtr_min:qtr_max

data_list_US <- rep(list(NULL), num_bank_US)

for (i in 1:num_bank_US)
{
  temp <- data_US_bank %>% 
    dplyr::filter(comnam == name_bank_US$comnam[i]) %>%
    dplyr::distinct(.) #delete duplicate entries if any
  
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
  
  ### List format to Wide Data Matrix format conversion (for covariance matrices) ###
  
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

data_list_US_df <- dplyr::bind_rows(data_list_US) #in panel format

# Bank name, CUSIP, NCUSIP and SIC map
name_cusip_sic <- data_list_US_df %>%
  dplyr::select(comnam, ncusip, cusip_8, siccd) %>%
  dplyr::distinct()

# Bank name, CUSIP, NCUSIP map
name_cusip <- data_list_US_df %>%
  dplyr::select(comnam, ncusip, cusip_8) %>%
  dplyr::distinct()

### Functions for detecting and/or replacing missing values ##########################

func_full_NA_killer <- function(data_matrix)
{
  # This function kills a data frame's full NA columns and rows, in that order.
  # It take as input a data matrix and returns it after deleting (first) full NA
  # columns and (then) full NA rows.
  
  # full NA column killer
  temp_no_NA_col <- data_matrix[, colSums(is.na(data_matrix)) < nrow(data_matrix)]
  
  # full NA row killer
  temp_no_NA_col_row <- temp_no_NA_col[rowSums(is.na(temp_no_NA_col)) !=
                                         ncol(temp_no_NA_col), ]
  
  return(temp_no_NA_col_row)
}


func_stale <- function(vec)
{
  # This function returns the number of 0s in a vector ignoring NAs.
  
  temp_vec <- vec[!is.na(vec)]
  temp_sum <- sum(temp_vec == 0)
  return(temp_sum)
}

func_missing <- function(vec)
{
  # This function returns number of missing values in a vector
  return(sum(is.na(vec)))
}

func_NA_filler <- function(vec)
{
  # This function fills a vector's missing values with its median 
  vec[is.na(vec)] <- median(vec, na.rm = T)
  return(vec)
}

#####################################################################################
### Quarterly covariance matrices and their eigenvector computation #################
#####################################################################################

list_ret_banks <- rep(list(NULL), qtr_max)
list_cov <- rep(list(NULL), qtr_max)
list_eig_vec <- rep(list(NULL), qtr_max)
list_eig_val <- rep(list(NULL), qtr_max)
var_share <- rep(list(NULL), qtr_max)
pc_out_of_sample <- rep(list(NULL), qtr_max)
pc_in_sample <- rep(list(NULL), qtr_max)

for (k in qtr_grid)
{
  # Isolate return matrix for that quarter
  temp_mat_q <- temp_df %>% 
    dplyr::filter(qtr_num == qtr_grid[k]) %>%
    dplyr::select(-c(date, qtr_num)) 
  
  temp_q <- func_full_NA_killer(temp_mat_q) #kill the full NA columns and rows
  
  # Ignore those columns which have too many stale or missing entries
  ncol_stale <- apply(temp_q, 2, func_stale)
  ncol_NA <- apply(temp_q, 2, func_missing)
  
  # Ignore if stale+missing is more than tolerance fraction of the sample
  tol_NA_stale = 0.30
  temp_q <- temp_q[, ncol_NA+ncol_stale <= floor(tol_NA_stale*nrow(temp_q))]
  
  # Replace still remnant NA values with medians
  temp_q <- apply(as.matrix(temp_q), 2, func_NA_filler) %>%
    as.data.frame()
  
  # Store quarterly bank returns for quarterly regressions
  list_ret_banks[[k]] <- temp_q
  
  # Compute covariance matrices 
  temp_cov <- temp_q %>% 
    cov(., use = "pairwise.complete.obs")
  
  #Pick nearest positive definite matrix
  temp_cov <- Matrix::nearPD(temp_cov)

  list_cov[[k]] <- as.matrix(temp_cov$mat)
  
  list_eig_val[[k]] <- temp_cov$eigenvalues
  
  # Variance contribution of first eigenvector is \lambda_1/(sum over \lambda)
  var_share[[k]] <- cumsum(list_eig_val[[k]])/sum(list_eig_val[[k]]) 
  names(var_share[[k]]) <- paste0("Lambda_", 1:length(list_eig_val[[k]])) 
  
  # Compute eigenvectors (already sorted top to bottom by default)
  list_eig_vec[[k]] <- eigen(list_cov[[k]], symmetric = T)$vectors
  
}

### Principal Component Computation #####################################

for (l in qtr_grid[-qtr_max]) #For all except the last quarter
{
  temp_q_next <- list_ret_banks[[l+1]] %>% as.matrix()
  temp_q_current <- list_ret_banks[[l]] %>% as.matrix()
  
  pc_in_sample[[l]] <- temp_q_current%*%as.matrix(list_eig_vec[[l]])%>%
    as.matrix(.)
  
  bank_qtr_next <- ncol(temp_q_next)
  
  eig_matrix_curr <- list_eig_vec[[l]]
  num_eig_vec_current <- ncol(eig_matrix_curr)
  
  num_diff_banks <- min(bank_qtr_next, num_eig_vec_current)
  
  if (bank_qtr_next == num_eig_vec_current)
  {
    pc_out_of_sample[[l+1]] <- temp_q_next%*%as.matrix(eig_matrix_curr) %>%
      as.matrix(.)
  } else if (bank_qtr_next - num_eig_vec_current > 0)
  {
    pc_out_of_sample[[l+1]] <- temp_q_next[, 1:num_diff_banks]%*%as.matrix(eig_matrix_curr)%>%
      as.matrix(.)
  } else if (bank_qtr_next - num_eig_vec_current < 0)
  {
    pc_out_of_sample[[l+1]] <- temp_q_next%*%as.matrix(
      eig_matrix_curr[1:num_diff_banks, 1:num_diff_banks]
      )%>%
      as.matrix(.)
  }


}

#######################################################################
##### Principal Component Regressions and Integration Computation #####
#######################################################################

US_bank_integration_qtr <- rep(list(NULL), qtr_max)
list_reg_in_sample <- rep(list(NULL), qtr_max)
list_reg_out_of_sample <- rep(list(NULL), qtr_max)
list_qtrly_integration_out <- rep(list(NULL), qtr_max)
list_qtrly_integration_in <- rep(list(NULL), qtr_max)

# Assume 15 principal components are enough to explain 90% variance
# num_pc <- 15

for (m in qtr_grid[-1])
{
  # Principal components as explanatory variables
  var_x_expl_out <- pc_out_of_sample[[m]]
  var_x_expl_in <- pc_in_sample[[m]]

  ### Out of Sample Regressions ###
  
  # Note that each quarter, num of explanatory pc is different now
  # Pick as many PCs as will explain 90% of the variance
  var_x_expl_out <- var_x_expl_out[, which(var_share[[m]] < 0.9)] %>% 
    data.matrix(.)
  
  # Dependent variable is bank returns
  var_y <- list_ret_banks[[m]] %>% data.matrix(.)
  
  list_reg_out_of_sample[[m]] <- summary(lm(var_y ~ var_x_expl_out))
  temp_adj_rsqr_out <- lapply(list_reg_out_of_sample[[m]], `[`, "adj.r.squared") %>%
    as.data.frame(.)
  names(temp_adj_rsqr_out) <- colnames(var_y)
  
  ### In sample regressions ###
  if (m != qtr_max)
  {
    var_x_expl_in <- var_x_expl_in[, which(var_share[[m]] < 0.9)] %>% 
      data.matrix(.)
    list_reg_in_sample[[m]] <- summary(lm(var_y ~ var_x_expl_in))
    temp_adj_rsqr_in <- lapply(list_reg_in_sample[[m]], `[`, "adj.r.squared") %>%
      as.data.frame(.)
    names(temp_adj_rsqr_in) <- colnames(var_y)
  }
  
  # Store each bank's integration each quarter as a row vector in a list
  list_qtrly_integration_out[[m]] <- temp_adj_rsqr_out
  list_qtrly_integration_in[[m]] <- temp_adj_rsqr_in
  
}

names(list_qtrly_integration_out) <- paste0("Quarter_", qtr_grid)
names(list_qtrly_integration_in) <- paste0("Quarter_", qtr_grid)

integration_matrix_qtrly_out <- dplyr::bind_rows(list_qtrly_integration_out)
integration_matrix_qtrly_in <- dplyr::bind_rows(list_qtrly_integration_in)

rep_yr <- rep(year_min:year_max, each = 4)
rep_qtr <- rep(c("Q1","Q2","Q3","Q4"), num_years)
date_col <- paste0(rep_yr, rep_qtr)

integration_matrix_qtrly_out <- integration_matrix_qtrly_out %>%
  tibble::add_column(., Date = date_col[-1]) %>%
  dplyr::select(Date, everything())

integration_matrix_qtrly_in <- integration_matrix_qtrly_in %>%
  tibble::add_column(., Date = date_col[-1]) %>%
  dplyr::select(Date, everything())

b_1 <- names(integration_matrix_qtrly_out[2])
b_end <- names(integration_matrix_qtrly_out[ncol(integration_matrix_qtrly_out)])

b_1_in <- names(integration_matrix_qtrly_in[2])
b_end_in <- names(integration_matrix_qtrly_out[ncol(integration_matrix_qtrly_in)])

int_mat_qtrly_out_long <- integration_matrix_qtrly_out %>%
  tidyr::gather(., b_1:b_end, key = "Banks", value = "Integration")
int_mat_qtrly_in_long <- integration_matrix_qtrly_in %>%
  tidyr::gather(., b_1:b_end_in, key = "Banks", value = "Integration")

# Writing Out Some Important Files
readr::write_csv(integration_matrix_qtrly_out, "Integration_Out.csv")
readr::write_csv(integration_matrix_qtrly_in, "Integration_In.csv")
readr::write_csv(name_cusip_sic, "US_Bank_CUSIP_SIC.csv")
readr::write_csv(name_cusip, "US_Bank_CUSIP.csv")
readr::write_csv(int_mat_qtrly_in_long, "Integration_In_Long.csv")
readr::write_csv(int_mat_qtrly_out_long, "Integration_Out_Long.csv")