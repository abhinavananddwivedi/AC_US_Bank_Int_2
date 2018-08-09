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
  # This function returns number of NAs in a vector
  return(sum(is.na(vec)))
}

func_NA_filler <- function(vec)
{
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
pc_out_of_sample <- rep(list(NULL), qtr_max)
var_share <- rep(list(NULL), qtr_max)

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
    # as.data.frame(.) %>% 
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

### Out-of-sample Principal Component Computation #################################

for (l in qtr_grid[-qtr_max]) #For all except the last quarter
{
  temp_q_next <- list_ret_banks[[l+1]] %>% as.matrix()
  temp_q_current <- list_ret_banks[[l]] %>% as.matrix()
  
  if (ncol(temp_q_next) == ncol(list_eig_vec[[l]]))
  {
    pc_out_of_sample[[l+1]] <- temp_q_next%*%as.matrix(list_eig_vec[[l]])
  } else if (ncol(temp_q_current) == ncol(list_eig_vec[[l]]))
  {
    pc_out_of_sample[[l+1]] <- temp_q_current%*%as.matrix(list_eig_vec[[l]])
  }


}

func_mat_isna <- function(matrix)
{
  return(colSums(is.na(matrix)))
}

# # Computing share of explanatory power of eigenvectors, top to bottom
# 
# eig_vec_med <- c()
# eig_vec_mean <- c()
# 
# for (i in 1:15)
# {
#   eig_vec_med[i] <- lapply(var_share, `[`, paste0("Lambda_", i)) %>%
#     unlist() %>% 
#     median()
#   
#   eig_vec_mean[i] <- lapply(var_share, `[`, paste0("Lambda_", i)) %>%
#     unlist() %>% 
#     mean()
# }
# IMPROVE THIS LINE!!!

#######################################################################
##### Principal Component Regressions and Integration Computation #####
#######################################################################

# US_bank_integration_qtr <- rep(list(NULL), qtr_max)
# temp_reg <- rep(list(NULL), qtr_max)
# 
# for (m in qtr_grid[-1])
# {
#   
# }

# for (l in 2:qtr_max) #PC computed from quarter 2 onwards
# {
#   # Principal components as explanatory variables
#   temp_x <- pc_out_of_sample[[l]]
#
#   # Note that each quarter, num of explanatory pc is different now
#   temp_x <- temp_x[, which(var_share[[l]] < 0.90)] %>%
#     data.matrix(.)
#
#   # Dependent variables are corresponding quarterly bank returns
#   temp_y <- list_ret_banks[[l]] %>%
#     data.matrix(.)
#
#   # Inlcude only those banks which have more than 20% usable returns in quarter
#   temp_y <- temp_y[, colSums(is.na(temp_y)) <= nrow(temp_y)*0.20]
#   ### THIS SEEMS REDUNDANT. FULLY FILLED MATRICES ENCOUNTERED
#   ### MAYBE STALE VALUE CHECKING INSTEAD?
#
#   # Computing integration as adjusted R squares of PC regresssions
#   if (nrow(temp_y) == nrow(temp_x)) #if the sizes of matrices conform
#   {
#     temp_reg[[l]] <- summary(lm(temp_y ~ temp_x, na.action = na.omit)) #store the summary of OLS
#     temp_adj_rsqr <- lapply(temp_reg[[l]], `[`, "adj.r.squared") %>%
#       as.data.frame(.) #store the adjusted rsquares from each regression
#     names(temp_adj_rsqr) <- colnames(temp_y)
#
#     # Store each bank's integration each quarter as a row vector in a list
#     temp_qtr_integration[[l]] <- max(temp_adj_rsqr, 0)
#
#   }
# }
#
# names(temp_qtr_integration) <- paste0("Quarter_", qtr_grid)
# 
# temp_null <- sapply(temp_qtr_integration, is.null) #locate null elements
# 
# # For convenience, change the integration list to a matrix with banks as columns #
# # Convert list to tibble 
# temp_qtr_mat <- dplyr::bind_rows(temp_qtr_integration) #note that NULL elements are killed
# 
# # Initialize the integration matrix, name its columns as banks
# temp_int_mat <- data.frame(matrix(NA, nrow = qtr_max, ncol = ncol(temp_qtr_mat)))
# names(temp_int_mat) <- colnames(temp_qtr_mat)
# 
# # Assign the relevant rows to the matrix, the rest remain NAs
# temp_int_mat[temp_null==0, ] <- temp_qtr_mat

#####################################################################################
#time_stop <- Sys.time()
#(time_stop - time_start)
