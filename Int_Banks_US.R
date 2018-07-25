### Integration Among US Banks: A Reboot Using WRDS Data ###

# Declare libraries
library(tidyverse)

# Read, tidy and preprocess the datasets from CRSP and Compustat 

source('Int_Banks_US_data_preprocessing.R', echo = F)
#takes ~90 seconds to run

### Function Declarations #####################################################################

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


func_part_NA_filler <- function(vec)
{
  # This function replaces missing values of a vector with its median
  
  vec_med <- median(vec, na.rm = T)
  vec_NA <- is.na(vec)
  vec[vec_NA] <- vec_med
  
  return(vec)
}

func_high_stale_NA_filler <- function(temp_matrix, alpha = 0.5)
{
  # This function takes a data matrix and removes columns with a high proportion
  # of missing or stale entries. (Stale entries have return 0.) Any columns
  # with leftover missing entries are replaced with their respective medians.
  #
  # The inputs are the data matrix and the critical threshold (in [0,1]) defining 
  # "too high". The default is alpha = 0.5. This function depends on three other 
  # self-defined functions func_stale(), func_NA_killer() and func_part_NA_filler()
  
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

### The Main Script Starts Here ###

name_bank_US <- data_US_bank %>%
  dplyr::select(comnam) %>%
  dplyr::distinct(.)

num_bank_US <- nrow(name_bank_US)

year_min <- min(lubridate::year(data_US_bank$date)) #year 1
year_max <- max(lubridate::year(data_US_bank$date)) #year end
num_years <- year_max - year_min + 1
years <- year_min:year_max

qtr_min <- 1
qtr_max <- 4*num_years
qtr_grid <- qtr_min:qtr_max

data_list_US <- rep(list(NULL), num_bank_US)

# # These banks have inconsistent, duplicated data: ignore them
# ind_bank_ignore <- c(57, 99, 128, 210, 217, 335, 388,
#                      509, 559, 589, 597, 626, 934,
#                      1051, 1300, 1361, 1679, 1691) 
# 
# ind_bank_use <- setdiff((1:num_bank_US), ind_bank_ignore)

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

#names(data_list_US) <- name_bank_US

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
    temp_q_subsequent <- temp_df %>% 
      dplyr::filter(qtr_num == qtr_grid[k+1]) %>%
      dplyr::select(-c(date, qtr_num))

    # Kill+Fill the NA columns and rows and save as matrix
    temp_q_subsequent <- func_full_NA_killer(temp_q_subsequent) %>% 
      func_high_stale_NA_filler(.) %>%
      as.matrix(.)

    # Current quarter's returns
    temp_q_current <- temp_df %>% 
      dplyr::filter(qtr_num == qtr_grid[k]) %>%
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
  
}

# Computing share of explanatory power of eigenvectors, top to bottom

eig_vec_med <- c()
eig_vec_mean <- c()

for (i in 1:15)
{
  eig_vec_med[i] <- lapply(var_share, `[`, paste0("Lambda_", i)) %>%
    unlist() %>% 
    median()
  
  eig_vec_mean[i] <- lapply(var_share, `[`, paste0("Lambda_", i)) %>%
    unlist() %>% 
    mean()
}
# IMPROVE THIS LINE!!!

#######################################################################
##### Principal Component Regressions and Integration Computation #####
#######################################################################

temp_qtr_integration <- rep(list(NULL), qtr_max)
temp_reg <- rep(list(NULL), qtr_max)

for (l in 2:qtr_max) #PC computed from quarter 2 onwards
{
  # Principal components as explanatory variables
  temp_x <- pc_out_of_sample[[l]]
  
  # Note that each quarter, num of explanatory pc is different now
  temp_x <- temp_x[, which(var_share[[l]] < 0.90)] %>% 
    data.matrix(.)
  
  # Dependent variables are corresponding quarterly bank returns 
  temp_y <- list_ret_banks[[l]] %>% 
    data.matrix(.)
  
  # Inlcude only those banks which have more than 20% usable returns in quarter
  temp_y <- temp_y[, colSums(is.na(temp_y)) <= nrow(temp_y)*0.20]
  
  # Computing integration as adjusted R squares of PC regresssions
  if (nrow(temp_y) == nrow(temp_x)) #if the sizes of matrices conform
  {
    temp_reg[[l]] <- summary(lm(temp_y ~ temp_x, na.action = na.omit)) #store the summary of OLS
    temp_adj_rsqr <- lapply(temp_reg[[l]], `[`, "adj.r.squared") %>%
      as.data.frame(.) #store the adjusted rsquares from each regression
    names(temp_adj_rsqr) <- colnames(temp_y)
    
    #temp_adj_rsqr[which(temp_adj_rsqr < 0)] <- 0
    
    # Store each bank's integration each quarter as a row vector in a list
    temp_qtr_integration[[l]] <- temp_adj_rsqr 
  }
}

names(temp_qtr_integration) <- paste0("Quarter_", qtr_grid)

temp_null <- sapply(temp_qtr_integration, is.null) #locate null elements

# For convenience, change the integration list to a matrix with banks as columns #
# Convert list to tibble 
temp_qtr_mat <- dplyr::bind_rows(temp_qtr_integration) #note that NULL elements are killed

# Initialize the integration matrix, name its columns as banks
temp_int_mat <- data.frame(matrix(NA, nrow = qtr_max, ncol = ncol(temp_qtr_mat)))
names(temp_int_mat) <- colnames(temp_qtr_mat)

# Assign the relevant rows to the matrix, the rest remain NAs
temp_int_mat[temp_null==0, ] <- temp_qtr_mat

# Sample statistics along rows (stat_dim = 1) or columns (stat_dim = 2)

## Time Trends for Integration ##

# temp_int_t <- temp_int_mat %>% as.data.frame()
# 
# #########################################################
# # Pick column only if fewer than 10(/44) unusable entries
# temp_index_low <- which(colSums(is.na(temp_int_t)) <= 10) 
# temp_int_t_reg <- temp_int_t[, temp_index_low] %>% data.matrix()
# 
# # Testing for time trends
# temp_lm_t <- summary(lm(temp_int_t_reg ~ qtr_grid, na.action = na.omit))
# 
# temp_lm_t_val <- rep(list(NULL), length(temp_lm_t))
# temp_lm_p_val <- rep(list(NULL), length(temp_lm_t))
# 
# for (p in 1:length(temp_lm_t))
# {
#   #temp_temp <- temp_lm_t[[p]][["coefficients"]][, c("t value", "Pr(>|t|)")]
#   temp_temp <- temp_lm_t[[p]]
#   temp_lm_t_val[[p]] <- temp_temp$coefficients[, "t value"] %>%
#     as.data.frame()
#   temp_lm_p_val[[p]] <- temp_temp$coefficients[, "Pr(>|t|)"] %>%
#     as.data.frame()
# }
# ### VERY POOR CODE WRITING HERE, PLEASE USE SOME FUNCTIONAL PROGRAMMMING HERE
# names(temp_lm_p_val) <- names(temp_lm_t_val) <- names(temp_lm_t)
# 


#####################################################################################
#time_stop <- Sys.time()
#(time_stop - time_start)
