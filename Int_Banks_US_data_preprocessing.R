#####################################################################
### Integration Among US Banks Using Data From CRSP and Compustat ###
#####################################################################

# Libraries
library(tidyverse)
library(lubridate)
library(moments)

#############################################################
### Preprocessing Bank Price Data from CRSP #################
#############################################################

############################
### Directory_Management ###
############################

# For reproduction of this script change the address
# of the directories to conform to their location in 
# the host machine. Note Linux vs Windows addresses.

data_folder_path <- "../Data_Bank_Int/" #Name of folder where data file exists
file_name_ret_daily <- "SICCD_6000_6799.dta" #CRSP daily return file
file_name_TA <- "US_Bank_Cstat_TA.dta" #Compustat total assets file

file_path_ret <- paste0(data_folder_path, file_name_ret_daily)
file_path_TA <- paste0(data_folder_path, file_name_TA)

### Read .dta file for US banks
data_US_full <- haven::read_dta(file_path_ret)
# Note that since the data file is ~7 GB, this step 
# takes time. Not suitable for small RAM machines.

data_US_bank_TA <- haven::read_dta(file_path_TA)

#############################################
### Data Filtration, Cleaning and Tidying ###
#############################################

# Admissible SIC codes
ind_comm_banks <- c(6020:6029) #commercial banks
ind_saving_inst <- c(6030:6039) #saving institutions
ind_credit_union <- c(6060:6069) #credit unions
ind_bank_hold <- c(6710:6712) #bank holding companies

ind_bank_use <- c(ind_comm_banks, ind_saving_inst,
                  ind_credit_union, ind_bank_hold) #use only these

# Admissible share codes
# Information taken from http://www.crsp.com/products/documentation/data-definitions-1
ind_share_code_common <- c(10, 11) #only common shares included

### Filter CRSP data ###

# Intermediate dataset with primary cleaning up
data_US_inter <- data_US_full %>% 
  dplyr::filter(siccd %in% ind_bank_use |
                  hsiccd %in% ind_bank_use) %>% #ignore non-banks
  dplyr::filter(shrcd %in% ind_share_code_common) %>% #include common shares only
  dplyr::filter(lubridate::year(date) >= "1993") %>% #only banks post '93
  dplyr::filter(prc > 1) #ignore banks with nominal price <= $1

# Identifying US banks
data_US_id <- data_US_inter %>%
  dplyr::select(comnam, siccd,
                hsiccd, ncusip, 
                cusip, permno) %>%
  dplyr::distinct() 

cusip_US_8 <- unique(data_US_id$cusip) %>%
  tibble::as_tibble() #cusips of CRSP US banks (8 digits)

### COMPUSTAT DATA CUSIPS (8 digits) ###

# Identify banks with size >$1B in the last year
data_US_1b_id <- data_US_bank_TA %>%
  dplyr::filter(fyearq == 2016 & fqtr == 4) %>% 
  dplyr::filter(atq >= 1000) %>% #total assets in $millions, 1B=1000mil
  dplyr::select(conm, conml,
                sic, cusip,
                gvkey) %>%
  dplyr::distinct(.)

# Admissible cusips of banks with assets >$1B
cusip_1b_8 <- data_US_1b_id$cusip %>%
  substr(., 1, 8) %>%
  tibble::as_tibble()

func_cusip_check <- function(cusip_8)
{
  # This function accepts an 8 digit cusip
  # and returns 1 if the last two digits
  # indicate common stocks (10 or 11) else
  # returns 0
  last_2_char <- substr(cusip_8, 7, 8)
  if (last_2_char == '10' |
      last_2_char == '11')
  {
    return(1)
  } else
  {
    return(0)
  }
}

# Apply cusip check function to indicate common stock status
test_comm_share <- sapply(cusip_1b_8$value, func_cusip_check)

# Attach to Compustat data then filter banks with common stock
data_US_1b_id <- data_US_1b_id %>%
  tibble::add_column(cusip_8 = cusip_1b_8$value) %>%
  tibble::add_column(comm_share = test_comm_share) %>%
  dplyr::filter(comm_share == 1) #only common shares

cusip_1b_8 <- data_US_1b_id$cusip_8 %>%
  tibble::as_tibble() #update admissible 8 digit cusips

### Isolate cusips of banks with assets > $1B in both datasets ###
common_cusip <- dplyr::intersect(cusip_1b_8$value, 
                                 cusip_US_8$value)

### The final CRSP sample of US banks contains only those banks 
### whose 2016 total assets are more than $1B

data_US_bank <- data_US_inter %>%
  dplyr::select(c(date, comnam, 
                  ncusip, cusip,
                  siccd, prc, ret)) %>%
  dplyr::rename(., "cusip_8" = cusip) %>%
  dplyr::filter(cusip_8 %in% common_cusip) %>%
  tibble::add_column(., qtr_num = NA) %>%
  dplyr::arrange(., comnam)

################################################
### Some summary statistics for bank returns ###
################################################

# Sample stats according to SICCD (industry classification)
summ_stat_siccd <- data_US_bank %>% 
  dplyr::group_by(siccd) %>% 
  dplyr::summarise(., 'minimum' = min(ret, na.rm = T), 
                   'maximum' = max(ret, na.rm = T), 
                   'avg' = mean(ret, na.rm = T), 
                   'med' = median(ret, na.rm = T), 
                   'std_dev' = sd(ret, na.rm = T), 
                   'iqr' = IQR(ret, na.rm = T),
                   'skew' = moments::skewness(ret, na.rm = T),
                   'kurt' = moments::kurtosis(ret, na.rm = T)
                   )

readr::write_csv(summ_stat_siccd, "Summary_Stat_SIC.csv")

# Sample stats according to comnam (bank name)
summ_stat_comnam <- data_US_bank %>% 
  dplyr::group_by(comnam) %>% 
  dplyr::summarise(., 'minimum' = min(ret, na.rm = T), 
                   'maximum' = max(ret, na.rm = T), 
                   'avg' = mean(ret, na.rm = T), 
                   'med' = median(ret, na.rm = T), 
                   'std_dev' = sd(ret, na.rm = T), 
                   'iqr' = IQR(ret, na.rm = T),
                   'skew' = moments::skewness(ret, na.rm = T),
                   'kurt' = moments::kurtosis(ret, na.rm = T)
                   )

readr::write_csv(summ_stat_comnam, "Summary_Stat_Banks.csv")


#############################################################
### Preprocessing Bank Balance Sheet Data from Compustat ####
#############################################################

file_name_Cstat <- "US_Bank_Cstat.dta" #CRSP daily return file

file_path_Cstat <- paste0(data_folder_path, file_name_ret_daily)

### Read .dta file for US banks
data_US_Cstat <- haven::read_dta(file_path_Cstat)