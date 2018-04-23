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

for (i in seq_along(1:num_bank_US))
{
  temp <- data_US_bank %>% dplyr::filter(comnam == name_bank_US[i])
  
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
}

names(data_list_US) <- name_bank_US

data_list_US_df <- dplyr::bind_rows(data_list_US)


######################################################################################
time_stop <- Sys.time()
#(time_stop - time_start)
