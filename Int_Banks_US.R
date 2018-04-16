### Integration Among US Banks: A Reboot Using WRDS Data ###

library(tidyverse)

############################
### Directory_Management ###
############################

# For replication or reproduction of this script, change the address
# of the directories below to conform to their location in the host
# machine

data_folder_path <- "../Data_Bank_Int/"
file_name <- "SICCD_6020-6079_6710-6712_20171105.dta"
file_path <- paste0(data_folder_path, file_name)

# Read .dta file for US banks

data_US <- haven::read_dta(file_path)

#bank_name <- data %>% dplyr::distinct(comnam)
#num_bank <- length(bank_name)