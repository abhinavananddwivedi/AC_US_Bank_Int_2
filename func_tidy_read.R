# The following function takes as input a file path and then reads it tidily
# and extracts useful information after filtering, selecting etc. Since in this 
# project, all filtration etc. is common to all countries, we store this set of
# instructions in a single function
func_tidy_read <- function(data_file)
{
  file_path <- paste0(data_folder, data_file) 
  data <- haven::read_dta(file_path)
  
  # Stulz says that banks lie in SIC codes between 6020 to 6079 or 6710 to 6712
  data_bank <- data %>% dplyr::filter(sic %in% c(6020:6079, 6710:6712)) 
  
  data_bank <- data_bank %>% 
    dplyr::select(c(datadate, conm, curcdd, cshoc, cshtrd, prccd, sic)) %>%
    dplyr::filter(curcdd == "TWD") %>% #NOT COMMON ACROSS COUNTRIES
    tibble::add_column(., rel_ret = NA) %>%
    tibble::add_column(., qtr_num = NA)
  
  return(data_bank)
}