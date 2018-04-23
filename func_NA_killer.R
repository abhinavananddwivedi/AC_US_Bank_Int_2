# This function kills a data frame's full NA columns and rows, in that order
func_NA_killer <- function(data_matrix)
{
  # full NA column killer
  temp_no_NA_col <- data_matrix[, colSums(is.na(data_matrix)) < nrow(data_matrix)]
  
  # full NA row killer
  temp_no_NA_col_row <- temp_no_NA_col[rowSums(is.na(temp_no_NA_col)) != 
                                         ncol(temp_no_NA_col), ]
  
  return(temp_no_NA_col_row)
}
