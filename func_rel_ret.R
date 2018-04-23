# The following function accepts a vector of prices and returns a vector of
# relative returns via the formula: r_t = (p_t - p_{t-1})/p_t. By construction, 
# the first date's return is NA
func_rel_ret <- function(prices)
{
  n <- length(prices)
  temp_ret <- diff(prices)/prices[-n]
  rel_ret <- c(NA, temp_ret)
  return(rel_ret)
}