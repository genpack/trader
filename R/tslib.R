# Header
# Filename:     time_series_lib.R
# Description:  Contains some functions useful for time series prediction, portfolio optimization
#               and optimum lot recommendation
# Author:       Nima Ramezani Taghiabadi
# Email :       N.RamezaniTaghiabadi@uws.edu.au
# Date:         11 September 2013
# Version:      1.0
# Changes from previous version:


moving.average.element = function(x,m,i, adjust = "right"){
  n =length(x)
  adjust = tolower(adjust)
  if (m > n) {print("Error: moving average weight greater than the given vector length")
              return(NA)}
  if (adjust %in% c("center","centre","central")){
    j = floor(m/2)
    if ((i <= j)|(i > n - j)){
      # cat("The moving average can not be computed for the first and last ",j," elements \n")
      return(NA)}

    # The program starts here:

    if (m%%2) {return(mean(x[(i-j):(i+j)]))} else{
      s = (m-1)*moving.average.element(x, m - 1,i)
      s = s + 0.5*x[i-j] + 0.5*x[i+j]
      return(s/m)}
  } else if (adjust == "right"){
      return(mean(x[(i - m + 1):i]))
  }
}

moving.average=function(x, m, adjust = "right"){
  # Returns the moving average of a time series
  n  = length(x)
  ma = rep(NA,n)
  adjust = tolower(adjust)
  if (adjust %in% c("center","centre","central")){
    j  = floor(m/2)
    if (m%%2){ # m is odd
      w=rep(1,m)/m
    } else{ # m is even
      w = c(0.5, rep(1, m-1), 0.5)/m}
    offset = (-j):j
    for (i in (j+1):(n-j)){
      ma[i] = sum(w*x[i+offset])
    }
  } else if (adjust == "right"){
    for (i in m:n) {
      ma[i] = mean(x[(i - m + 1):i])
    }
  } else {print("moving.average Error: Given adjust not known. Must be central, right or left")}
  return(ma)
}

seasonal = function(a, m){
  # The following function computes a seasonal component of a time series by averaging
  # over all observations that are m apart.
  seas = c()
  n = length(a)
  a = c(a,rep(NA, m - n %% m))
  A = matrix(a, nrow = m)
  s = rowMeans(A, na.rm=TRUE)
  s = s - mean(s)
  k = floor(n/m)
  j = n%%m
  for (i in 1:k){seas = c(seas, s)}
  seas = c(seas, s[1:j])
  return(seas)
}

components = function(x, freq = 7, adjust = "right"){
  adjust  = tolower(adjust)
  trnd    = moving.average(x, freq, adjust = adjust)
  s.plus.e = x - trnd
  s        = seasonal(s.plus.e, freq)
  e        = s.plus.e - s
  output = list(x = x, trend = trnd, seasonal = s, irregular = e )
  return(output)
}



