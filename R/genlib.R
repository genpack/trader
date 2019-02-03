# Header
# Filename:     general_lib.R
# Description:  Contains some general functions useful for programming in R
# Author:       Nima Ramezani Taghiabadi
# Email :       N.RamezaniTaghiabadi@uws.edu.au
# Start Date:   21 October 2013
# Last change:  21 October 2013
# Version:      1.0

vector.dimension.equal <- function(v1,v2){
  #returns TRUE if the two given vectors have the same length
  n1 = length(v1)
  n2 = length(v2)
  return ((n1==n2))
}

matrix.dimension.equal <- function(m1,m2){
  #returns TRUE if the two given matrices have the same dimensions
  d1 = dim(m1)
  d2 = dim(m2)
  return ((d1[1]==d2[1]) & (d1[2]==d2[2]))
}

assert <- function(flag, error.msg){
  # halts the program if flag is TRUE displaying the given error.msg
  if (!flag) {stop(error.msg)}
}

equal <- function(a,b, eps = 0.0000001){
  # Returns True if a and b are equal :-)
  return((abs(a-b) < eps))
}

floor.to.precision <- function(a, precision = 0.01){
  return(floor(a/precision)*precision)
}

clear.workspace <- function(){
  rm(list = ls())
}

## Returns a matrix of zeros
zeros <- function (m,n){
  return(matrix(rep(0, m*n), m , n))
}

is.boolean = function(x) inherits(x, 'logical')
