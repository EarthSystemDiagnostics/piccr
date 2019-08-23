library(tidyverse)

#' addColumnDExcess
#'
#' Add the column "dExcess" to every dataframe in datasets. dExcess is 
#' calculated as dExcess = dD - 8 * d18O.
#' 
#' @param datasets A named list of dataframes.
#'
#' @return A list. The list elements are named like the input list "datasets". 
#'         Each element of the list is a dataframe that includes the column
#'         "dExcess".
addColumnDExcess <- function(datasets){
  
  map(datasets, addColumnDExcessForSingleDataset)
}

addColumnDExcessForSingleDataset <- function(dataset){
  
  mutate(dataset, dExcess = delta.H2 - delta.O18 * 8)
}