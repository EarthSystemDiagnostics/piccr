#' Calculate d-excess
#'
#' Calculate the second-order parameter d-excess from the d18O and dD values of
#' a given data set according to \code{d-excess = dD - 8 * d18O}.
#' 
#' @param dataset a data frame with measurement data of a specific data set.
#'
#' @return The input \code{dataset} supplemented by the column \code{dExcess}.
#' 
addColumnDExcess <- function(dataset){
  
  dplyr::mutate(dataset, dExcess = `d(D_H)Mean` - `d(18_16)Mean` * 8)
}
