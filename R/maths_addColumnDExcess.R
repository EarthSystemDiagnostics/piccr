#' Add column dExcess
#'
#' Add the column "dExcess" to the dataset. dExcess is 
#' calculated as dExcess = dD - 8 * d18O.
#' 
#' @param dataset A data.frame.
#'
#' @return A data.frame.
#' 
addColumnDExcess <- function(dataset){
  
  dplyr::mutate(dataset, dExcess = `d(D_H)Mean` - `d(18_16)Mean` * 8)
}
