library(tidyverse)

#' calculate poooled standard deviation
#' 
#' Calculate the pooled standard deviation for d18O and dD. The pooled standard
#' deviation is calculated using the following formulas:
#' 
#'     x = (n_1 - 1) \* sd_1^2 + ... + (n_k - 1) \* sd_k^2
#'     y = n_1 + ... + n_k - k
#'     sdPooled = sqrt( x / y )
#'
#' @param datasets A data.frame with isotope measurement data.
#'
#' @return A list with the two named elements
#'         "d18O" and "dD" (the pooled standard deviation for d18O and dD).
calculatePoooledSD <- function(dataset){
 
   stdDevForEachSample <- dataset %>% 
     group_by(`Identifier 1`, block) %>% 
     summarise(n = n(), 
               sd.d18O = sd(`d(18_16)Mean`),
               sd.dD = sd(`d(D_H)Mean`)) %>% 
     ungroup()

   pooledStdDev <- stdDevForEachSample %>%
     mutate(summand.d18O = (n-1) * sd.d18O ^ 2,
            summand.dD = (n-1) * sd.dD ^ 2) %>% 
     summarise(numerator.d18O = sum(summand.d18O, na.rm = T),
               numerator.dD = sum(summand.dD, na.rm = T), 
               denominator = sum(n) - n()) %>%
     summarise(pooledStdDev.d18O = sqrt(numerator.d18O / denominator),
               pooledStdDev.dD = sqrt(numerator.dD / denominator))
  
  list(d18O = pooledStdDev$pooledStdDev.d18O,
       dD = pooledStdDev$pooledStdDev.dD)
}