#' Calculate pooled standard deviation
#' 
#' Calculate the pooled standard deviation for d18O and dD for a given Picarro
#' data set, which provides a measure for the overall stability of consecutive
#' injections in the Picarro data.
#'
#' The pooled standard deviation provides a way to estimate the standard
#' deviation of several populations which may have different mean values but for
#' which you can assume that the standard deviation of each population is the
#' same. For Picarro data, the different populations are the individual samples
#' and we assume that the true standard deviation of the injections for a
#' specific sample is the same for all samples. The pooled standard deviation
#' \eqn{\sigma_p} for \eqn{k} samples is then calculated according to
#' \deqn{
#' x = (n_1 - 1) * \sigma_1^2 + ... + (n_k - 1) * \sigma_k^2
#' y = n_1 + ... + n_k - k
#' \sigma_p = sqrt(x / y)
#' }
#' where \eqn{n_i} and \eqn{\sigma_i} are the number of injections and the
#' standard deviation for sample \eqn{i}, respectively.
#'
#' @param dataset a data frame with measurement data of a specific data set.
#' @import dplyr
#'
#' @return A list with two elements \code{d18O} and \code{dD} with the pooled
#'   standard deviation for d18O and dD, respeectively.
#' @source https://en.wikipedia.org/wiki/Pooled_variance
#' 
calculatePoooledSD <- function(dataset){
 
   stdDevForEachSample <- dataset %>% 
     group_by(`Identifier 1`, block, vial_group) %>%
     summarise(n = n(), 
               sd.d18O = sd(`d(18_16)Mean`, na.rm = TRUE),
               sd.dD = sd(`d(D_H)Mean`), na.rm = TRUE) %>%
     ungroup()

   pooledStdDev <- stdDevForEachSample %>%
     mutate(summand.d18O = (n-1) * sd.d18O ^ 2,
            summand.dD = (n-1) * sd.dD ^ 2) %>% 
     summarise(numerator.d18O = sum(summand.d18O, na.rm = TRUE),
               numerator.dD = sum(summand.dD, na.rm = TRUE),
               denominator = sum(n) - n()) %>%
     summarise(pooledStdDev.d18O = sqrt(numerator.d18O / denominator),
               pooledStdDev.dD = sqrt(numerator.dD / denominator))
  
  list(d18O = pooledStdDev$pooledStdDev.d18O,
       dD = pooledStdDev$pooledStdDev.dD)
}
