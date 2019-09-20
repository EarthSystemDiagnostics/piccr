##' Re-calculate injection numbers
##'
##' Re-calculate injection numbers to account for probes being measured
##' from two or more consectuive vials.
##' @param datasets a named list of data frames; each data frame is a read in
##' data set.
##' @return the same named list of data frames where for each data set the
##' column of injection numbers has been re-calculated accounting for
##' possible consecutive vials of the same probe.
normalizeInjectionNumbers <- function(datasets) {
  
  map(datasets, function(dataset){
    dataset %>% 
      group_by(`Identifier 1`, block) %>% 
      mutate(`Inj Nr` = row_number()) %>%
      ungroup() %>%
      arrange(Line)
  })
}
