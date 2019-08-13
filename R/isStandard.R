library(purrr)

isStandard <- function(id1, config){
  id1 %in% map(config$standards, ~ .$name)
}