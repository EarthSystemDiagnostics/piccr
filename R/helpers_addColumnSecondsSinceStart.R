library(lubridate)
library(tidyverse)

addColumnSecondsSinceStart <- function(dataset){
  
  dataset %>%
    mutate(SecondsSinceStart = lubridate::ymd_hms(.$TimeCode)) %>%
    mutate(SecondsSinceStart = c(0, lubridate::int_diff(.$SecondsSinceStart))) %>%
    mutate(SecondsSinceStart = cumsum(.$SecondsSinceStart))
}