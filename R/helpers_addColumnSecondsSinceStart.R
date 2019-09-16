library(lubridate)
library(tidyverse)

addColumnSecondsSinceStart <- function(dataset){
  
  dataset %>%
    mutate(SecondsSinceStart = lubridate::ymd_hms(.$`Time Code`)) %>%
    mutate(SecondsSinceStart = c(0, lubridate::int_diff(.$SecondsSinceStart))) %>%
    mutate(SecondsSinceStart = cumsum(.$SecondsSinceStart))
}