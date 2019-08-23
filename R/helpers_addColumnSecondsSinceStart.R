library(lubridate)
library(tidyverse)

addColumnSecondsSinceStart <- function(dataset){
  
  dataset %>%
    mutate(SecondsSinceStart = ymd_hms(.$TimeCode)) %>%
    mutate(SecondsSinceStart = c(0, int_diff(.$SecondsSinceStart))) %>%
    mutate(SecondsSinceStart = cumsum(.$SecondsSinceStart))
}