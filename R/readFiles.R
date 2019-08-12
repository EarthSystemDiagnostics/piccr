library(readr)
library(purrr)
library(stringr)

readFiles <- function(config, folder = ".") {
  files_in_folder <- list.files(path = folder, pattern = config$FILE_ID) %>%
    str_c(folder, "/", .)
  data <- map(files_in_folder, read_csv)
  return(data)
}