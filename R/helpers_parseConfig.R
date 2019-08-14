library(yaml)

parseConfig <- function(config_file){
  tryCatch({
    return(read_yaml(config_file))
  }, error = function(e) {
    stop("Error reading config file. Make sure that you specified the 
         correct path and that read permissions are given.")
  })
}