library(yaml)

#' Read configuration file
#'
#' Read the specified YAML configuration file for piccr processing.
#' @param configFile A character string naming the config file.
#' @return A list.
#' @export
parseConfig <- function(configFile){

  tryCatch({
    yaml::read_yaml(configFile)
  }, error = function(e) {
    stop("Error reading config file. Make sure that you specified the 
         correct path and that read permissions are given.")
  })
}
