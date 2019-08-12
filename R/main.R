process_files <- function(config_file){
  
  config <- parse_config(config_file)
  
  processed_data <- read_files(config) %>% 
    associate_standards_with_true_values(config) %>%
    process_data()
  
  write_data_to_file(config, processed_data)
  
  invisible(processed_data)
}

process_data <- function(raw_data){
  
  memory_corrected_data <- correct_for_memory_effect(raw_data)
  calibrated_data <- calibrate(memory_corrected_data)
  drift_corrected_data <- correct_for_drifting_pattern(calibrated_data)
  
  invisible(list(memory_corrected = memory_corrected_data, 
                 calibrated = calibrated_data, 
                 drift_corrected = drift_corrected_data))
}