library(readr)

outputSummaryFile <- function(processedData, outputFile, config){
  
  meanPooledSdO18 <- mean(map_dbl(processedData$pooledStdDev, ~ .$d18O))
  meanPooledSdH2  <- mean(map_dbl(processedData$pooledStdDev, ~ .$dD))
  
  averageOverAllFiles <- sprintf(
    str_c("### AVERAGE OVER ALL FILES: ###\n\n",
          "pooled standard deviation delta O18: %.2f\n",
          "pooled standard deviation delta H2: %.2f\n\n"),
    meanPooledSdO18, meanPooledSdH2
  )
  
  valuesForEachFile <- str_c(
    "### VALUES FOR EACH FILE: ###\n\n",
    "file name, pooled sd delta O18, pooled sd delta H2\n",
    paste(map2_chr(processedData$pooledStdDev, names(processedData$pooledStdDev), 
                   ~ sprintf("%s, %.2f, %.2f", .y, .x$d18O, .x$dD)), collapse = "\n"),
    "\n\n"
  )

  
  trueValues <- map(config$standards, ~ list(o18_True = .$o18_True, 
                                             H2_True = .$H2_True))
  names(trueValues) <- map_chr(config$standards, ~ .$name)
  
  print(trueValues)
  
  standardBias <- str_c(
    "### INTER STANDARD BIAS TO LITERATURE VALUES FOR EACH FILE: ###\n\n",
    "file name, standard, block, bias O18, bias H2\n",
    paste(unlist(map2(
      processedData$processed, names(processedData$processed), ~ apply(.x, 1, function(row){
        block <- row[["block"]]
        name <- row[["Identifier 1"]]
        print(row)
        if(!is.na(block)) 
          sprintf("%s, %s, %.f, %.2f, %.2f", .y, name, as.numeric(block), 
                  as.numeric(row[["delta.O18"]]) - trueValues[[name]][["o18_True"]],
                  as.numeric(row[["delta.H2"]]) - trueValues[[name]][["H2_True"]])
    }))), collapse = "\n")
  )
  
  write_file(str_c(averageOverAllFiles, valuesForEachFile, standardBias), outputFile)
}