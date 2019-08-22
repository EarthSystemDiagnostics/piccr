library(lubridate)

calibrateUsingSimpleDriftCorrection <- function(datasets, config){
  
  datasets %>%
    linearDriftCorrection() %>%
    linearCalibration(config, block = 1)
}

linearDriftCorrection <- function(datasets){
  map(datasets, linearDriftCorrectionSingleDataset)
}

linearDriftCorrectionSingleDataset <- function(dataset){
  
  dataset <- addColumnSecondsSinceStart(dataset)
  alphaValues <- calculateDriftSlope(dataset)
  driftCorrectedData <- applyDriftCorrection(dataset, alphaValues)
  
  return(driftCorrectedData)
}

addColumnSecondsSinceStart <- function(dataset){
  
  dataset %>%
    mutate(SecondsSinceStart = ymd_hms(.$TimeCode)) %>%
    mutate(SecondsSinceStart = c(0, int_diff(.$SecondsSinceStart))) %>%
    mutate(SecondsSinceStart = cumsum(.$SecondsSinceStart))
}

calculateDriftSlope <- function(dataset){
  
  trainingData <- dataset %>%
    filter(useForDriftCorr == TRUE) %>%
    drop_na(block)
  
  dataForEachStandard <- split(trainingData, trainingData$`Identifier 1`)
  
  # TODO: clean this code
  slopeD18O <- dataForEachStandard %>%
    map(function(x) lm(`d(18_16)Mean` ~ SecondsSinceStart, data = x)) %>%
    map_dbl(~ coef(.)[[2]]) %>%
    mean()
  
  slopeDD <- dataForEachStandard %>%
    map(function(x) lm(`d(D_H)Mean` ~ SecondsSinceStart, data = x)) %>%
    map_dbl(~ coef(.)[[2]]) %>%
    mean()
  
  list(d18O = slopeD18O, dD = slopeDD)
}

applyDriftCorrection <- function(dataset, alpha){
  
  dataset %>%
    mutate(`d(18_16)Mean` = `d(18_16)Mean` - alpha$d18O * SecondsSinceStart,
           `d(D_H)Mean` = `d(D_H)Mean` - alpha$dD * SecondsSinceStart) %>%
    select(-SecondsSinceStart)
}