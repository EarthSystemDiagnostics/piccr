#' Correct for memory effect
#' 
#' Correct a given measurement data set for the memory effect by applying
#' memory coefficients estimated based on analysing the standards in the first
#' standard block.
#' 
#' @param dataset a data frame with measurement data of a specific data set. It
#'   needs to contain the additional columns \code{block} and
#'   \code{vial_group} which are not included in the raw Picarro output.
#'
#' @return A named list with two elements:
#'   \describe{
#'   \item{\code{datasetMemoryCorrected}:}{the input \code{dataset} with the
#'     d18O and dD columns corrected for memory effect.}
#'   \item{\code{memoryCoefficients}:}{a data frame of estimated memory
#'     coefficients (see \code{\link{calculateMemoryCoefficients}} for
#'     details).}
#' }
#' @seealso \code{\link{calculateMemoryCoefficients}},
#'   \code{\link{groupStandardsInBlocks}},
#'   \code{\link{assignVialsToGroups}}
#' 
correctForMemoryEffect <- function(dataset){
  
  memoryCoefficients <- calculateMemoryCoefficients(dataset)
  datasetMemoryCorrected <- applyMemoryCorrection(dataset, memoryCoefficients)
  
  return(list(datasetMemoryCorrected = datasetMemoryCorrected,
              memoryCoefficients = memoryCoefficients))
}

#' Calculate memory coefficients
#'
#' Calculate memory coefficients using the data of standards measured in the
#' first standard block at the beginning of the measurement sequence.
#'
#' Memory coefficients are estimated based on the formula in
#' \code{\link{formulaMemCoeff}} for the (n - 1) of the n standards in the first
#' standard block (per definition, no coefficients can be calulated for the very
#' first standard in the block), and the average across these (n - 1) individual
#' sets of memory coefficients is calculated. The maximum number of mean memory
#' coefficients is given by the maximum number of injections of the used
#' standards.
#' 
#' @param dataset a data frame with measurement data of a specific data set. It
#'   needs to contain the additional columns \code{block} and
#'   \code{vial_group} which are not included in the raw Picarro output.
#' @import dplyr
#'
#' @return A data frame with rows corresponding to injection numbers and columns
#'   to standards and isotopic species; specifically, for each unique vial block
#'   of standards, memory coefficients for d18O and dD are returned. The
#'   mean memory coefficients are returned in the columns \code{memoryCoeffD18O}
#'   (d18O) and \code{memoryCoeffDD} (dD). See also the package vignette for
#'   more details.
#' @seealso \code{\link{groupStandardsInBlocks}},
#'   \code{\link{assignVialsToGroups}}
#' 
calculateMemoryCoefficients <- function(dataset) {
  
  block1 <- filter(dataset, block == 1)
  deltaTrueAndDeltaTruePrev <- getDeltaTrueAndDeltaTruePrevForEachSample(block1, `Identifier 1`, `vial_group`)
  
  memoryCoefficients <- deltaTrueAndDeltaTruePrev %>%
    mutate(memoryCoeffD18O = formulaMemCoeff(.$`d(18_16)Mean`, .$deltaTrueD18O, .$deltaTruePrevD18O),
           memoryCoeffDD = formulaMemCoeff(.$`d(D_H)Mean`, .$deltaTrueDD, .$deltaTruePrevDD))
  
  # get the mean mem coeff as a dataframe
  meanMemoryCoefficients <- memoryCoefficients %>%
    group_by(`Inj Nr`) %>%
    summarise(memoryCoeffD18O = mean(memoryCoeffD18O, na.rm = T), 
              memoryCoeffDD = mean(memoryCoeffDD, na.rm = T))
  
  # extract the mem coeff for each standard as a list of dataframes
  memoryCoeffForEachStandard <- memoryCoefficients %>%
    select(`Inj Nr`, `Identifier 1`, vial_group, memoryCoeffD18O, memoryCoeffDD)

  groupNames <- memoryCoeffForEachStandard %>%
    group_keys(`Identifier 1`, `vial_group`) %>%
    apply(1, function(x) {paste(x, collapse = "_vial")})

  memoryCoeffForEachStandard <- memoryCoeffForEachStandard %>%
    group_split(`Identifier 1`, `vial_group`) %>%
    setNames(groupNames)

  memoryCoeffForEachStandard <- purrr::map(names(memoryCoeffForEachStandard), ~ {
    data <- select(memoryCoeffForEachStandard[[.]], `Inj Nr`, memoryCoeffD18O, memoryCoeffDD)
    setNames(data, c("Inj Nr", stringr::str_c(., "_d18O"), stringr::str_c(., "_dD")))
  })
  
  # join the mean mem coeff and the mem coeff for each standard into one dataframe
  tablesToJoin <- append(memoryCoeffForEachStandard, list(meanMemoryCoefficients))
  memCoeffOutput <- purrr::reduce(tablesToJoin, full_join, by = "Inj Nr")

  # remove all trailing rows from output dataframe which only contain NA values
  memCoeffOutput <- memCoeffOutput %>%
    arrange(desc(`Inj Nr`)) %>%
    filter(cumany(!is.na(memoryCoeffD18O))) %>%
    arrange(`Inj Nr`)
  
  return(memCoeffOutput)
}

#' Apply memory correction
#'
#' Apply a memory correction to a specific data set given a set of memory
#' coefficients for d18O and dD as a function of the injection number.
#'
#' Note that if the number of supplied memory coefficients does not match the
#' maximum number of sample injections in the given data set, the memory
#' coefficients are padded with \code{1}'s to match the data. The memory
#' correction is applied according to the formula in
#' \code{\link{formulaCorrectMem}}.
#' 
#' @param dataset a data frame with measurement data of a specific data set.
#' @param memoryCoefficients a data frame of memory coefficients; must include
#'   the columns \code{Inj Nr} (injection number), \code{memoryCoeffD18O}
#'   (memory coefficients for d18O) and \code{memoryCoeffDD} (memory
#'   coefficients for dD).
#' @import dplyr
#'
#' @return The input \code{dataset} with the d18O and dD columns corrected for
#'   memory effect.
#' @seealso \code{\link{formulaCorrectMem}}
#' 
applyMemoryCorrection <- function(dataset, memoryCoefficients){
  
  # get list with one dataframe per sample
  samples     <- group_split(dataset, `Identifier 1`, block, `vial_group`)
  sampleOrder <- order(purrr::map_dbl(samples, ~ first(.$Line)))
  samples     <- samples[sampleOrder]
  
  # accumulate result in these vars
  memoryCorrectedO18      <- list()
  memoryCorrectedH2       <- list()
  memoryCorrectedO18[[1]] <- rep(NA, nrow(samples[[1]]))
  memoryCorrectedH2[[1]]  <- rep(NA, nrow(samples[[1]]))
  
  # Store state in these vars. Initialize to mean of last three injections of first sample.
  deltaTruePrevO18 <- mean(tail(samples[[1]]$`d(18_16)Mean`, 3), na.rm = T)
  deltaTruePrevH2  <- mean(tail(samples[[1]]$`d(D_H)Mean`, 3), na.rm = T)
  
  for (i in 2:length(samples)){
    
    sampleData <- samples[[i]]

    # join sample and mem. coeffs., ensure to keep all sample rows (injections)
    joinedData <- left_join(sampleData, memoryCoefficients, by = c("Inj Nr"))

    # set memory coefficients to 1 for all rows that have an injection number
    # greater than the maximum injection number of the memory coefficients
    joinedData[joinedData$`Inj Nr` > max(memoryCoefficients$`Inj Nr`),
               c("memoryCoeffD18O", "memoryCoeffDD")] <- 1
    
    # calculate memory corrected values for the current sample
    o18MemoryCorrected <- formulaCorrectMem(
      joinedData$`d(18_16)Mean`, joinedData$memoryCoeffD18O, deltaTruePrevO18)
    dDMemoryCorrected <- formulaCorrectMem(
      joinedData$`d(D_H)Mean`, joinedData$memoryCoeffDD, deltaTruePrevH2)
    
    # update delta true prev. If the new value is NA use the last known value.
    deltaTruePrevO18New <- mean(o18MemoryCorrected, na.rm = T)
    deltaTruePrevH2New  <- mean(dDMemoryCorrected, na.rm = T)
    if (!is.na(deltaTruePrevO18New)) deltaTruePrevO18 <- deltaTruePrevO18New
    if (!is.na(deltaTruePrevH2New)) deltaTruePrevH2 <- deltaTruePrevH2New
    
    # add memory corr values to result
    memoryCorrectedO18[[i]] <- o18MemoryCorrected
    memoryCorrectedH2[[i]]  <- dDMemoryCorrected
  }
  
  # create output dataframe and return it
  dataset[["d(18_16)Mean"]] <- unlist(memoryCorrectedO18)
  dataset[["d(D_H)Mean"]]   <- unlist(memoryCorrectedH2)
  return(dataset)
}

#' Estimate true sample values
#'
#' To calculate memory coefficients, an estimate of the true values of the
#' standard vials is needed. This function estimates these from the mean of the
#' last three injections of the standard vials, thereby assuming a sufficiently
#' large number of injections to provide good, i.e. memory-free, estimates.
#' 
#' @param dataset a data frame with a block of standard vial measurements;
#'   usually the first standard block at the beginning of the measurement
#'   sequence.
#' @param ... names of column variables which are used to group the data in
#'   \code{block} into unique sets of standard vials; in the default programme
#'   flow of \code{piccr}, \code{Identifier 1} and \code{vial_group} are used.
#' @import dplyr
#'
#' @return The input data set supplemented by four new columns:
#'   \describe{
#'   \item{\code{deltaTrueD18O}:}{the estimated true d18O value of the current
#'     vial.}
#'   \item{\code{deltaTrueDD}:}{the estimated true dD value of the current
#'     vial.}
#'   \item{\code{deltaTruePrevD18O}:}{the estimated true d18O value of the
#'     previous vial.}
#'   \item{\code{deltaTruePrevDD}:}{the estimated true dD value of the previous
#'     vial.}
#' }
#' @seealso \code{\link{groupStandardsInBlocks}},
#'   \code{\link{assignVialsToGroups}}
#' 
getDeltaTrueAndDeltaTruePrevForEachSample <- function(dataset, ...){
  
  lastThreeInj <- dataset %>%
    group_by(...) %>%
    slice((n()-2):n())
  
  deltaTrue <-lastThreeInj  %>%
    summarise(deltaTrueD18O = mean(`d(18_16)Mean`, na.rm = T),
              deltaTrueDD = mean(`d(D_H)Mean`, na.rm = T), 
              Line = min(Line))
  
  deltaTrueInCorrectOrder <- deltaTrue %>%
    arrange(Line) %>%
    select(-Line)
  
  deltaTruePrevD18O <- lag(deltaTrueInCorrectOrder$deltaTrueD18O)
  deltaTruePrevDD <- lag(deltaTrueInCorrectOrder$deltaTrueDD)
  
  deltaTrueAndDeltaTruePrev <- deltaTrueInCorrectOrder %>% 
    tibble::add_column(deltaTruePrevD18O = deltaTruePrevD18O,
                       deltaTruePrevDD = deltaTruePrevDD)
  
  deltasForAllRows <- inner_join(dataset, deltaTrueAndDeltaTruePrev)
  
  return(deltasForAllRows)
}

#' Memory coefficients
#'
#' Formula to calculate memory coefficients; see the package vignette for
#' details.
#' 
#' @param data numeric vector with measured values of a certain sample for
#'   consecutive injections.
#' @param deltaTrue numeric vector with the (estimated) true value of that
#'   sample.
#' @param deltaTruePrev numeric vector with the (estimated) true value of the
#'   previous sample.
#' 
#' @return numeric vector of the same length as \code{data} with the memory
#'   coefficients.
#' 
formulaMemCoeff <- function(data, deltaTrue, deltaTruePrev){
  (data - deltaTruePrev) / (deltaTrue - deltaTruePrev)
}

#' Memory correction
#'
#' Formula for the memory correction; see the package vignette for
#' details.
#' 
#' @param data numeric vector with measured sample values.
#' @param memCoeff numeric vector with memory coefficients matching the
#'   injection number of the values in \code{data}.
#' @param deltaTruePrev numeric vector of (estimated) true values of the
#'   respective previous sample of the samples in \code{data}.
#'
#' @return the input vector \code{data} corrected for memory.
#' 
formulaCorrectMem <- function(data, memCoeff, deltaTruePrev){
  data + (1 - memCoeff) / memCoeff * (data - deltaTruePrev)
}
