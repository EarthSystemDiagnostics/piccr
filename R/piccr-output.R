#' Documentation of piccr output list
#' @return A named list with 13 components:
#'   \describe{
#'   \item{\code{name}:}{the file name of the data set.}
#'   \item{\code{raw}:}{a data frame with the raw measurement data.}
#'   \item{\code{memoryCorrected}:}{a data frame with the memory-corrected
#'     measurement data, or \code{NULL} if no memory correction was applied.}
#'   \item{\code{calibrated}:}{a data frame with the calibrated measurement
#'     data based either on raw or memory-corrected data.}
#'   \item{\code{calibratedAndDriftCorrected}:}{a data frame with the calibrated
#'     and drift-corrected measurement data, or \code{NULL} if no drift
#'     correction was applied.}
#'   \item{\code{processed}:}{a data frame with the corrected and calibrated
#'     measurement data averaged over a specified number of injections.}
#' 
#'   \item{\code{memoryCoefficients}:}{a data frame with the estimated memory
#'     coefficients.}
#'   \item{\code{deviationsFromTrue}:}{data frame with the measured (and
#'     processsed) value (d18O and dH), the expected value, and the deviation of
#'     the measured from the expected value for each analysed standard.}
#'   \item{\code{rmsdDeviationsFromTrue}:}{a list with the two elements
#'     \code{d18O} and \code{dD} with the respective root mean square deviation
#'     across all deviations in \code{deviationsFromTrue} (with the very first
#'     standard of the measurement sequence excluded).}
#'   \item{\code{deviationOfControlStandard}:}{a list with the two elements
#'     \code{d18O} and \code{dD} of the deviations from the expected value for
#'     the quality control standard(s) specified in the config file.}
#'   \item{\code{pooledSD}:}{a list with the two elements \code{d18O}
#'     and \code{dD} of the pooled standard deviations across all measured
#'     vials.}
#'   \item{\code{calibrationParams}:}{a tibble with the estimated calibration
#'   parameters for the first standard block (and, if double calibration was
#'   active, the final block) of the measurement sequence.}
#'   \item{\code{driftParams}:}{estimated drift parameters; not yet
#'     implemented.}
#' }
#' @name piccr_output
NULL
