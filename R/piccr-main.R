##===================================================================================
## MAIN FUNCTION OF piccr
##
## author: Thomas Muench (tmuench@awi.de)
##===================================================================================

##' "piccr" main routine
##'
##' Correct and calibrate raw Picarro CRDS data.
##' @param dir the directory path as a character string where the raw Picarro
##' data files and the meta information file are located.
##' @return \code{0} returned invisibly for a successful programme run.
##' @author Thomas Münch
##' @export
piccr <- function(dir = getwd()) {

    cat("\n=== Starting piccr ===\n")


    #===========================================================================
    # ERROR CHECKING

    if (!dir.exists(dir)) stop("Given directory not found.")

    file <- file.path(dir, "INPUT.txt")
    if (!file.exists(file)) {
        stop(sprintf("Mandatory file INPUT.txt not found in %s."), dir)
    } else {
        if (file.access(file) != 0) stop("INPUT.txt not readable.")
    }


    #===========================================================================
    # META INPUT

    cat("Reading meta input...")

    ## read meta input
    meta.info_folder <- get.input_folder(dir)

    cat("done.\n")


    #===========================================================================
    # READ RAW DATA FROM FILES

    cat("Reading data...")

    ## call reading routine
    isotope_data <- wrapper.ReadDataFiles(meta.info_folder)

    ## store file data and meta information as list structure
    folder.iso_data <- list(isotope_data = isotope_data,
                         meta.info_folder = meta.info_folder)

    cat("done.\n")


    #===========================================================================
    # CORRECT AND CALIBRATE RAW DATA

    cat("Calibrating data...")

    calibration.results <- CalibratePicarroData(
        folder.iso_data,
        meta.info_folder$flag.MemCorr,
        meta.info_folder$flag.Calibration)

    cat("done.\n")


    #===========================================================================
    # OUTPUT OF CALIBRATED DATA AND ACCURACY ESTIMATES

    cat("Writing output...")

    WriteOutput(calibration.results, meta.info_folder)

    cat("done.\n")


    #===========================================================================
    # END

    cat("\nProgramme run successful.\n\n")

    invisible(0)

}
