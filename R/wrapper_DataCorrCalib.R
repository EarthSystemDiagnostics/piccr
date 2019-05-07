##===================================================================================
## WRAPPER_DATACORRCALIB.R:
## WRAPPER FUNCTION TO CORRECT AND CALIBRATE ISOTOPE DATA
## OBTAINED FROM PICARRO MEASUREMENTS
##
## author: Thomas Muench (tmuench@awi.de)
##===================================================================================

##' Correct and calibrate raw data
##'
##' Main function to correct and calibrate the raw Picarro data.
##' @param folder.iso_data a list with two elements: (1) the isotope data as the
##' result from the call to \code{\link{wrapper.ReadDataFiles}}; (2) the meta
##' information on the measurement sequence as the result from the call to
##' \code{\link{get.input_folder}}.
##' @param flag.MemCorr integer; flag to signal if a memory correction shall be
##' applied (1 = YES, 0 = NO)
##' @param flag.Calibration integer; flag to signal which calibration scheme
##' shall be applied (1 = linear drift correction + single three-point
##' calibration at the beginning of the measurement sequence; 2 = double
##' three-point calibration at the beginning and at the end of the measurement
##' sequence).
##' @return a list structure with the corrected and calibrated isotope data
##' together with statistical data on the quality of the approach.
##' @author Thomas Münch
CalibratePicarroData <- function(folder.iso_data,
                                 flag.MemCorr,
                                 flag.Calibration) {


    #---------------------------------------------------------------------------
    # result isotope data variable

    ## result data initialized as input data - the uncalibrated input data will
    ## then be overwritten with the corresponding calibrated data
    folder.iso_data.calibrated=folder.iso_data

    ## storage vectors for file means of rmsd after calibration
    rmsd.calib_delta.O18_files=vector()
    rmsd.calib_delta.H2_files=vector()

    ## storage list for std bias of inter block measurements
    std_bias=list()

    ## storage list for sample data's pooled standard deviation
    ## of the last three injections
    sample_data.pooledSD=list()

    ## storage vectors for standard deviation of calibration residuals for the
    ## first-block linear standard calibration
    sd.residuals_delta.O18_files=vector()
    sd.residuals_delta.H2_files=vector()


    #---------------------------------------------------------------------------

    ## number of data files in current folder
    no_files=length(folder.iso_data$isotope_data)

    ## meta information on standard measurements for files in this folder
    meta.info_std=folder.iso_data$meta.info_folder$meta.info_std


    #---------------------------------------------------------------------------
    # loop over files in current folder

    for (ix.file in (1:no_files)){

        ## total data content of this file
        file_data=
            folder.iso_data$isotope_data[[ix.file]]$raw.file_data
        
        ## standard data in this file
        std_data=
            folder.iso_data$isotope_data[[ix.file]]$std_data

        ## isotope data in this file (sample data)
        sample_data=
            folder.iso_data$isotope_data[[ix.file]]$sample_data

        ## meta info file
        meta.info_file=
            folder.iso_data$isotope_data[[ix.file]]$meta.info_file


        #-----------------------------------------------------------------------
        # apply memory correction to standard and sample data

        if (flag.MemCorr)
        {

            ## apply memory correction
            res.MemoryCorr=MemoryCorrection(file_data,
                                            std_data,
                                            sample_data,
                                            meta.info_std)

            ## corrected std data
            std_data=res.MemoryCorr$std_data
            ## corrected sample data
            sample_data=res.MemoryCorr$sample_data

        }


        #-----------------------------------------------------------------------
        # call standard calibration function

        res.calibration=
            wrapper.StandardCalibration(std_data,meta.info_std,flag.Calibration)

        ## calibrated standard data
        std_data=res.calibration$std_data

        ## root mean square deviation of inter block std.s after calibration for
        ## this file
        rmsd.calib_delta.O18_files[ix.file]=res.calibration$rmsd$delta.O18
        rmsd.calib_delta.H2_files[ix.file]=res.calibration$rmsd$delta.H2

        ## bias of inter standards after calibration for this file
        std_bias[[ix.file]]=res.calibration$std_bias

        ## standard deviation of first-block calibration residuals
        sd.residuals_delta.O18_files[ix.file]=
            res.calibration$sd.residuals$delta.O18
        sd.residuals_delta.H2_files[ix.file]=
            res.calibration$sd.residuals$delta.H2


        #-----------------------------------------------------------------------
        # drift-correct and calibrate sample data

        sample_data.calibrated=
            SampleCalibration(sample_data,res.calibration,flag.Calibration)


        #-----------------------------------------------------------------------
        # get mean and pooled standard deviation of the last three injections of
        # sample data

        sample_data.MeanPooledSD=
            TakeMean_CalcPooledSD.sample_data(sample_data.calibrated)

        ## mean
        sample_data.mean=sample_data.MeanPooledSD$sample_data
        ## pooled sd
        sample_data.pooledSD[[ix.file]]=sample_data.MeanPooledSD$pooled.sd


        #-----------------------------------------------------------------------
        # final output formatting of data

        ## create new data.frame of stacked standard and sample data in correct
        ## order; -> needed for potential output if desired
        file_data=StackCalibData(std_data,sample_data.mean)

        ## bring sample-, standard- and file-data.frames to final output format
        sample_data.mean=FinalDataFormat(sample_data.mean)
        std_data=lapply(std_data,function(x){lapply(x,FinalDataFormat)})
        file_data=FinalDataFormat(file_data)


        #-----------------------------------------------------------------------
        # copy calibrated data to output data file

        ## calibrated standard data

        folder.iso_data.calibrated$isotope_data[[ix.file]]$std_data=
            std_data

        ## calibrated sample data

        folder.iso_data.calibrated$isotope_data[[ix.file]]$sample_data=
            sample_data.mean

        ## calibrated total file data

        folder.iso_data.calibrated$isotope_data[[ix.file]]$file_data=
            file_data


        #-----------------------------------------------------------------------

        ### END OF FILE LOOP
    }


    #---------------------------------------------------------------------------
    # return

    return.results=list(
        folder.iso_data.calibrated=folder.iso_data.calibrated,
        rmsd.calib_delta.O18_files=rmsd.calib_delta.O18_files,
        rmsd.calib_delta.H2_files=rmsd.calib_delta.H2_files,
        sd.residuals_delta.O18_files=sd.residuals_delta.O18_files,
        sd.residuals_delta.H2_files=sd.residuals_delta.H2_files,
        std_bias=std_bias,
        sample_data.pooledSD=sample_data.pooledSD
    )

    return(return.results)


    ### END OF FUNCTION ###
}

