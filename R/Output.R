##==============================================================================
## OUTPUT.R:
## FUNCTIONS TO OUTPUT CORRECTED AND CALIBRATED PICARRO DATA
## TOGETHER WITH ACCURACY ESTIMATES OF CAORRECTION/CALIBRATION PROCEDURE
##
## author: Thomas Muench (tmuench@awi.de)
##==============================================================================

##' Write "piccr" output
##'
##' Write the corrected and calibrated isotope data from all read files to disk,
##' alongside with statistical information on the quality of the
##' correction/calibration approach.
##' @param calibration.results the output from
##' \code{\link{CalibratePicarroData}}.
##' @param meta.info_folder the output from \code{\link{get.input_folder}}.
##' @author Thomas Münch
WriteOutput <- function(calibration.results, meta.info_folder) {


    #===========================================================================
    # get corrected and calibrated data and error estimates of calibration

    ## file data
    folder.iso_data.calibrated=calibration.results$folder.iso_data.calibrated

    ## on a file basis:

    ## root-mean square deviation of inter.block standards
    ## after correction/calibration procedure;
    ## given as vectors
    rmsd.calib_delta.O18_files=calibration.results$rmsd.calib_delta.O18_files
    rmsd.calib_delta.H2_files=calibration.results$rmsd.calib_delta.H2_files

    ## absolute bias of inter.block standards after correction/calibration
    ## procedure; given as list
    std_bias=calibration.results$std_bias

    ## pooled standard deviation of last three injections of sample data;
    ## given as list
    pooledSD=calibration.results$sample_data.pooledSD

    ## standard deviation of first-block calibration residuals
    sd.residuals_delta.O18_files=
        calibration.results$sd.residuals_delta.O18_files
    sd.residuals_delta.H2_files=
        calibration.results$sd.residuals_delta.H2_files


    #===========================================================================
    # output of corrected+calibrated data

    ## create output directory if it does not already exist
    out_dir=paste(meta.info_folder$folder_path,'calibrated/',sep='')
    if (!file.exists(out_dir))
        dir.create(out_dir)

    ## save as R data file
    save(folder.iso_data.calibrated,
         file=paste(out_dir,basename(meta.info_folder$folder_path),
                    '.calibrated','.rdat',sep=''))

    ## save as csv text files
    lapply(folder.iso_data.calibrated$isotope_data,CSV.Output,
           out_dir,meta.info_folder$flag.CSV_Output)


    #===========================================================================
    # write output of accuracy estimates

    ## convert pooled sd data
    pooledSD3Inj_delta.O18=sapply(pooledSD,function(x){mean(x['delta.O18'])})
    pooledSD3Inj_delta.H2=sapply(pooledSD,function(x){mean(x['delta.H2'])})

    ## create data frame of file values
    FileNo=1:length(folder.iso_data.calibrated$isotope_data)
    df.file=data.frame(
        FileNo,
        rmsd_delta.O18=rmsd.calib_delta.O18_files,
        rmsd_delta.H2=rmsd.calib_delta.H2_files,
        pooledSD3Inj_delta.O18,pooledSD3Inj_delta.H2,
        sd.residuals_delta.O18=sd.residuals_delta.O18_files,
        sd.residuals_delta.H2=sd.residuals_delta.H2_files)

    ## create data frame of means
    df.mean=data.frame(
        mean.rmsd_delta.O18=mean(rmsd.calib_delta.O18_files),
        mean.rmsd_delta.H2=mean(rmsd.calib_delta.H2_files),
        mean.pooledSD3Inj_delta.O18=mean(pooledSD3Inj_delta.O18),
        mean.pooledSD3Inj_delta.H2=mean(pooledSD3Inj_delta.H2))


    #===========================================================================
    # write output

    ## output file name
    out.file=paste(out_dir,'OutputAccuracy.txt',sep='')

    ## wrapper function to format data frame
    Format.Col <- function(col){
        if (is.numeric(col)) return(as.numeric(sprintf("%.3f",col)))
        else return(col)
    }

    write('### FILE MEANS: ###',file=out.file,sep="\n")
    write('',file=out.file,sep="\n",append=TRUE)
    suppressWarnings(
        write.table(as.data.frame(lapply(df.mean,Format.Col)),
                    file=out.file,sep=',',dec='.',row.names=FALSE,append=TRUE))

    write('',file=out.file,sep="\n",append=TRUE)
    write('### VALUES FOR EACH FILE: ###',file=out.file,sep="\n",append=TRUE)
    write('',file=out.file,sep="\n",append=TRUE)
    suppressWarnings(
        write.table(as.data.frame(lapply(df.file,Format.Col)),
                    file=out.file,sep=',',dec='.',row.names=FALSE,append=TRUE))


    #===========================================================================
    # write output of calibration bias of inter.block standards

    ## get dimnsions
    no_files=length(FileNo)
    no_blocks=dim(std_bias[[1]]$delta.O18)[1]
    no_std=dim(std_bias[[1]]$delta.O18)[2]

    ## create array from bias data
    bias.arr=array(dim=c(no_blocks*no_files,no_std*2))

    for (ix.file in 1:no_files){
        bias.arr[(ix.file-1)*no_blocks+(1:no_blocks),1:no_std]=
            std_bias[[ix.file]]$delta.O18
        bias.arr[(ix.file-1)*no_blocks+(1:no_blocks),(no_std+1):(2*no_std)]=
            std_bias[[ix.file]]$delta.H2
    }

    ## name array columns
    colnames(bias.arr) <-
        c(paste(colnames(std_bias[[1]]$delta.O18),'delta.O18',sep='_'),
          paste(colnames(std_bias[[1]]$delta.O18),'delta.H2',sep='_'))

    ## convert array to data.frame
    df=as.data.frame(bias.arr)

    ## create output data.frame with meta info of file no. and block no., and
    ## bias data
    df.out=data.frame(FileNo=sort(rep(1:no_files,no_blocks)))
    df.out$block=rep(rownames(std_bias[[1]]$delta.O18),no_files)
    df.out[names(df)]=df[names(df)]

    ## write output
    write('',file=out.file,sep="\n",append=TRUE)
    write('### INTER STANDARD BIAS TO LITERATURE VALUES FOR EACH FILE: ###',
          file=out.file,sep="\n",append=TRUE)
    write('',file=out.file,sep="\n",append=TRUE)
    suppressWarnings(
        write.table(as.data.frame(lapply(df.out,Format.Col)),
                    file=out.file,sep=',',dec='.',row.names=FALSE,append=TRUE))


    ### END OF FUNCTION ###
}

##' csv output
##'
##' Wrapper function to produce the csv file output of the corrected and
##' calibrated isotope data.
##' @param file_data a data frame with the isotope data to write.
##' @param out_dir character string with the path to the output directory.
##' @param flag.CSV_Output integer; flag to signal which output scheme
##' to use (1 = only output the sample data; 2 = output sample as well
##' as standard data in the original order of the read files).
##' @author Thomas Münch
CSV.Output <- function(file_data, out_dir, flag.CSV_Output) {

    out_file=paste(paste(file_data$meta.info_file$deviceID,
                         file_data$meta.info_file$measurement_date,sep='_'),
                   'csv',sep='.')

    if (flag.CSV_Output==1) {
        data_out=file_data$sample_data
    } else if (flag.CSV_Output==2) {
        data_out=file_data$file_data
    } else {
        stop('\nInvalid flag for CSV output.\n')
    }

    LineNo=1:dim(data_out)[1]
    
    write.csv(cbind(LineNo,data_out),
              file=paste(out_dir,out_file,sep=''),
              row.names=FALSE)


}

