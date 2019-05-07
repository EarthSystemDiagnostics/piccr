##===================================================================================
## INPUT.R:
## INPUT FUNCTION TO PREPARE READ IN OF ISOTOPE DATA FROM PICARRO MEASUREMENTS
## -> READ IN INPUT.txt
##
## author: Thomas Muench (tmuench@awi.de)
##===================================================================================

##' Read in meta information file.
##'
##' Read in the meta information file \code{INPUT.txt} containing necessary
##' information on the measurement sequence and the scheme of how to correct the
##' data.
##' @param dir the directory path as a character string where the meta
##' information file is located.
##' @return a list with the information read in from \code{INPUT.txt}.
##' @author Thomas Münch
get.input_folder <- function(dir) {


    #---------------------------------------------------------------------------
    # read in meta input file INPUT.txt line-by-line
    meta=readLines(paste(dir,'INPUT.txt',sep=''))
    meta=meta[-c(1:3,13:16)]


    #---------------------------------------------------------------------------
    # declare list variable to store meta information on data folder

    res=list()


    #---------------------------------------------------------------------------
    # READ META INFO 1 = FIRST PART OF META INPUT FILE

    tag=vector()
    value=vector()

    for (ITAG in 1:9){
        ILINE=gsub('\t','',unlist(strsplit(meta[ITAG],'/')))
        tag[ITAG]=ILINE[1]
        value[ITAG]=ILINE[2]
    }

    names(value) <- tag


    #---------------------------------------------------------------------------
    # STORE META INFO 1 IN LIST STRUCTURE

    ## folder path
    res$folder_path=dir

    ## file identifier
    res$fileID=value['FILE_ID']

    ## column separator for files in given folder
    res$files.column_sep=value['COLUMNSEP']

    ## decimal separator for files in given folder
    res$files.dec_sep=value['DECSEP']

    ## name of standard scheme used for measurements in given folder
    res$meta.info_std$name.std_scheme=value['NAME_StdScheme']

    ## number of standards used for measurements in given folder
    res$meta.info_std$std_no=as.integer(value['STD_NO'])

    ## number of standard blocks employed for measurements in given folder
    res$meta.info_std$block_no=as.integer(value['BLOCK_NO'])

    ## calibration flag
    res$flag.Calibration=as.integer(value['CALIBRATION_FLAG'])

    ## memory-correction flag
    res$flag.MemCorr=as.integer(value['MEMORYCORR_FLAG'])

    ## output flag
    res$flag.CSV_Output=as.integer(value['OUTPUT_FLAG'])


    #---------------------------------------------------------------------------
    # READ META INFO 2 = SECOND PART OF META INPUT FILE

    value=matrix(nrow=res$meta.info_std$std_no,ncol=6+res$meta.info_std$block_no)
    for (ITAG in 1:res$meta.info_std$std_no)
        value[ITAG,]=gsub(' ','',unlist(strsplit(meta[ITAG+9],'/')))


    #---------------------------------------------------------------------------
    # STORE META INFO 2 IN LIST STRUCTURE

    ## standard names
    res$meta.info_std$std_names=value[,1]

    ## literature values of standards
    delta.O18=as.numeric(value[,2]) ## delta-O18
    delta.H2=as.numeric(value[,3])  ## delta-H2
    res$meta.info_std$std_lit_values=rbind(delta.O18,delta.H2)
    colnames(res$meta.info_std$std_lit_values) <- res$meta.info_std$std_names

    ## measurement scheme -> which standard is measured how often
    res$meta.info_std$std_scheme=
        array(dim=c(res$meta.info_std$block_no,res$meta.info_std$std_no))
    block_names=vector()
    for (ix.block in (1:res$meta.info_std$block_no))
    {
        res$meta.info_std$std_scheme[ix.block,]=as.integer(value[,ix.block+3])
        block_names=c(block_names,paste('block',ix.block,sep=''))
    }
    rownames(res$meta.info_std$std_scheme) <- block_names
    colnames(res$meta.info_std$std_scheme) <- value[,1]

    ## logicals giving number of std.s that are to be used for
    ## memory correction, drift correction and calibration, respectively
    memory.corr=as.integer(value[,3+res$meta.info_std$block_no+1])
    drift.corr=as.integer(value[,3+res$meta.info_std$block_no+2])
    calibration=as.integer(value[,3+res$meta.info_std$block_no+3])
    res$meta.info_std$std.data_corr=
        rbind(memory.corr,drift.corr,calibration)
    colnames(res$meta.info_std$std.data_corr) <- res$meta.info_std$std_names


    #---------------------------------------------------------------------------
    # return and end of function

    return(res)

    ### END OF FUNCTION ###
}

