##===================================================================================
## READIN.R:
## FUNCTIONS TO READ IN PICARRO DATA FILES
##
## author: Thomas Muench (tmuench@awi.de)
##===================================================================================

##' Read in Picarro data
##'
##' Wrapper function to read in Picarro data from all files in a given folder.
##' @param meta.info_folder the output from \code{\link{get.input_folder}}.
##' @return a list structure with the Picarro data from all files.
##' @author Thomas Münch
wrapper.ReadDataFiles <- function(meta.info_folder) {


    #---------------------------------------------------------------------------
    # get all files in given folder

    data_files=
        Sys.glob(paste(meta.info_folder$folder_path,
                       meta.info_folder$fileID, sep=''))


    #---------------------------------------------------------------------------
    # call routine to read in data from these files

    file_data=lapply(data_files,ReadDataFiles,meta.info_folder)


    #---------------------------------------------------------------------------
    # return data

    return(file_data)


    ### END OF FUNCTION ###
}

##' Read in Picarro data
##'
##' Read in Picarro data from file.
##' @param file character string with the path to the Picarro file which shall
##' be read.
##' @inheritParams wrapper.ReadDataFiles
##' @return a list strucure with the file data and meta information.
##' @author Thomas Münch
ReadDataFiles <- function(file, meta.info_folder) {


    #---------------------------------------------------------------------------
    # read in file data

    ## read file
    file_data=read.table(file, header = TRUE, quote = "\"",
                         sep = meta.info_folder$files.column_sep,
                         dec = meta.info_folder$files.dec_sep,
                         strip.white = TRUE)

    ## convert column Identifier1 to character
    ## (not harmful if already IS of type character)
    file_data$Identifier.1=as.character(file_data$Identifier.1)


    #---------------------------------------------------------------------------
    # get relevant file data and store it as data frame

    file_data.df=data.frame(
        sample_names=file_data$Identifier.1,
        line.numbers=1:dim(file_data)[1],
        injection.no=file_data$Inj.Nr,
        delta.O18=file_data$d.18_16.Mean,
        delta.H2=file_data$d.D_H.Mean,
        stringsAsFactors=FALSE
    )

    ## error checking;
    ## look for NA's in the data, if found, issue a warning and their location
    warn=getOption('warn');options(warn=1)
    if (length(which(is.na(file_data.df),arr.ind=TRUE)->NA.loc)!=0){
        row.loc=unique(NA.loc[,'row'])
        cat('\n')
        warning(sprintf('\nNAs located in file %s:',file),call.=FALSE)
        cat(sprintf('row number(s) of occurrence: %s',
                    paste(sort(unique(NA.loc[,'row'])),collapse=' ')),'\n\n')
    }
    options(warn=warn)


    #---------------------------------------------------------------------------
    # split file data into standard and sample data

    ## get file indices that match standard vial measurements
    file_indices=sort(unlist(sapply(meta.info_folder$meta.info_std$std_names,
                                    function(x){
                                        which(x==file_data.df$sample_names)})))

    ## error checking
    if (length(file_indices)!=sum(meta.info_folder$meta.info_std$std_scheme))
        stop(sprintf('(from ReadIn.R:88):\nIncorrect number of standard injections read!\nCheck file %s.\n\n',file),call.=FALSE)

    ## get standard data
    file.std_data=ReadStandardData(file_data.df,meta.info_folder$meta.info_std)

    ## get firn sample data = remaining part of file
    file.sample_data=file_data.df[-file_indices,]
    rownames(file.sample_data) <- as.character(1:dim(file.sample_data)[1])


    #---------------------------------------------------------------------------
    # get meta information on file from file path

    ## actual name of data file
    filename=basename(file)

    ## split filename into substrings
    name_split=strsplit(filename,'_')

    ## device ID -> Picarro device that has been used for measurement
    deviceID=name_split[[1]][1]

    ## measurement date -> always next to last of name_split
    len.name_split=length(name_split[[1]])
    measurement_date=as.numeric(name_split[[1]][len.name_split-1])


    #---------------------------------------------------------------------------
    # return variable

    return_data=list()

    return_data$meta.info_file$deviceID=deviceID
    return_data$meta.info_file$measurement_date=measurement_date

    return_data$raw.file_data=file_data.df
    return_data$std_data=file.std_data
    return_data$sample_data=file.sample_data

    return(return_data)


    ### END OF FUNCTION ###
}

##' Read isotope standard data
##'
##' Extract the isotope data of the standard samples from a data frame of read
##' Picarro file data.
##' @param file_data a data frame of Picarro file data.
##' @param meta.info_std the respective sublist of the output from
##' \code{\link{get.input_folder}}.
##' @return a list structure with the standard isotope data.
##' @author Thomas Münch
ReadStandardData <- function(file_data, meta.info_std) {


    #---------------------------------------------------------------------------
    # loop over standards by calling 'PassVector' and extract std data separated
    # into the measurement blocks by calling 'GetStdBlocks' for each standard

    ## parameters
    std_no.arr=cbind(c(1:meta.info_std$std_no))
    params=list(std_names=meta.info_std$std_names,block_no=meta.info_std$block_no,
                std_scheme=meta.info_std$std_scheme,file_data=file_data)

    ## read standard data
    std_data=apply(std_no.arr,1,PassVector,GetStdBlocks,params)

    ## name the standards within list structure
    names(std_data) <- meta.info_std$std_names


    #---------------------------------------------------------------------------
    # return variable

    return(std_data)


    ### END OF FUNCTION ###
}

##' Split standard data into measurement blocks
##'
##' Split the isotope data of the read in standrad sample measurements into the
##' respective measurement blocks, i.e. the standard measurements at the
##' beginning, the end and possible in between of the measurement sequence.
##' @param ix.std number of the isotope standard to extract.
##' @param params list structure with needed parameters; see the code.
##' @return a list structure with \code{n} elements where \code{n} are the
##' number of different measurement blocks; each element is a matrix containing
##' the isotope data for standard number \code{ix.std}.
##' @author Thomas Münch
GetStdBlocks <- function(ix.std, params) {

    ## +++ ARGUMENTS that have to be passed to this function: +++
    ## +++ ix.std, params = list(std_names, std_scheme, file_data) +++

    ## error checking
    if (!'std_names' %in% names(params) ||
        !'block_no' %in% names(params) ||
        !'std_scheme' %in% names(params) ||
        !'file_data' %in% names(params))
        stop('Missing Arguments in function GetStdBlocks()')

    ## return variable
    res=list()

    ## extract file rows corresponding to standards
    name=params$std_names[ix.std]        
    file_indices=which(name==params$file_data$sample_names)

    ## get entire data set
    delta.O18=params$file_data$delta.O18[file_indices]
    delta.H2=params$file_data$delta.H2[file_indices]

    ##  store in array
    std.data_array=cbind(file_indices,delta.O18,delta.H2)


    ## -------------------------------------------------------------------------
    ## split into sets corresponding to first, intermediate and final
    ## measurements -> number of measurements depending on standard and location
    ## in rack
    ## -------------------------------------------------------------------------
        
    ## index offset at first rack position
    offset=0

    ## create vector of standard block names
    block_names=vector()

    ## count variable of measured blocks
    count=0

    ## loop over number of standard blocks

    for (ix.block in (1:params$block_no)){

        ## name of current standard block
        this.block_name=paste('block',ix.block,sep='')
        
        ## get row index for this standard block
        row.ind=match(this.block_name,rownames(params$std_scheme))

        ## index offset at rack positions behind first block
        if (ix.block!=1)
            offset=offset+params$std_scheme[row.ind-1,ix.std]

        ## check if standard was measured at this block position
        if (params$std_scheme[row.ind,ix.std]!=0)
            {

                ## block was measured for this std:
                ## -> increment count variable, copy block name into vector
                count=count+1
                block_names=c(block_names,this.block_name)
                
                ## range of file indices for standard at this block position
                range=(1:params$std_scheme[row.ind,ix.std]) + offset

                ## injection numbers from measurement scheme
                injection.no=1:params$std_scheme[row.ind,ix.std]
                ## line numbers of data
                line.numbers=std.data_array[range,'file_indices']
                ## delta-O18 data
                delta.O18=std.data_array[range,'delta.O18']
                ## delta-H2 data
                delta.H2=std.data_array[range,'delta.H2']

                ## store in array
                block_data=cbind(line.numbers,injection.no,delta.O18,delta.H2)

                res[[count]]=block_data
            }
    }

    ## copy names of actually measured blocks to list structure
    names(res) <- block_names

    return(res)

    
### END OF FUNCTION ###
}

