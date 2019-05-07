##===================================================================================
## MEMORYCORRECTION.R:
## FUNCTIONS TO PERFORM MEMORY CORRECTION ON THE STANDARD AND SAMPLE DATA
##
## author: Thomas Muench (tmuench@awi.de)
##===================================================================================

##' Perform memory correction
##'
##' Function to obtain memory coefficients and to apply them to memory-correct
##' the sample and standard isotope data.
##' @param file_data data frame of raw isotope data of a specific measurement
##' file.
##' @param std_data subset of \code{file_data} with only the data of the
##' standard samples.
##' @param sample_data subset of \code{file_data} with only the data of the
##' measurement samples.
##' @param meta.info_std the respective sublist of the output from
##' \code{\link{get.input_folder}}.
##' @return a list with two data frames with the memory-corrected standard and
##' sample data.
##' @author Thomas Münch
MemoryCorrection <- function(file_data, std_data, sample_data, meta.info_std) {


    #---------------------------------------------------------------------------
    # get standard memory correction scheme = index numbers of memory correction
    # std.s from user information

    ix.memory.std=which(meta.info_std$std.data_corr['memory.corr',]==1)


    #---------------------------------------------------------------------------
    # calculate memory coefficients

    mem.coeff.O18=array(
        dim=c(length(ix.memory.std),dim(std_data[[ix.memory.std[1]]]$block1)[1]))
    mem.coeff.H2=array(dim=dim(mem.coeff.O18))
    
    for (ix.std in ix.memory.std){
        for (ix.inj in 1:dim(std_data[[ix.memory.std[1]]]$block1)[1])
        {

            ## --- delta-O18 --- ##

            max.inj=max(which(!is.na(std_data[[ix.std-1]]$block1[,'delta.O18'])))
            prev_data.last=std_data[[ix.std-1]]$block1[max.inj,'delta.O18']
            max.inj=max(which(!is.na(std_data[[ix.std]]$block1[,'delta.O18'])))
            curr_data.last=std_data[[ix.std]]$block1[max.inj,'delta.O18']

            curr_data=std_data[[ix.std]]$block1[ix.inj,'delta.O18']
            gradient=prev_data.last-curr_data.last

            mem.coeff.O18[which(ix.std==ix.memory.std),ix.inj]=
                (prev_data.last-curr_data)/gradient

            ## --- delta-H2 --- ##

            max.inj=max(which(!is.na(std_data[[ix.std-1]]$block1[,'delta.H2'])))
            prev_data.last=std_data[[ix.std-1]]$block1[max.inj,'delta.H2']
            max.inj=max(which(!is.na(std_data[[ix.std]]$block1[,'delta.H2'])))
            curr_data.last=std_data[[ix.std]]$block1[max.inj,'delta.H2']

            curr_data=std_data[[ix.std]]$block1[ix.inj,'delta.H2']
            gradient=prev_data.last-curr_data.last

            mem.coeff.H2[which(ix.std==ix.memory.std),ix.inj]=
                (prev_data.last-curr_data)/gradient
        }
    }

    mem.coeff_O18=1-colMeans(mem.coeff.O18,na.rm=TRUE)
    mem.coeff_H2=1-colMeans(mem.coeff.H2,na.rm=TRUE)


    #---------------------------------------------------------------------------
    # correct sample data

    samples.mem_corrected=
        apply(sample_data,1,MemCorrData,file_data,mem.coeff_O18,mem.coeff_H2)

    sample_data.corr=sample_data
    sample_data.corr$delta.O18=samples.mem_corrected[3,]
    sample_data.corr$delta.H2=samples.mem_corrected[4,]

    ## extract first three injections of data and the corresponding sample names
    ix.first3=which(!is.na(match(sample_data.corr[,'injection.no'],c(1,2,3))))
    sample_data.corr=sample_data.corr[ix.first3,]


    #---------------------------------------------------------------------------
    # correct standard data

    std_data.corr=list()
    for (ix.std in 1:meta.info_std$std_no)
        std_data.corr[[ix.std]]=
            wrapper.MemCorrStdData(ix.std,std_data,file_data,
                                   mem.coeff_O18,mem.coeff_H2,
                                   meta.info_std)

    names(std_data.corr) <- meta.info_std$std_names


    #---------------------------------------------------------------------------
    # return memory-corrected data

    res=list()
    res$sample_data=sample_data.corr
    res$std_data=std_data.corr

    return(res)

}


#====================================================================================
# AUXILIARY FUNCTIONS
#====================================================================================

##' Apply memory correction
##'
##' Apply the memory correctino scheme of van Geldern and Barth (2012) on a
##' given measurement (i.e. injection).
##' @param x a four-element numeric vector with the line number, injection
##' number, d18O value and d2H value of a specific injection from the
##' measurement sequence.
##' @param file_data data frame of raw isotope data of a specific measurement
##' file.
##' @param mem.coeff_O18 numeric vector of memory coefficients for oxygen
##' isotope data (d18O).
##' @param mem.coeff_H2 numeric vector of memory coefficients for deuterium
##' isotope data (d2H).
##' @return a four-element numeric vector as \code{x} but with the
##' memory-corrected isotope data.
##' @author Thomas Münch
MemCorrData <- function(x, file_data, mem.coeff_O18, mem.coeff_H2) {

    line.no=as.numeric(x['line.numbers'])
    inj.no=as.numeric(x['injection.no'])
    d18O=as.numeric(x['delta.O18'])
    d2H=as.numeric(x['delta.H2'])

    ## get file line number of last injection of previous sample
    line.no_last=line.no-inj.no

    ## if that injection is NA, go to next previous injection;
    ## but no more than two before
    count=0
    while (is.na(file_data[line.no_last,'delta.O18'])){
        count=count+1
        if (count==3) break
        line.no_last=line.no_last-1
    }

    d18O=d18O+mem.coeff_O18[inj.no]*(d18O-file_data[line.no_last,'delta.O18'])
    d2H=d2H+mem.coeff_H2[inj.no]*(d2H-file_data[line.no_last,'delta.H2'])

    return(c(line.no,inj.no,d18O,d2H))
    
}

##' Apply memory correction
##'
##' Apply the memory correction scheme of van Geldern and Barth (2012) on
##' standard data.
##' @param x the number of the standard to be corrected.
##' @inheritParams MemCorrData
##' @inheritParams MemoryCorrection
##' @return the memory-corrected subset of the standard data of standard
##' no. \code{x}.
##' @author Thomas Münch
wrapper.MemCorrStdData <- function(x, std_data, file_data,
                                   mem.coeff_O18, mem.coeff_H2, 
                                   meta.info_std) {


    block.str=
        paste(rep('block',meta.info_std$block_no),1:meta.info_std$block_no,sep='')

    for (block.no in (1:meta.info_std$block_no)){

        if (block.str[block.no] %in% names(std_data[[x]]))
        {
            if (x==1 && block.no==1) next

            std.corr=
                apply(std_data[[x]][[block.str[block.no]]],1,MemCorrData,
                      file_data,mem.coeff_O18,mem.coeff_H2)

            line.numbers=std.corr[1,]
            injection.no=std.corr[2,]
            delta.O18=std.corr[3,]
            delta.H2=std.corr[4,]
            dat=cbind(line.numbers,injection.no,delta.O18,delta.H2)
            
            std_data[[x]][[block.str[block.no]]]=
                GetSubVec(dat,GetFirstN,params=list(N=3))
        }
    }

    return(std_data[[x]])
}

