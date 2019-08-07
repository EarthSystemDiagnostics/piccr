##===================================================================================
## DATAHANDLING.R:
## FUNCTION TO CALCULATE MEAN AND POOLED STANDARD DEVIATION
## OF THE LAST THREE INJECTIONS OF THE SAMPLE DATA
##
## author: Thomas Muench (tmuench@awi.de)
##===================================================================================

##' Sample means and pooled standard deviations
##'
##' Function to calculate the mean and the pooled standard deviation over the
##' last three injections of the sample data.
##' @param sample_data a data frame with the sample data for a specific
##' measurement file.
##' @return a two element list: (1) a data frame similar to \code{sample_data}
##' with the mean sample data of the last three injections; (2) an array with
##' the pooled standard deviations.
##' @author Thomas Münch
TakeMean_CalcPooledSD.sample_data <- function(sample_data){


    #---------------------------------------------------------------------------
    # get sample data sorted by injections

    ## maximum injection number
    max.inj_no=max(sample_data[,'injection.no'])

    ## create auxiliary variable containing the numeric data stored in
    ## 'sample_data' as a matrix
    aux=sample_data
    aux$sample_names <- NULL
    aux=data.matrix(aux)

    ## concatenate all sample data into a vector corresponding to ascending
    ## injection no.s
    cbind_injections.sample_data=c(sapply((1:max.inj_no),ReturnInjNoData,aux))


    #---------------------------------------------------------------------------
    # partition sample data in a 3D array:
    # -> rows = sample number; columns = data type (LineNo, InjNo, O18, H2);
    #    third dimension = injection number

    ## dimension of array
    dim.sample_data.partitioned=
        c(dim(aux)[1]/max.inj_no,dim(aux)[2],max.inj_no)

    ## partition data in array if file contains more than one sample in total
    if (dim.sample_data.partitioned[1]!=1)    
        sample_data.array=array(cbind_injections.sample_data,
                                dim.sample_data.partitioned)


    #---------------------------------------------------------------------------
    # take mean of last three injections/calculate pooled standard deviation

    if (dim.sample_data.partitioned[1]==1){
        
        ## file contains only one sample in total
        
        ## take mean - convert result to array for consistency
        sample_data.MeanLast3Inj=array(colMeans(aux),dim=c(1,4))
        ## calculate pooled sd
        mean.var=apply(aux[,c('delta.O18','delta.H2')],2,var,na.rm=TRUE)
        pooled.sd=sqrt(mean.var)
        mean.sd=pooled.sd
        
    } else {
        
        ## get index range of the last three injections
        range=((max.inj_no-2):max.inj_no)

        ## correct range if less than three injections were measured in total
        if (max.inj_no<3) range=(1:max.inj_no)

        ## take mean
        sample_data.MeanLast3Inj=apply(sample_data.array[,,range],c(1,2),
                                       mean,na.rm=TRUE)
        ## calculate pooled sd
        mean.var=apply(sample_data.array[,c(3,4),range],c(1,2),var,na.rm=TRUE)
        mean.sd=sqrt(mean.var)
        pooled.sd=sqrt(colMeans(mean.var))
    }

    ## name columns again as these information has been lost in above process
    colnames(sample_data.MeanLast3Inj) <- colnames(aux)
    names(pooled.sd) <- c('delta.O18','delta.H2')


    #---------------------------------------------------------------------------
    # reduce vector of sample names to length of mean sample data

    sample_names.mean=
        sample_data[which(sample_data[,'injection.no']==max.inj_no),
                    'sample_names']


    #---------------------------------------------------------------------------
    # convert data back to data.frame

    sample_data=as.data.frame(sample_data.MeanLast3Inj)
    sample_data$sample_names=sample_names.mean

    ## move sample_names back to first column - only for later file output
    sample_data=sample_data[c(dim(sample_data)[2],1:(dim(sample_data)[2]-1))]

    ## add standard deviation of the mean
    sample_data$sd.O18=mean.sd[, 1]
    sample_data$sd.H2=mean.sd[, 2]


    #---------------------------------------------------------------------------
    # return data

    res=list(sample_data=sample_data,pooled.sd=pooled.sd)

    return(res)


    ### END OF FUNCTION ###
}

