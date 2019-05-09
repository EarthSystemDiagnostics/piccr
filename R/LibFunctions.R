##===================================================================================
## LIBFUNCTIONS.R:
## COLLECTION OF LIBRARY FUNCTIONS
##
## author: Thomas Muench (tmuench@awi.de)
##===================================================================================

##' Pass vector
##'
##' Pass vector to function.
##' @param x numeric vector.
##' @param FUN function to apply to \code{x}.
##' @param params additional parameters passed on to \code{FUN}.
##' @return the result of \code{FUN} applied on \code{x}.
##' @author Thomas Münch
PassVector <- function(x, FUN, params) return(FUN(x,params))

##' Pass list to lapply
##'
##' Pass list to lapply with specified functions.
##' @param x a list.
##' @param wrapperFUN function to use in call to \code{lapply}.
##' @param FUN function argument passed on to \code{wrapperFUN}.
##' @param params additional parameters passed on to \code{FUN}.
##' @return a list of the results of \code{wrapperFUN} applied on \code{x}.
##' @author Thomas Münch
GetSubList <- function(x, wrapperFUN, FUN, params)
    return(lapply(x,wrapperFUN,FUN,params))

##' Pass object to apply
##'
##' Pass an object to apply with specified function.
##' @param x an object that can be passed to \code{apply}.
##' @param FUN function to use in call to \code{apply}.
##' @param params additional parameters passed on to \code{FUN}.
##' @return a object of the same type as \code{x} of the results of \code{FUN}
##' applied on the columns of \code{x}.
##' @author Thomas Münch
GetSubVec <- function(x, FUN, params)
    return(apply(x,2,FUN,params))

##' See PassVector
##'
##' See \code{\link{PassVector}}!?
##' @inheritParams PassVector
##' @author Thomas Münch
GetSubArr <- function(x, FUN, params)
    return(FUN(x,params))

##' Last N elements
##'
##' Return the last N elements of a vector.
##' @param x a numeric vector.
##' @param params a list containing an element \code{N} giving the number of the
##' last \code{N} elements to extract.
##' @return numeric vector of length \code{N}. 
##' @author Thomas Münch
GetLastN <- function(x, params) {
    if (!'N' %in% names(params)) stop('Argument N missing in function GetLastN()')
    return(x[(length(x)-(params$N-1)):length(x)])
}

##' Mean of last N elements
##'
##' Return the mean of the last N elements of a vector.
##' @param x a numeric vector.
##' @param params a list containing an element \code{N} giving the number of the
##' last \code{N} elements to average.
##' @return numeric value with the average of the \code{N} elements.
##' @author Thomas Münch
GetMeanLastN <-  function(x, params) {
    if (!'N' %in% names(params)) stop('Argument N missing in function GetMeanLastN()')
    return(mean(x[(length(x)-(params$N-1)):length(x)],na.rm=TRUE))
}

##' SD of last N elements
##'
##' Return the standard deviation of the last N elements of a vector.
##' @param x a numeric vector.
##' @param params a list containing an element \code{N} giving the number of the
##' last \code{N} elements to average.
##' @return numeric value with the standard deviation of the \code{N} elements.
##' @author Thomas Münch
GetSDLastN <-  function(x, params) {
    if (!'N' %in% names(params)) stop('Argument N missing in function GetSDLastN()')
    return(sd(x[(length(x)-(params$N-1)):length(x)],na.rm=TRUE))
}

##' First N elements
##'
##' Return the first N elements of a vector.
##' @param x a numeric vector.
##' @param params a list containing an element \code{N} giving the number of the
##' first \code{N} elements to extract.
##' @return numeric vector of length \code{N}. 
##' @author Thomas Münch
GetFirstN <-  function(x, params) {
    if (!'N' %in% names(params)) stop('Argument N missing in function GetFirstN()')
    return(x[1:params$N])
}

##' Mean of first N elements
##'
##' Return the mean of the first N elements of a vector.
##' @param x a numeric vector.
##' @param params a list containing an element \code{N} giving the number of the
##' first \code{N} elements to average.
##' @return numeric value with the average of the \code{N} elements.
##' @author Thomas Münch
GetMeanFirstN <-  function(x, params) {
    if (!'N' %in% names(params))
        stop('Argument N missing in function GetMeanFirstN()')
    return(mean(x[1:params$N],na.rm=TRUE))
}

##' Extract block variable
##'
##' Extract a certain variable for a given standard from the standard data of a
##' given standard block of the measurement sequence (*sigh*).
##' @param x character string with the name of the standard for which to extract
##' \code{var} (I think...).
##' @param std_data data frame with the standard data from a certain measurement
##' file.
##' @param block.no the number of the block within the measurement sequence.
##' @param var the variable to extract.
##' @return the extracted variable.
##' @author Thomas Münch
ExtractBlockVariable <- function(x, std_data, block.no, var) {

    block.str=paste('block',block.no,sep='')

    if (!block.str %in% names(std_data[[x]])){
        res=NA}
    else{
        if (length(std_data[[x]][[block.str]])==4){
            res=std_data[[x]][[block.str]][var]}
        else{
            res=std_data[[x]][[block.str]][,var]}
    }

    return(res)
}

##' Data of a specific injection
##'
##' Obtain all sample data for a given injection number.
##' @param x integer; the injection number for which to extract the data.
##' @param sample_data data frame with the sample data from a certain measurement
##' file.
##' @return a data frame with the extracted data.
##' @author Thomas Münch
ReturnInjNoData <- function(x, sample_data)
    return(sample_data[which(sample_data[,'injection.no']==x),])

##' Fit exponential
##'
##' Fit an exponential decay function to memory coefficients.
##'
##' The following exponential fit is applied:
##' \code{mem.coeff ~ 1 - a * exp(-b * inj.no)}.
##' @param inj_no numeric vector of injection numbers corresponding to the
##' memory coefficients in \code{mem.coeff}.
##' @param mem.coeff numeric vector of estimated memory coefficients to fit.
##' @return a list with the two fit parameters \code{a} and \code{b}; see
##' details.
##' @author Thomas Münch
FitExponential <- function(inj_no, mem.coeff) {

    ## fit exponential for memory correction
    exp.model=nls(mem.coeff~1-a*exp(-b*inj_no),start=list(a=0.1,b=1),
                  control=nls.control(maxiter=500,
                                      minFactor=1/4096,
                                      warnOnly=TRUE))
    a=coef(exp.model)[1]
    b=coef(exp.model)[2]

    return(list(a=a,b=b))
}

##' Data output format
##'
##' Format isotope data for text file output, i.e. strip the line and injection
##' numbers from the data and retain only the isotope data complemented by the
##' d-excess value.
##' @param x a numeric vector of Picarro data for a given measurement injection
##' sequence.
##' @return the isotopic data from \code{x}.
##' @author Thomas Münch
FinalDataFormat <- function(x) {
    
    ## remove columns of 'line.numbers' and 'injection.no' -> not needed for output
    x=x[-match(c('line.numbers','injection.no'),names(x))]
    
    ## introduce column of d-excess values
    x['d.excess']=x['delta.H2']-8*x['delta.O18']
    x['sd.d.excess']=sqrt((x['sd.H2'])^2+64*(x['sd.O18'])^2)

    return(x)
}

##' Output file data
##'
##' Convert corrected and calibrated standard and sample data back to a data
##' frame for output.
##' @param std_data a list structure with the corrected and calibrated
##' standard data.
##' @param sample_data a data frame with the corrected and calibrated
##' sample data.
##' @return a data frame with both data types merged together in the same format
##' as the input data files.
##' @author Thomas Münch
StackCalibData <- function(std_data, sample_data) {

    ## unlist standard data and convert to data.frame
    tmp=unlist(std_data,recursive=FALSE)
    tmp.df=data.frame(t(sapply(1:length(tmp),function(x){tmp[[x]]})))
    tmp.df$sample_names=names(tmp)

    ## move sample_names to first column for consistency with other data.frames
    tmp.df=tmp.df[c(dim(tmp.df)[2],1:(dim(tmp.df)[2]-1))]

    ## stack standard data and sample data into single data.frame
    total=rbind(tmp.df,sample_data)
    ## sort data.frame by line.numbers
    file_data=total[order(total$line.numbers),]

    return(file_data)
}

