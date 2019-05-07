##===================================================================================
## LIBFUNCTIONS.R:
## COLLECTION OF LIBRARY FUNCTIONS
##
## author: Thomas Muench (tmuench@awi.de)
##===================================================================================


#====================================================================================
PassVector <- function(x,FUN,params) return(FUN(x,params))

#====================================================================================
GetSubList <- function(x,wrapperFUN,FUN,params)
    return(lapply(x,wrapperFUN,FUN,params))

#====================================================================================
GetSubVec <- function(x,FUN,params)
    return(apply(x,2,FUN,params))

#====================================================================================
GetSubArr <- function(x,FUN,params)
    return(FUN(x,params))

#====================================================================================
GetLastN <- function(x,params){
    if (!'N' %in% names(params)) stop('Argument N missing in function GetLastN()')
    return(x[(length(x)-(params$N-1)):length(x)])
    }

#====================================================================================
GetMeanLastN <-  function(x,params){
    if (!'N' %in% names(params)) stop('Argument N missing in function GetMeanLastN()')
    return(mean(x[(length(x)-(params$N-1)):length(x)],na.rm=TRUE))
}

#====================================================================================
GetFirstN <-  function(x,params){
    if (!'N' %in% names(params)) stop('Argument N missing in function GetFirstN()')
    return(x[1:params$N])
}

#====================================================================================
GetMeanFirstN <-  function(x,params){
    if (!'N' %in% names(params))
        stop('Argument N missing in function GetMeanFirstN()')
    return(mean(x[1:params$N],na.rm=TRUE))
}

#====================================================================================
ExtractBlockVariable <- function(x,std_data,block.no,var){

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

#====================================================================================
ReturnInjNoData <- function(x,sample_data)
    return(sample_data[which(sample_data[,'injection.no']==x),])

#====================================================================================
FitExponential <- function(inj_no,mem.coeff){

## fit exponential for memory correction
exp.model=nls(mem.coeff~1-a*exp(-b*inj_no),start=list(a=0.1,b=1),
    control=nls.control(maxiter=500,minFactor=1/4096,warnOnly=TRUE))
a=coef(exp.model)[1]
b=coef(exp.model)[2]

return(list(a=a,b=b))
}

#====================================================================================
FinalDataFormat <- function(x){
    
    ## remove columns of 'line.numbers' and 'injection.no' -> not needed for output
    x=x[-match(c('line.numbers','injection.no'),names(x))]
    
    ## introduce column of d-excess values
    x['d.excess']=x['delta.H2']-8*x['delta.O18']

    return(x)
}

#====================================================================================
StackCalibData <- function(std_data,sample_data){

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


