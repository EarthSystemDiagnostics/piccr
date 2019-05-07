##===================================================================================
## DRIFTCORRECTION.R:
## FUNCTION TO CALCULATE A LINEAR DRIFT CORRECTION REGRESSION
## APPLY IT TO THE STANDARD DATA
##
## author: Thomas Muench (tmuench@awi.de)
##===================================================================================


#***************
# MAIN WRAPPER *
#***************
#
########################################################################
LinearDriftCorrection.standard_data <- function(std_data,meta.info_std){
########################################################################


#------------------------------------------------------------------------------------
# get standard drift correction scheme = index numbers of drift correction std.s
# from user information

ix.drift_correction.standards=which(meta.info_std$std.data_corr['drift.corr',]==1)


#------------------------------------------------------------------------------------
# get drift correction slopes

## get isotope data and line numbers corresponding to the last three injections
std_data.Last3=lapply(std_data,GetSubList,GetSubVec,GetLastN,params=list(N=3))

## get mean of first block line numbers used for drift correction
mean.line.no_first.block=mean(sapply(
    ix.drift_correction.standards,
    ExtractBlockVariable,std_data.Last3,1,'line.numbers'),na.rm=TRUE)

## get mean drift correction slope for delta-O18
flag.iso_type='delta.O18'
mean.slope_delta.O18=mean(
    sapply(ix.drift_correction.standards,GetDriftCorrSlope,
           std_data.Last3,meta.info_std,flag.iso_type),na.rm=TRUE)

## get mean drift correction slope for delta-H2
flag.iso_type='delta.H2'
mean.slope_delta.H2=mean(
    sapply(ix.drift_correction.standards,GetDriftCorrSlope,
           std_data.Last3,meta.info_std,flag.iso_type),na.rm=TRUE)


#------------------------------------------------------------------------------------
# correct standard data for drift

## delta-O18

flag.iso_type='delta.O18'

params=list(flag.iso_type=flag.iso_type,
    mean.slope=mean.slope_delta.O18,
    mean.line.no_first.block=mean.line.no_first.block)

std_data.DriftCorr1=
    lapply(std_data,GetSubList,GetSubArr,DriftCorrection,params)

## delta-H2

flag.iso_type='delta.H2'

params=list(flag.iso_type=flag.iso_type,
    mean.slope=mean.slope_delta.H2,
    mean.line.no_first.block=mean.line.no_first.block)

std_data.DriftCorr2=
    lapply(std_data.DriftCorr1,GetSubList,GetSubArr,DriftCorrection,params)


#------------------------------------------------------------------------------------
# output

res=list()

res$std_data=std_data.DriftCorr2
res$mean.line.no_drift.std_first.block=mean.line.no_first.block
res$mean.slope_delta.O18=mean.slope_delta.O18
res$mean.slope_delta.H2=mean.slope_delta.H2

return(res)


### END OF FUNCTION ###
}


# **********************************************************
# SUB-ROUTINE TO CALCULATE A LINEAR DRIFT CORRECTION SLOPE *
# **********************************************************
#
###########################################################################
GetDriftCorrSlope <- function(ix.std,std_data,meta.info_std,flag.iso_type){
###########################################################################


## get name of final standard block
final.block=paste('block',meta.info_std$block_no,sep='')
    
## concatenate line numbers of first and final block
line.numbers=c(std_data[[ix.std]][['block1']][,'line.numbers'],
    std_data[[ix.std]][[final.block]][,'line.numbers'])
    
## concatenate isotope data of first and final block
iso.data=c(std_data[[ix.std]][['block1']][,flag.iso_type],
    std_data[[ix.std]][[final.block]][,flag.iso_type])

## return NA if one entire block only contains NA-values
if (all(is.na(std_data[[ix.std]][['block1']][,flag.iso_type])) ||
    all(is.na(std_data[[ix.std]][[final.block]][,flag.iso_type]))) return(NA)

## linear calibration of isotope data vs. line numbers (time)
lm.drift=lm(iso.data ~ line.numbers)

## get regression slope of linear drift correction
drift.slope=lm.drift$coeff[2]


return(drift.slope)


### END OF FUNCTION ###
}


#*********************************************
# FUNCION TO PERFORM LINEAR DRIFT CORRECTION *
#*********************************************
#
###############################################
DriftCorrection <- function(block_data,params){
###############################################

block_data[,params$flag.iso_type]=block_data[,params$flag.iso_type]-
    params$mean.slope*(
        block_data[,'line.numbers']-params$mean.line.no_first.block)

return(block_data)

### END OF FUNCTION ###
}

