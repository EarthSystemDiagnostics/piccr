##===================================================================================
## CALIBRATION.R:
## FUNCTIONS TO CALIBRATE THE STANDARD AND SAMPLE DATA
##
## author: Thomas Muench (tmuench@awi.de)
##===================================================================================


# ***************************************************************
# WRAPPER FUNCTION TO PERFORM LINEAR CALIBRATION WITH STANDARDS *
# ***************************************************************
#
#################################################################################
wrapper.StandardCalibration <- function(std_data,meta.info_std,flag.Calibration){
#################################################################################


#------------------------------------------------------------------------------------
# input from calling routine

## flag signalling the calibration scheme

if (flag.Calibration!=0 && flag.Calibration!=1 && flag.Calibration!=2)
    stop('Unknown value of flag.Calibration in function standard_calibration()')


#------------------------------------------------------------------------------------
# linear drift correction with mean slope if calibration.flag=1

if (flag.Calibration==1)
    {
        
        res.drift_corr=
            LinearDriftCorrection.standard_data(std_data,meta.info_std)

        std_data=res.drift_corr$std_data
        
    }


#------------------------------------------------------------------------------------
# standard means of last three injections

## calculate mean of the last three injection isotope values
## and of the file line numbers for each measurement block
std_data.MeanLast3=
    lapply(std_data,GetSubList,GetSubVec,GetMeanLastN,params=list(N=3))


#------------------------------------------------------------------------------------
# call actual standard calibration function for each isotope type

flag.iso_type='delta.O18'
res.calibration_delta.O18=StandardCalibration(
    std_data.MeanLast3,meta.info_std,flag.iso_type,flag.Calibration)

std_data.MeanLast3=res.calibration_delta.O18$std_data

flag.iso_type='delta.H2'
res.calibration_delta.H2=StandardCalibration(
    std_data.MeanLast3,meta.info_std,flag.iso_type,flag.Calibration)

std_data.MeanLast3=res.calibration_delta.H2$std_data


#------------------------------------------------------------------------------------
# return values

res=list()

## +++++++++++++++++++++++++++++
## drift correction coefficients
## +++++++++++++++++++++++++++++

if (flag.Calibration==1)
    {
        
        res$drift_corr$mean.line.no_drift.std_first.block=
            res.drift_corr$mean.line.no_drift.std_first.block
        res$drift_corr$mean.slope_delta.O18=
            res.drift_corr$mean.slope_delta.O18
        res$drift_corr$mean.slope_delta.H2=
            res.drift_corr$mean.slope_delta.H2

    }

res$calib.coeff_delta.O18=res.calibration_delta.O18$calib.coeff
res$calib.coeff_delta.H2=res.calibration_delta.H2$calib.coeff


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## root mean square deviation, std bias,
## standard deviation of residuals (first-block calibration)
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++

res$rmsd$delta.O18=res.calibration_delta.O18$rmsd
res$rmsd$delta.H2=res.calibration_delta.H2$rmsd

res$std_bias$delta.O18=res.calibration_delta.O18$std.bias_inter
res$std_bias$delta.H2=res.calibration_delta.H2$std.bias_inter

res$sd.residuals$delta.O18=res.calibration_delta.O18$sd.residuals_FirstBlockCalib
res$sd.residuals$delta.H2=res.calibration_delta.H2$sd.residuals_FirstBlockCalib


## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## calibrated standard values + mean of respective line numbers
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

res$std_data=std_data.MeanLast3


#------------------------------------------------------------------------------------

return(res)


### END OF FUNCTION ###
}


#******************************************************************************
# FUNCTION TO PERFORM LINEAR CALIBRATION WITH STANDARDS FOR SPECIFIC ISO TYPE *
#******************************************************************************
#
#################################################################
StandardCalibration <- function(std_data,meta.info_std,
                                 flag.iso_type,flag.Calibration){
#################################################################


#------------------------------------------------------------------------------------
# input from calling routine

## flag signalling the isotope type
if (flag.iso_type!='delta.O18' && flag.iso_type!='delta.H2')
    stop('Unknown type of water isotope in function standard_calibration()')

## get standard calibration scheme = index numbers of calibration std.s
ix.calibration.standards=which(meta.info_std$std.data_corr['calibration',]==1)

## literature values of standards for this isotope type
std_lit_values=meta.info_std$std_lit_values[flag.iso_type,]

## create vector of block names
block.names=
    paste(rep('block',meta.info_std$block_no),1:meta.info_std$block_no,sep='')


#------------------------------------------------------------------------------------
# convert standard means of measurement blocks from list to matrix structure

std_means_blocks=array()

for (ix.block in (1:meta.info_std$block_no)){

    std_means_blocks=rbind(std_means_blocks,
        sapply(c(1:meta.info_std$std_no),
               ExtractBlockVariable,std_data,ix.block,flag.iso_type))
}

## remove first line NA's
std_means_blocks=std_means_blocks[-1,]

## name columns and rows accordingly
colnames(std_means_blocks) <- meta.info_std$std_names
rownames(std_means_blocks) <- block.names


#------------------------------------------------------------------------------------
# linear calibration of standard data of first measurement block
# of the calibration standards against their literature values

## linear calibration of first means against literature values
lm.calib=
    lm(std_means_blocks['block1',ix.calibration.standards] ~
       std_lit_values[ix.calibration.standards])

sd.residuals_FirstBlockCalib=sd(lm.calib$residuals)

## create an array to store regression coefficients = calibration intercepts
## and slopes for the standard measurement blocks
calib.coeff_blocks=array(dim=c(meta.info_std$block_no,2))
colnames(calib.coeff_blocks) <- c('intercept','slope')
rownames(calib.coeff_blocks) <- block.names

## for this type of calibration, intercept and slope are kept constant
for (ix.block in (1:meta.info_std$block_no))
    {
        ## intercept of linear regression
        calib.coeff_blocks[ix.block,'intercept']=-lm.calib$coeff[1]/lm.calib$coeff[2]
        ## slope of linear regression
        calib.coeff_blocks[ix.block,'slope']=1/lm.calib$coeff[2]
    }


#====================================================================================
# DOUBLE THREE-POINT CALIBRATION

if (flag.Calibration==2){

#------------------------------------------------------------------------------------
# double calibration additionally with final block

## linear calibration of final means against literature values
final.block=block.names[meta.info_std$block_no]
lm.calib=
    lm(std_means_blocks[final.block,ix.calibration.standards] ~
       std_lit_values[ix.calibration.standards])

## get regression coefficients of linear calibration
calib.coeff_blocks[final.block,'intercept']=-lm.calib$coeff[1]/lm.calib$coeff[2]
calib.coeff_blocks[final.block,'slope']=1/lm.calib$coeff[2]


#------------------------------------------------------------------------------------
# get mean line numbers according to the two linear calibrations to obtain a
# linearly interpolated calibration slope and intercept

mean.line.no_blocks=vector()

for (ix.block in (1:meta.info_std$block_no))    
    mean.line.no_blocks=c(mean.line.no_blocks,
        mean(sapply(ix.calibration.standards,ExtractBlockVariable,
                    std_data,ix.block,'line.numbers'),na.rm=TRUE))

## name columns accordingly
names(mean.line.no_blocks) <- block.names


#------------------------------------------------------------------------------------
# linear interpolation of calibration slopes+intercepts to position of inter blocks

intercept.interpolation=lm(
    c(calib.coeff_blocks['block1','intercept'],
      calib.coeff_blocks[final.block,'intercept']) ~
    c(mean.line.no_blocks['block1'],mean.line.no_blocks[final.block]))

slope.interpolation=lm(
    c(calib.coeff_blocks['block1','slope'],
      calib.coeff_blocks[final.block,'slope']) ~
       c(mean.line.no_blocks['block1'],mean.line.no_blocks[final.block]))

for (ix.block in (2:(meta.info_std$block_no-1)))
    {
        
        calib.coeff_blocks[block.names[ix.block],'intercept']=
            intercept.interpolation$coeff[1]+
                intercept.interpolation$coeff[2]*mean.line.no_blocks[ix.block]

        calib.coeff_blocks[block.names[ix.block],'slope']=
            slope.interpolation$coeff[1]+
                slope.interpolation$coeff[2]*mean.line.no_blocks[ix.block]
        
    }

}

#====================================================================================


#------------------------------------------------------------------------------------
# calibrate standard measurement blocks with calibration function

calib.std_values=array(dim=c(meta.info_std$block_no,meta.info_std$std_no))
rownames(calib.std_values) <- block.names
colnames(calib.std_values) <- meta.info_std$std_names

for (ix.block in (1:meta.info_std$block_no))
    calib.std_values[ix.block,]=
        calib.coeff_blocks[block.names[ix.block],'intercept']+
        calib.coeff_blocks[block.names[ix.block],'slope']*
            std_means_blocks[block.names[ix.block],]


#------------------------------------------------------------------------------------
# check quality of calibration with those standards not used for calibration

## absolute difference between calibrated values of inter blocks
## and literature values
inter.blocks=block.names[2:(meta.info_std$block_no-1)]
std.bias_inter=
    calib.std_values[inter.blocks,]-
        matrix(std_lit_values,nrow=length(2:(meta.info_std$block_no-1)),
              ncol=meta.info_std$std_no,byrow=TRUE)

## remove NA's = standards not measured at inter blocks
notNA=apply(std.bias_inter,2,function(x){any(!is.na(x))})
std.bias_inter=array(std.bias_inter[,notNA],
    dim=c(length(inter.blocks),length(which(notNA))))

## name bias matrix
colnames(std.bias_inter) <- meta.info_std$std_names[which(notNA)]
rownames(std.bias_inter) <- inter.blocks

## root mean square deviation (rmsd)
rmsd=sqrt(mean(std.bias_inter^2,na.rm=TRUE))


#------------------------------------------------------------------------------------
# return values

res=list()

## ++++++++++++++++++++++++
## calibration coefficients
## ++++++++++++++++++++++++

if (flag.Calibration==0 || flag.Calibration==1)
    {
        calib.intercept=calib.coeff_blocks['block1','intercept']
        calib.slope=calib.coeff_blocks['block1','slope']
    }
else ##flag.Calibration==2
    {
        calib.intercept=
            c(intercept.interpolation$coeff[1],intercept.interpolation$coeff[2])
        calib.slope=
            c(slope.interpolation$coeff[1],slope.interpolation$coeff[2])
    }

res$calib.coeff$intercept=calib.intercept
res$calib.coeff$slope=calib.slope


## ++++++++++++++++++++++++++++++++++++++++++++++++++++
## std inter bias + root mean square deviation +
## standard deviation of residuals (first-block calib.)
## ++++++++++++++++++++++++++++++++++++++++++++++++++++

res$std.bias_inter=std.bias_inter
res$rmsd=rmsd
res$sd.residuals_FirstBlockCalib=sd.residuals_FirstBlockCalib


## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## calibrated standard values + mean of respective line numbers
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

for (ix.std in (1:meta.info_std$std_no))
    {
        for (ix.block in (1:meta.info_std$block_no))
            {
                this.block=block.names[ix.block]
                if (meta.info_std$std_scheme[this.block,ix.std]!=0)
                    std_data[[ix.std]][[this.block]][flag.iso_type]=
                        calib.std_values[this.block,ix.std]
            }
    }

res$std_data=std_data


#------------------------------------------------------------------------------------

return(res)


### END OF FUNCTION ###
}



#************************************************************
# FUNCTION TO APPLY LINEAR CALIBRATION MODEL TO SAMPLE DATA *
#************************************************************
#
#################################################################################
SampleCalibration <- function(sample_data,calibration.results,flag.Calibration){
#################################################################################


#------------------------------------------------------------------------------------
# input from calling routine - drift-correction and calibration coefficients;
# structure of input depends on calibration scheme -> variable calibration.flag

## flag signalling the calibration scheme

if (flag.Calibration!=0 && flag.Calibration!=1 && flag.Calibration!=2)
    stop('Unknown value of flag.Calibration in function standard_calibration()')

## drift correction and calibration coefficients

## extra drift correction only for flag.Calibration=1
if (flag.Calibration==1)
    {
    
        ## mean drift correction slope for delta-O18
        drift.corr.slope_delta.O18=
            calibration.results$drift_corr$mean.slope_delta.O18

        ## mean drift correction slope for delta-H2
        drift.corr.slope_delta.H2=
            calibration.results$drift_corr$mean.slope_delta.H2

        ## mean line number of first block drift correction standards
        mean.line.no_drift.std_first.block=
            calibration.results$drift_corr$mean.line.no_drift.std_first.block

    }

## --- variable type of calibration coefficients depends on calibration scheme: ---

## flag.Calibration=1: calibration intercept/slope is single value
## flag.Calibration=2: calibration intercept/slope are vectors where 1st entry
##                     is the intercept, 2nd entry the slope to linearly interpolate
##                     the calibration coefficients between beginning and end of file

## intercept for delta-O18
calib.intercept_delta.O18=calibration.results$calib.coeff_delta.O18$intercept

## slope for delta-O18
calib.slope_delta.O18=calibration.results$calib.coeff_delta.O18$slope

## intercept for delta-H2
calib.intercept_delta.H2=calibration.results$calib.coeff_delta.H2$intercept

## slope for delta-H2
calib.slope_delta.H2=calibration.results$calib.coeff_delta.H2$slope


#------------------------------------------------------------------------------------
# apply correction and calibration models to sample data

if (flag.Calibration==0)
    {
        ## === three-point calibration without drift correction === ##
        
        ## --- delta-O18 data --- ##
        
        sample_data[,'delta.O18']=
            calib.slope_delta.O18*sample_data[,'delta.O18']+calib.intercept_delta.O18

        ## --- delta-H2 data --- ##
        
        sample_data[,'delta.H2']=
            calib.slope_delta.H2*sample_data[,'delta.H2']+calib.intercept_delta.H2
    }        
else if (flag.Calibration==1)
    {
        ## === linear drift correction + three-point calibration === ##
        
        ## --- delta-O18 data --- ##

        ## drift corrected data
        drift.corr_delta.O18=
            sample_data[,'delta.O18']-drift.corr.slope_delta.O18*
                (sample_data[,'line.numbers']-mean.line.no_drift.std_first.block)
        ## calibration
        sample_data[,'delta.O18']=
            calib.slope_delta.O18*drift.corr_delta.O18+calib.intercept_delta.O18

        ## --- delta-H2 data --- ##

        ## drift corrected data
        drift.corr_delta.H2=
            sample_data[,'delta.H2']-drift.corr.slope_delta.H2*
                (sample_data[,'line.numbers']-mean.line.no_drift.std_first.block)
        ## calibration
        sample_data[,'delta.H2']=
            calib.slope_delta.H2*drift.corr_delta.H2+calib.intercept_delta.H2
        
    }
else
    {
        ## === double block three-point calibration with linear interpolation === ##

        ## --- delta-O18 data --- ##

        ## linear interpolation of calibration slopes and intercepts to line numbers
        calib.intercept_interpolated=calib.intercept_delta.O18[1]+
            calib.intercept_delta.O18[2]*sample_data[,'line.numbers']
        calib.slope_interpolated=calib.slope_delta.O18[1]+
            calib.slope_delta.O18[2]*sample_data[,'line.numbers']

        ## calibration
        sample_data[,'delta.O18']=
            calib.slope_interpolated*sample_data[,'delta.O18']+
                calib.intercept_interpolated

        ## --- delta-H2 data --- ##

        ## linear interpolation of calibration slopes and intercepts to line numbers
        calib.intercept_interpolated=calib.intercept_delta.H2[1]+
            calib.intercept_delta.H2[2]*sample_data[,'line.numbers']
        calib.slope_interpolated=calib.slope_delta.H2[1]+
            calib.slope_delta.H2[2]*sample_data[,'line.numbers']

        ## calibration
        sample_data[,'delta.H2']=
            calib.slope_interpolated*sample_data[,'delta.H2']+
                calib.intercept_interpolated

    }

#------------------------------------------------------------------------------------
# return drift-corrected and calibrated data

return(sample_data)


### END OF FUNCTION ###
}

