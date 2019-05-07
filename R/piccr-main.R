#!/usr/bin/env Rscript

##===================================================================================
## MAIN FUNCTION OF piccr
##
## author: Thomas Muench (tmuench@awi.de)
## last update: December 2017
##===================================================================================

cat('\n=== Starting piccr ===\n')


#====================================================================================
# SOURCE SUB ROUTINES

cat('\nSourcing piccr files...\n')

## path of folder containing piccr
package_folder=as.character(Sys.getenv()['PWD'])

## all R files in piccr folder
pck.files=list.files(package_folder,pattern='[.][R]$')
## exclude main routine (does not need to be sourced)
pck.files=pck.files[-grep('main',pck.files)]
## source files
for (f in pck.files){
    cat(f,':')
    source(file.path(package_folder,f))
    cat(' ok.\n')
}

cat('\n')


#====================================================================================
# INPUT

cat('Reading meta input...')

## get command line arguments
args=commandArgs(TRUE)
## first command line argument is working folder where Picarro isotope data
## files are located together with INPUT.txt specifying necessary input info
working_folder=args[1]

## error handling
if (is.na(working_folder))
    stop('\nSpecify folder containing the Picarro data as command-line argument.\n')

## read meta input
meta.info_folder=get.input_folder(working_folder)

cat('done.\n')


#====================================================================================
# READ RAW DATA FROM FILES

cat('Reading data...')

## call reading routine
isotope_data=wrapper.ReadDataFiles(meta.info_folder)

## store file data and meta information as list structure
folder.iso_data=list(isotope_data=isotope_data,
    meta.info_folder=meta.info_folder)

cat('done.\n')


#====================================================================================
# CORRECT AND CALIBRATE RAW DATA

cat('Calibrating data...')

calibration.results=CalibratePicarroData(folder.iso_data,
    meta.info_folder$flag.MemCorr,meta.info_folder$flag.Calibration)

cat('done.\n')


#====================================================================================
# OUTPUT OF CALIBRATED DATA AND ACCURACY ESTIMATES

cat('Writing output...')

WriteOutput(calibration.results,meta.info_folder)

cat('done.\n')


#====================================================================================
# END

cat('\nProgramme run successful.\n\n')
q(save="no",status=0)


#====================================================================================
