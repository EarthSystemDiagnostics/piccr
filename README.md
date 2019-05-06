# piccr

## \*\* README \*\*

Note that this branch is *for archiving only*! It contains the legacy code of **piccr** from [BitBucket](https://bitbucket.org/ecus/piccr/) when **piccr** was still just a bundle of R scripts and functions. Now **piccr** is actively developed to be transformed into an R package; please see the other branches here on GitHub.

## Introduction

**piccr** is a bundle of R scripts that provide an easy-to-use and quick way to correct and calibrate raw isotope data output from Picarro Inc. cavity ring down spectroscopy measurement devices. The bundle is capable of automatically processing at once a given amount of data files that follow the same measurement sequence. Correction of the data includes a correction for memory effects in subsequent injections of the same water sample and a linear drift correction (van Geldern and Barth, 2012). Calibration is performed based on the measurement of in-house isotope standards along with the samples.

For questions regarding the script and its use please contact Thomas Münch <<thomas.muench@awi.de>> at the Alfred Wegener Institute Helmholtz Centre for Polar and Marine Research in Potsdam, Germany.

**Please note** that **piccr** is currently supported for use under Mac OS X and Linux only; Windows computers are not supported. In due time, **piccr** will be converted into a full **R** package to ease its use, which then will also support Windows machines; so please stay tuned for updates.

## Installation

Download the repository by navigating to the `Downloads` pane of **BitBucket** and click on `Download repository`. This will download a zip file containing the package. Save the file and extract the zip archive into a directory of your choice. This directory will contain all necessary scripts including the main routine `piccr-main.R` which is already executable. If, for some reason, this is not the case, run `chmod +x piccr-main.R` from the command line within the repository directory.

## Use

### Overview

The general idea of usage is to collect all raw Picarro output files that have been measured according to the same sequence within one directory. Put a copy of the jobfile `INPUT.txt`, which is distributed together with **piccr**, into that directory and update the file according to the given measurement sequence (see [input information](#markdown-header-input-information) below).

Run **piccr** on that directory by issuing the following command on the command line,

```
<PATH>/piccr-main.R <DIR>
```

where `<PATH>` is the path to the directory where the **piccr** script files are located, and `<DIR>` is the directory of the raw Picarro data files. This will correct and calibrate all raw data according to the settings in `INPUT.txt`. The processed isotope data will be stored in a subdirectory called `calibrated` located within the given `<DIR>`, along with an information file `OutputAccuracy.txt` reporting on the achieved accuracy and precision of the run. Additionally, the processed data  will be saved into an **R** data file `<DIR>.calibrated.rdat`.

### Input information

All necessary information for a programme run is specified in the jobfile `INPUT.txt`. This file has to be placed within the directory of the raw Picarro data in order to be read in by the programme. The necessary structure consists of two parts:

**META INPUT**

Parameter  | Key     |  Explanation 
:--------- | :-----: | :-----
FILE_ID    | *.csv   | file extension of Picarro files
COLUMNSEP  | ,       | column separator of Picarro files
DECSEP     |    .    | decimal separator of Picarro files
NAME_StdScheme |    `<name>` | name of applied standard scheme (optional)
STD_NO       |    `<val>`    | total number of used standards
BLOCK_NO   |    `<val>`    | number of *standard blocks*
CALIBRATION_FLAG| `<key>` | type of calibration
MEMORYCORR_FLAG    | `<key>` |signal memory correction
OUTPUT_FLAG    | `<key>` |output format

**META STANDARDS**

NAME | O18 | H2 | #INJ_block1 | MEMORYCORR | DRIFTCORR | CALIBRATION
:--- | :---: | :---: | :---: | :---: | :---: | :---:
`<name>` | `<val>` | `<val>` | `<no>` | `<key>` | `<key>` | `<key>`

#### Notes on *meta input*:

* The first three parameters are the default parameters for standard Picarro Inc. files. Note however that *any* file with an `*.csv` extension that is found in the given data  folder `<DIR>` will be read in by the programme; thus, `<DIR>` should only contain Picarro files.
* The name of the applied standard scheme is optional for later ease of reference.
* The number of *standard blocks* refers to the number of blocks of in-house standard measurements distributed over the measurement sequence; typically `val = 3` for an *initial*, an *intermediate* and a *final block*. In principle, an *initial block* would be sufficient when neglecting the independent quality control at a later point and the check against drift, but is not recommended.
* Possible types of calibration are:
  1. `key = 0`: calibration using *initial block* standards;
  2. `key = 1`: calibration as in 1. + linear drift correction against drift of calibration intercept;
  3. `key = 2`: calibration as in 1. + linear drift correction against drift of calibration intercept and calibration slope by performing second calibration using *final block* standards.
* Memory correction: `key = 0`: apply no memory correction; `key = 1`: apply memory correction.
* Output format: `key = 1`: only output sample data; `key = 2`: output sample and standard data following original measurement sequence.

#### Notes on *meta standards*:
* For each used standard, one row is specified.
* the standard name must reflect the name used in the Picarro files.
* O18 and H2 `<val>` specify the true oxygen and hydrogen isotope ratios of the standard, respectively.
* `<no>`specifies the number of repeated injections used for the standard in the given *block*, for each block one column is used.
* `<key>` assumes values of 0 or 1 to signal if the standard shall be used for the respective correction or calibration procedure (1 = yes, 0 = no).

For a complete example, a typical `INPUT.txt`is distributed along with the **piccr** script bundle.

### Details

Calibration of raw measurement values is based on a linear regression of measured to known values of in-house reference standards. A typical measurement sequence distributes two to three reference standards for calibration at the beginning of each measurement run (the *initial standard block*).

Both the memory correction and the drift correction scheme follow the procedures presented in van Geldern and Barth (2012).

The memory correction relies on the measurement of several isotope standards within the *initial standard block*. These typically should be injected 12 times (2 times 6 injections from different vials), and should differ considerably in expected isotopic composition, in order to reliably calculate memory coefficients for each injection. Determination of the memory coefficients then allows the reduction of necessary repeated injections of the remaining measurements to obtain a stable measurement value by correcting for the memory effect. **piccr** is built to handle three repeated injections of the remaining measurements once the memory correction has been determined. Compared to the standard procedure of using six repeated injections without memory correction, as recommended by Picarro Inc., this nearly halves the total measurement time.

Drift correction can be performed based on two different approaches:

1. Linear regression of calibrated isotope values of standards measured both within the *initial standard block* and within the *final standard block* at the end of the measurement run. This correction can be seen as the correction for a drift in calibration intercept.

2. Alternatively, a drift correction can be applied which corrects both for a potential drift in calibration intercept as well as calibration slope. This is based on a second calibration performed with standards measured in the *final standard block*, and linear regression of calibration intercept and slope between *initial* and *final block*.

For all procedures (calibration and corrections), the jobfile `INPUT.txt` is used to indicate which standards are intended to be used for which purpose(s).

Finally, to obtain an independent estimate of the overall precision and accuracy of the measurement, one or more standards are assumed to be measured in an *intermediate block*, located roughly in the middle of the measurement sequence, which have not been used for the correction and calibration procedures.

## Literature cited

van Geldern, R. and Barth, J. A.: Optimization of instrument setup and post-run corrections for oxygen and hydrogen stable isotope measurements of water by isotope ratio infrared spectroscopy (IRIS), Limnol. Oceanogr. Methods, 10, 1024–1036, doi: [10.4319/lom.2012.10.1024](https://doi.org/10.4319/lom.2012.10.1024), 2012.
