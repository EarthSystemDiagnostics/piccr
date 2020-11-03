# piccr 1.3.1.9000

* patch to comply with the new 'readr 1.4.0' function argument naming for
  `write_*()' functions (stops deprecation warnings).

# piccr 1.3.0.9000

* Major enhancements to the quality control output:
  * the estimated memory coefficients from the memory correction approach are
    now included in the quality control output, i.e. they are printed per default
    to the quality control output file ("run.info") and to to the terminal output
    upon interactive usage;
  * the calibration and drift correction parameters do no longer only include
    just the estimated slope and intercept but now also quality control
    information such as p-values, r-squared value and regression
    residuals. These information on the calibration and drift correction process
    are now printed per default to the quality control output file, and they can
    be printed to the terminal upon interactive usage when desired.
* Some code improvements.

# piccr 1.2.0.9000

* New version of the quality control output which is tailored to the new **piccr**
  output structure. It improves the appearance of the quality control output
  file and provides a new function to display the main quality control
  information when using **piccr** in interactive mode:
  * new function `printQualityControl()`, which can be used to print quality
    control information during **piccr** interactive usage;
  * new function `printRunInfo()` prints a string of the **piccr** run and version
    information;
  * function `outputSummaryFile()` now uses `printQualityControl()` and
    `printRunInfo()` to fill the contents of the output summary file;
  * new function `gatherQualityControlInfo()` summarises the relevant quality
    control information across a bunch of processed measurement files;
  * the functions `buildFirstSection()`, `buildSecondSection()` and
    `buildThirdSection()` are obsolete and have been removed.
* The function `calculateRMSD()` now offers the possibility of calculating the
  RMSD of one vector against zero.
* Update is erroneously labelled as 1.2.1.9000.

# piccr 1.1.1.9000

* Patch update to comply with new tibble (>=1.0.0) and dplyr (>=3.0.0) versions.

# piccr 1.1.0.9000

* Adds the **piccr** processing guide as a package vignette.

# piccr 1.0.0.9000

* Major update without backwards compatibility from bundle of scripts version to
  proper package.
* Complete rewriting of source code now using modern "tidy" programming style.
* **piccr** can now be run in interactive mode on the R command line with the main
  function `processFiles()`.
* **piccr** can be integrated with other packages using the interface via
  `processData()`.
* Program run parameters are now handled via a `yaml` configuration file.
  
# piccr 0.1.0.9000

* **piccr** bundle of scripts version.
* Does not work interactively but only on a Linux command line.
* Version tagging does not consistently use the .9000 development version suffix.
  
