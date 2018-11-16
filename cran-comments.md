This is a bugfix to correct the Debian warnings for 2.0.0 submission.  We believe the warnings were isolated to the vignette and have been updated to no longer write to the installation folder see: https://github.com/USEPA/CompTox-ToxCast-tcpl/commit/922ed9011f2c89dc5ee23bf25e43e662f3f448a0

## Test environments

* local Windows 10 install, R 3.5.1

## R CMD check results

0 ERRORs | 0 WARNINGs | 0 NOTES.

## Downstream dependencies

* There are 2 Downstream dependencies for this package.
* toxboot: error related to increased table size in new version of tcpl.
           author is aware of issue (https://github.com/ericwatt/toxboot/issues/13)
* toxplot: fit_curve_tcpl function fails due to additional columns in tcplFit.
			Author notified 9/10 and is aware of changes.

## Vignette changes

*Data_processing.rmd under vignettes was modified to update the tcplLite directory to be a temp directory rather than the file installation destination
* This release's documentation include three vignettes written in rmarkdown. The htmltable, rmarkdown, and prettydoc dependencies generate html outputs of the the vignettes with reasonable sizes to adhere with CRAN policies. To run the vignette, these packages need to be pre-installed, and were listed under 'suggests' type of dependencies. 
