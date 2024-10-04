* added package anchors to links NOTEd in current cran check results


## Test environments

* local Windows 10 install, R 4.2.2
* R Under development (unstable) (2023-10-04 r85267 ucrt)
* Rhub Windows Server 2022, R-devel, 64 bit
* Rhub Fedora Linux, R-devel, clang, gfortran
* Rhub Ubuntu Linux 20.04.1 LTS, R-release, GCC

## rhub CMD check results
- All notes appear to be related to testing (rhub) environment
Found the following (possibly) invalid URLs:
  URL: https://www.epa.gov/chemical-research/exploring-toxcast-data-downloadable-data
    From: inst/doc/Data_retrieval.html
          inst/doc/Introduction_Appendices.html
    Status: 403
    Message: Forbidden
  URL: https://www.epa.gov/chemical-research/toxicity-forecasting
    From: inst/doc/Introduction_Appendices.html
    Status: 403
    Message: Forbidden
* checking HTML version of manual ... [13s] NOTE
Skipping checking math rendering: package 'V8' unavailable
* checking for non-standard things in the check directory ... NOTE
Found the following files/directories:
  ''NULL''
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'

## win devel check results
1 NOTE - Found the following (possibly) invalid URLs...
All links appear to work correctly when hyperlinked from the vignette, similar note to the rhub testing

## local R CMD check results

0 ERRORs | 0 WARNINGs | 0 NOTES.

## Downstream dependencies

* There are 2 Downstream dependencies for this package.

## revdepcheck results

We checked 2 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

