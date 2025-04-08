## Changes from last version
* Updating Maintainer
* Change minimum R version to 4.1.0 to fix NOTES

## Test environments

* local Windows 10 install, R 4.3.3
* R Under development (unstable) (2025-04-04 r88118 ucrt)

## winbuilder check results
* The only note is related to a change in maintainer
Installation time in seconds: 21
Check time in seconds: 299
Status: 1 NOTE
R version 4.5.0 RC (2025-04-04 r88118 ucrt)


    
## local R CMD check results
2 NOTES:
* We are switching maintainers so this first one is expected.
* We believe the unable to verify current time is a false positive unrelated to package.


── R CMD check results  tcpl 3.2.1 ────
Duration: 5m 15.3s

❯ checking CRAN incoming feasibility ... [153s] NOTE
  Maintainer: 'Madison Feshuk <feshuk.madison@epa.gov>'
  
  New maintainer:
    Madison Feshuk <feshuk.madison@epa.gov>
  Old maintainer(s):
    Jason Brown <brown.jason@epa.gov>

❯ checking for future file timestamps ... NOTE
  unable to verify current time

0 errors ✔ | 0 warnings ✔ | 2 notes ✖

## Downstream dependencies

* There are 3 Downstream dependencies for this package.

## revdepcheck results

We checked 3 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages



