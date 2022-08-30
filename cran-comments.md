Major release for tcpl v3 updating models
Maintainer is changing from Richard Judson to Jason Brown

## Test environments

* local Windows 10 install, R 4.1.2
* Rhub Fedora Linux, R-devel, clang, gfortran
* Rhub Ubuntu Linux 20.04.1 LTS, R-release, GCC
* Rhub Windows Server 2022, R-devel, 64 bit

## rhub CMD check results
* checking CRAN incoming feasibility ... [24s] NOTE
Maintainer: 'Jason Brown <brown.jason@epa.gov>'

New maintainer:
  Jason Brown <brown.jason@epa.gov>
Old maintainer(s):
  Richard S Judson <Judson.Richard@epa.gov>
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
* checking HTML version of manual ... NOTE
Skipping checking HTML validation: no command 'tidy' found
Skipping checking math rendering: package 'V8' unavailable

- Maintainer is changing from Richard to Jason
- Other notes appear to be related to testing environment


## local R CMD check results

0 ERRORs | 0 WARNINGs | 0 NOTES.

## Downstream dependencies

* There are 2 Downstream dependencies for this package.

## revdepcheck results

We checked 2 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages