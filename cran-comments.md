This is a Major update for tcpl:
* Incorporation of tcplLite into the package,
  a functionality that allows the user to work from local files formatted like invitrodb tables,
  rather than a MySQL database.

This release makes Richard Judson the new maintainer of tcpl.
Dayne Filer will need to approve the change <dayne.filer@gmail.com> .

## Test environments

* local Windows 10 install, R 3.5.1
* local RHEL 7.5 install, R 3.5.0

## R CMD check results

0 ERRORs | 0 WARNINGs | 0 NOTES.

## Downstream dependencies

* There are 2 Downstream dependencies for this package.
* toxboot: error related to increased table size in new version of tcpl.
           author is aware of issue (https://github.com/ericwatt/toxboot/issues/13)
* toxplot: fit_curve_tcpl function fails due to additional columns in tcplFit.
			Author notified 9/10 and is aware of changes.
