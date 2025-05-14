# tcpl (development version)

# tcpl 3.3.0

## New features

* `tcplPlot()` gains a `compare` parameter, which replaces the `compare.val` 
parameter, for selecting a plot-data field by which samples are grouped into 
comparison plots. `tcplPlot()` also gains `group.fld`, `group.threshold`, and 
`hide_losing_models` parameters, providing more customizability to comparison
and individual concentration-response plots alike.
* `tcplLoadData()`, and by extension `tcplPlot()` and `tcplPlotLoadData()`, now 
support chemical queries via the `fld` parameter (#162).
* `tcplVarMat()` now produces a z-score matrix by default, customizable via the
`std_vars` parameter (#326).
* `tcplRun()` gains a `ready_only` parameter to only process endpoints annotated
with `export_ready = 1` (#263).

## Minor improvements and fixes

### Processing method changes

* Level 6 model directionality flag is improved (#323).
* New level 5 method ow_loec.coff hit-calls using the lowest observed effective 
concentration (#308).
* New level 5 method include_loec.coff stores the lowest observed effective 
concentration (#345).
* New single-concentration method level 2 pc40 (#309).
* New level 3 method resp.censormed.neg25 (#299).

### General processing changes

* Level 4 fields nmed_gtbl_neg (number of median responses greater than baseline 
in the negative direction) and nmed_gtbl_pos (same in the positive direction) 
are now correctly calculated (#339).
* Level 3 multi- and level 1 single-concentration processing now retains un-
rounded concentrations (#324).

### Other

* Vignette now uses flag descriptions in place of flag names to be more clear, 
includes an image detailing visual examples of flags, and contains examples
showcasing new `tcplPlot()` functionality, among many other small, general 
improvements.
* `tcplSubsetChid()` now preserves the continuous hitcall and the binary hitcall 
is stored as `actc` (#189).

# tcpl 3.2.1

* Updating Maintainer
* Change minimum R version to 4.1.0 to fix NOTES

# tcpl 3.2.0

* Added a `NEWS.md` file to track changes to the package.
* updated vignettes
* Updated plotting including standalone and comparisons
* Added bidirectional summary stats and updated functions to work with newest 
version of invitrodb
* Added level 6 flags
* Implemented API functionality
* Removed tcplLite

# tcpl 3.1.0

* updated vignettes for tcplfit2 fitting
* bidirectional fitting by default
* this is the version used to create invitrodb v4.1

# tcpl 3.0.1

* fixes error from ggplot2

* minor bug fixes

# tcpl 3.0.0

* add functionality for additional model fitting with tcplfit2

* updated vignettes for new fitting

* created updated plotting function

# tcpl 2.1.0

* updated for release with toxcast invitrodb data

* added option to allow change in single conc bmad calculation

# tcpl 2.0.3

* Created tcplPlot a generic plotting function with call similar to tcplLoadData
* added option for concentration units to be different during plotting.
* fixed tcpllite bug for schema change
* filtered gnls from tcplCytopt
* updated to Rmariadb
* various bugfixes

# tcpl 2.0.2

* Updated tcplLoadChem to return dsstox substance ids
* Moved data.table to imports instead of depends
* Added new level 6 method for flagging viability gnls winners
* Added single concentration option for tcplSubsetChid
* Updated tcplCytoPt methodology
* Added ability to determine lowest effective concentration (loec) as level 5 method
* Added new AEID plot functionality to visualize 2 aeids on same plot through tcplMakeAeidPlts

# tcpl 2.0.1

* Updated vignettes to use tempdir and thus not write to install folder.


# tcpl 2.0


Major change for tcpl v2.0:

* Incorporation of tcplLite into the package, a functionality that allows the user to work from local files formatted like invitrodb tables, rather than a MySQL database.

* Using tcplLite is ideal for handling smaller datasets. 

* The SQLite driver was removed from the package.
* tcplConf() default is now for tcplLite, which uses .csv files rather than tables in a MySQL database. 
* The MySQL driver is available and recommended for interacting with the entire dataset as a database. 
* The package vignettes run using tcplLite.
Additional updates for tcpl v2.0
* tcplLvlCount: function added to count the samples at each level of processing for each aeid.
* Addition of mc4 methods: the baseline median absolute deviation can now be calculated based on the lowest two concentrations or on the neutral control wells. An mc4 method must now be assigned for analysis.
* tcplGetAeid: function added to find assays based on matching a character string within any assay endpoint name.
* tcplMakeAeidMultiPlts: 3x2 grid of plots on each page of a PDF, for a single aeid.
* tcplMakeChidMultiPlts: 3x2 grid of plots on each page of a PDF, for a single chemical id (chid).
* Vignettes are now posted as html from RMarkdown files for Introduction and appendices, data retrieval, and data processing.
* Various bug fixes and additions to the methods tables.

# tcpl 1.4.0


Changes from v1.2.2
  
* .plotFit (not exported) was updated to work with the new behavior of `nchar`
  for R>3.3 (PR#10/#26)
  
* tcplPlotPlate now works with single-conc data. Previously it only worked with
  the multi-conc data. (PR#14)
  
* Deleted out the 'nddr' level 6 methods. They were specific to one dataset and
  are no longer used. (PR#16)
  
* Removed all instances of "with = FALSE" in data.table calls. (PR#25)

* The package no longer writes to the installation directory. The vignette is 
  now built in TMPDIR and the location of the configuration file must be 
  specified by the user. This is to comply CRAN policies. The benefit to users
  that share an installation, is they can now each have their own config file.
  (PR#28)
  
* The specific method imports were removed for the RSQLite and RMySQL packages.
  This change reflects the new behavior of the RSQLite package. The RMySQL
  imports were removed to simplify the package, as they were found unnecessary.
  (PR#29)




# tcpl 1.2.3


Changes from v1.2.2
  
* .plotFit (not exported) was updated to work with the new behavior of `nchar`
  for R>3.3 (PR#10/#26)
  
* tcplPlotPlate now works with single-conc data. Previously it only worked with
  the multi-conc data. (PR#14)
  
* Deleted out the 'nddr' level 6 methods. They were specific to one dataset and
  are no longer used. (PR#16)
  
* Removed all instances of "with = FALSE" in data.table calls. (PR#25)

* The package no longer writes to the installation directory. The vignette is 
  now built in TMPDIR and the location of the configuration file must be 
  specified by the user. This is to comply CRAN policies. The benefit to users
  that share an installation, is they can now each have their own config file.
  (PR#28)
  
* The specific method imports were removed for the RSQLite and RMySQL packages.
  This change reflects the new behavior of the RSQLite package. The RMySQL
  imports were removed to simplify the package, as they were found unnecessary.
  (PR#29)

# tcpl 1.2.2


Changes from v1.2

* A try-statement was added to the underlying function that draws the plot for 
  tcplPlotHeat, so that the function does not return a figure margin error when
  running the checks for the package. 


# tcpl 1.2 


Changes from beta versions:

* The tcpl4 function now checks the new 'fit_all' field in the 
  assay_component_endpoint table, and will attempt to fit every curve when
  'fit_all' is 1.
* The TCPL_CHEM and TCPL_INT settings have been deprecated and are no longer 
  needed. To facilitate easier use and understanding, chemical tables included 
  in the tcpl database are restructured. 
* The TCPL_LOG setting has been deprecated.
* tcplUpdate is renamed to tcplSendQuery.
* Single concentration screening is now implemented.
* Added tcplRegister and tcplUpdate to make populating the database with new
  data easier.
* Changed the underlying database structure. The old database will not work
  with beta versions of the package.
* tcplPlotL4ID is now tcplPlotM4ID to match the new database structure.
* The tcpl1, tcpl2, etc. functions have been renamed and are no longer 
  exported. All processing is intended to occur through the tcplRun function. 
* tcplRunPipe is now tcplRun, and no longer defaults to writing a log file. 
  A log file can still be written using the 'outfile' setting, as previously. 
* Level 5 multiple-concentration processing has changed -- there is no longer
  a minimum cutoff. All cutoff values must be specified with the new level 5
  methods. 
* Level 6 multiple-concentration processing has changed -- some of the flag 
  methodologies were updated.
* tcplSetOpts renamed to tcplConf.
* tcplSetOpts (now called tcplConf) can now be given only one or a subset of 
  settings to change.
* tcplListOpts renamed to tcplConfList.
* tcplConfLoad, tcplConfSave, tcplConfDefault, and tcplConfReset added to make 
  working with the package settings easier for the user.
* Due a change in the underlying code for `rapply` the package now must use
  R version 3.2.0 or higher.
* Added processing methods. 
* tcplWriteLvl0 now excludes loading data for acsn values that are not 
  registered, but will load data for acsn values that are registered without
  returning an error.
* tcpl now imports RMySQL.
* tcplACVal and tcplACXX renamed tcplHillConc and tcplHillACXX, respectively. 
* tcplHillVal added.
* Bug in tcplPlotPlate that caused incorrect coloring when the input data.table
  contained multiple assays is fixed.
* 'fname' paramter is added to tcplMakeAeidPlts, and the default filename is 
  changed.
* 'quant' parameter is added to tcplPlotPlate.
* tcplLoadAsid deprecated, and is no longer exported.
* tcplLoadUnit deprecated, and is no longer exported.
* The call to 'browser' in tcplPlotFits is changed to skip the other lines 
  in the loop to make scanning through plots a little easier.
* Extensive updates to documentation and the vignette, now called 
  "tcpl_Overview."
* tcplAssignMthd, tcplLoadMthd, tcplListMthd, and tcplClearMthd functions are 
  renamed as tcplMthd- (e.g. tcplMthdLoad instead of tcplLoadMthd).
* tcplMthdClear no longer returns TRUE, matching the behavior of tcplMthdAssign.
* Added tcplCytoPt function to calculate the cytotoxicity points for chemical
  (this functionality used to be delivered by tcplVarMat, but is now separate).
* 'argx' and 'srgx' parameters are removed from tcplPrepOtpt.
* 'srgx' paramter is removed from tcplMakeAeidPlts.
* tcplLoadAsidInfo & tcplLoadAeidInfo deprecated, and are no longer exported. 
* tcplPrepOtpt longer contains the 'clib' parameter.

