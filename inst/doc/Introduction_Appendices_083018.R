## ----eval = FALSE, echo = FALSE, message = FALSE, warning = FALSE--------
#  
#  library(htmlTable)

## ----eval=TRUE, message=FALSE--------------------------------------------
library(data.table)
library(tcpl)
## Store the path for the tcpl directory for loading data
pkg_dir <- system.file(package = "tcpl")


## ----eval = FALSE--------------------------------------------------------
#  tcpl (v1.3) loaded with the following settings:
#    TCPL_DB:    C:/Users/user/R-3.4.4/library/tcpl/csv
#    TCPL_USER:  NA
#    TCPL_HOST:  NA
#    TCPL_DRVR:  tcplLite
#  Default settings stored in TCPL.conf. See ?tcplConf for more information.

## ----eval = FALSE--------------------------------------------------------
#  tcplConf(drvr = "MySQL",
#           user = "username",
#           pass = "password",
#           host = "localhost",
#           db   = "invitrodb")
#  

## ----eval = FALSE--------------------------------------------------------
#  tcplConf(drvr = "tcplLite", db = system.file("csv", package = "tcpl"), user = "", pass = "", host = "")

## ----warning = FALSE, echo = FALSE---------------------------------------
Field <- c("s0id ", "acid", "spid", "cpid", "apid", "rowi", "coli", "wllt", "wllq", "conc", "rval", "srcf")
Description <- c("Level 0 ID",
                 "Assay component ID",
                 "Sample ID",
                 "Chemical plate ID",
                 "Assay plate ID",
                 "Assay plate row index",
                 "Assay plate column index",
                 "Well type&dagger;",
                 "1 if the well quality was good, else 0&ddagger;",
                 "Concentration is micromolar",
                 "Raw assay component value/readout from vendor",
                 "Filename of the source file containing the data"
                 )

output <- 
  data.frame(Field, Description)

library(htmlTable)
htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        caption="Table 5: Fields in sc0 table.",
        tfoot="&dagger;Information about the different well types is available in Appendix B.")


## ----warning = FALSE, echo = FALSE---------------------------------------
Field <- c("s1id ", "s0id", "acid", "aeid", "logc", "bval", "pval", "resp")
Description <- c("Level 1 ID",
                 "Level 0 ID",
                 "Assay component ID",
                 "Assay component endpoint ID",
                 "Log base 10 concentration",
                 "Baseline value",
                 "Positive control value",
                 "Normalized response value"
              
                 )

output <- 
  data.frame(Field, Description)

library(htmlTable)
htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        caption="Table 6: Fields in sc1 table."
)


## ----warning = FALSE, echo = FALSE---------------------------------------
Field <- c("aeid ", "s0id", "s1id", "s2id")
Description <- c("Assay component endpoint ID",
                 "Level 0 ID",
                 "Level 1 ID",
                 "Level 2 ID"
              
                 )

output <- 
  data.frame(Field, Description)

library(htmlTable)
htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        caption="Table 7: Fields in sc2_agg table."
)


## ----warning = FALSE, echo = FALSE---------------------------------------
Field <- c("s2id ", "aeid", "spid", "bmad", "max_med", "hitc", "coff", "tmpi")
Description <- c("Level 2 ID",
                 "Assay component endpoint ID",
                 "Sample ID",
                 "Baseline median absolute deviation",
                 "Maximum median response value",
                 "Hit-/activity-call, 1 if active, 0 if inactive",
                 "Efficacy cutoff value",
                 "Ignore, temporary index used for uploading purposes"
                 )

output <- 
  data.frame(Field, Description)

library(htmlTable)
htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        caption="Table 8: Fields in sc2 table."
)


## ----warning = FALSE, echo = FALSE---------------------------------------
Field <- c("m1id", "m0id", "acid", "cndx", "repi")
Description <- c("Level 1 ID",
                 "Level 0 ID",
                 "Assay component ID",
                 "Concentration index",
                 "Replicate index"
                 )

output <- 
  data.frame(Field, Description)

library(htmlTable)
htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        caption="Table 9: Fields in mc1 table."
)


## ----warning = FALSE, echo = FALSE---------------------------------------
Field <- c("m2id", "m0id", "acid", "m1id", "cval")
Description <- c("Level 2 ID",
                 "Level 0 ID",
                 "Assay component ID",
                 "Level 1 ID",
                 "Corrected value"
                 )

output <- 
  data.frame(Field, Description)

library(htmlTable)
htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        caption="Table 10: Fields in mc2 table."
)


## ----warning = FALSE, echo = FALSE---------------------------------------
Field <- c("m3id", "aeid", "m0id", "acid", "m1id", "m2id", "bval", "pval", "logc", "resp")
Description <- c("Level 3 ID",
                 "Assay endpoint ID",
                 "Level 0 ID",
                 "Assay component ID",
                 "Level 1 ID",
                 "Level 2 ID",
                 "Baseline value",
                 "Positive control value",
                 "Log base 10 concentration",
                 "Normalized response value"
                 )

output <- 
  data.frame(Field, Description)

library(htmlTable)
htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        caption="Table 11: Fields in mc3 table."
)


## ----warning = FALSE, echo = FALSE---------------------------------------
Field <- c("aeid", "m0id", "m1id", "m2id", "m3id", "m4id")
Description <- c(
   "Assay endpoint ID","Level 0 ID",
                 "Level 1 ID",
                 "Level 2 ID",
                 "Level 3 ID",
                 "Level 4 ID"
                 
                 )

output <- 
  data.frame(Field, Description)

library(htmlTable)
htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        caption="Table 12: Fields in mc4_agg table."
)


## ----warning = FALSE, echo = FALSE---------------------------------------
Field <- c("m4id", "aeid", "spid", "bmad", "resp_max", "resp_min", "max_mean", "max_mean_conc", "max_med", "max_med_conc", "logc_max", "logc_min", "cnst", "hill", "hcov", "gnls", "gcov", "cnst_er", "cnst_aic", "cnst_rmse", "cnst_prob", "hill_tp", "hill_tp_sd", "hill_ga", "hill_ga_sd")

Description <- c("Level 4 ID",
                 "Assay endpoint ID",
                 "Sample ID",
                 "Baseline median absolute deviation",
                 "Maximum response value",
                 "Minimum response value",
                 "Maximum mean response value",
                 "Log concentration at *max_mean*",
                 "Maximum median response value",
                 "Log concentration at *max_med*",
                 "Maximum log concentration tested",
                 "Minimum log concentration tested",
                 "1 if the constant model converged, 0 if it failed to converge, N/A if series had less than four concentrations",
                  "1 if the Hill model converged, 0 if it failed to converge, N/A if series had less than four concentrations or if *max_med* < *3bmad*",
                "1 if the Hill model Hessian matrix could be inverted, else 0",
      "1 if the gain-loss model converged, 0 if it failed to converge, N/A
if series had less than four concentrations or if *max_med* < *3bmad*",
                "1 if the gain-loss model Hessian matrix could be inverted, else 0",
                "Scale term for the constant model",
                "AIC for the constant model",
                "RMSE for the constant model",
                "Probability the constant model is the true model",
                "Top asymptote for the Hill model",
                "Standard deviation for *hill_tp*",
                "AC~50~ for the Hill model",
                 "Standard deviation for *hill_ga* "
                 )

output <- 
  data.frame(Field, Description)

library(htmlTable)
htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        caption="Table 13: Fields in mc4 table (Part 1)."
)


## ----warning = FALSE, echo = FALSE---------------------------------------
Field <- c("hill_gw", "hill_gw_sd", "hill_er", "hill_er_sd", "hill_aic", "hill_rmse", "hill_prob", "gnls_tp", "gnls_tp_sd", "gnls_ga", "gnls_ga_sd", "gnls_gw", "gnls_gw_sd", "gnls_la", "gnls_la_sd", "gnls_lw", "gnls_lw_sd", "gnls_er", "gnls_er_sd", "gnls_aic", "gnls_rmse", "gnls_prob", "nconc", "npts", "nrep", "nmed_gtbl", "tmpi")

Description <- c("Hill coefficient",
                 "Standard deviation for *hill_gw*",
                 "Scale term for the Hill model",
                 "Standard deviation for *hill_er*",
                 "AIC for the Hill model",
                 "RMSE for the Hill model",
                 "Probability the Hill model is the true model",
                 "Top asymptote for the gain-loss model",
                 "Standard deviation for *gnls_tp*",
                 "AC~50~ in the gain direction for the gain-loss model",
                 "Standard deviation for *gnls_ga*",
                 "Hill coefficient in the gain direction",
                 "Standard deviation for *gnls_gw* ",
                 "AC~50~ in the loss direction for the gain-loss model",
                 "Standard deviation for *gnls_la*",
                "Hill coefficient in the loss direction",
                "Standard deviation for the *gnls_lw*",
                 "Scale term for the gain-loss model",
                 "Standard deviation for *gnls_er*",
                 "AIC for the gain-loss model",
                 "RMSE for the gain-loss model",
                "Probability the gain-loss model is the true model",
                "Number of concentrations tested ",
                "Number of points in the concentration series",
                "Number of replicates in the concentration series",
                "Number of median values greater than *3bmad*",
                "Ignore, temporary index used for uploading purposes"
                 )

output <- 
  data.frame(Field, Description)

library(htmlTable)
htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        caption="Table 14: Fields in mc4 table (Part 2)."
)


## ----warning = FALSE, echo = FALSE---------------------------------------
Field <- c("m5id", "m4id", "aeid", "modl", "hitc", "fitc", "coff", "actp", "modl_er", "modl_tp", "modl_ga", "modl_gw", "modl_la", "modl_lw", "modl_prob", "modl_rmse", "modl_acc", "modl_acb", "modl_ac10")

Description <- c("Level 5 ID",
                 "Level 4 ID",
                 "Assay endpoint ID",
                 "Winning model: \"cnst\", \"hill\", or \"gnls\"",
                 "Hit-/activity-call, 1 if active, 0 if inactive, -1 if cannot determine",
                 "Fit category",
                 "Effcacy cutoff value",

                "Activity probability (1 - *const_prob*)",
                "Scale term for the winning model",
                "Top asymptote for the winning model",
                "Gain AC~50~ for the winning model",
              "Gain Hill coefficient for the winning model",
              "Loss AC~50~ for the winning model",
       "Loss Hill coefficient for the winning model",
       "Probability for the winning model",
       "RMSE for the winning model",
       "Activity concentration at cutoff for the winning model",
       "Activity concentration at baseline for the winning model",
       "AC10 for the winning model"
                 )

output <- 
  data.frame(Field, Description)

library(htmlTable)
htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        caption="Table 15: Fields in mc5 table."
)


## ----warning = FALSE, echo = FALSE---------------------------------------
Field <- c("m6id", "m5id", "m4id", "aeid", "m6_mthd_id", "flag", "fval", "fval_unit")

Description <- c("Level 6 ID",
                 "Level 5 ID",
                 "Level 4 ID",
                 "Assay endpoint ID",
                 "Level 6 method ID",
                 "Text output for the level 6 method",
                 "Value from the flag method, if applicable",
                 "Units for *fval* , if applicable" )

output <- 
  data.frame(Field, Description)

library(htmlTable)
htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        caption="Table 16: Fields in mc6 table."
)


## ----warning = FALSE, echo = FALSE---------------------------------------
Field <- c("m4id", "Aeid", "Aenm", "Asid", "Acid", "Hit_pct", "Total_hitc", "Modl_ga_min", "Modl_ga_max", "Modl_ga_med", "Modl_gw_med", "Modl_ga_delta", "Cnst_pct", "Hill_pct", "Gnls_pct")

Description <- c("Level 4 ID",
                 "Assay endpoint ID",
                 "Assay endpoint name",
                 "Assay source ID",
                 "Assay component ID",
                 "Total percent of hit calls made after 1000 bootstraps",
                 "Total number of hit calls made after 1000 bootstraps",
                 "Low bound of the 95% confidence interval for the AC~50~",
                 "Upper bound of the 95% confidence interval for the AC~50~",
                 "Median AC~50~ after 1000 bootstraps",
                 "Median gain Hill coefficient for 1000 bootstraps",
                 "AC~50~ confidence interval width in log units",
                 "Percent of 1000 bootstraps that the constant model was selected as the winning model", 
                 "Percent of 1000 bootstraps that the Hill model was selected as the winning model",
                 "Percent of 1000 bootstraps that the gain-loss was selected as the winning model")

output <- 
  data.frame(Field, Description)

library(htmlTable)
htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        caption="Table 17: Fields in mc7 table."
)


## ----warning = FALSE, echo = FALSE---------------------------------------
Field <- c("assay", "assay_component", "assay_component_endpoint", "assay_component_map", "assay_reagent**", "assay_reference**", "assay_source", "chemical", "chemical_library", "citations**", "gene", "intended target**", "mc5_fit_categories", "organism**", "sample", "technological_target**")

Description <- c("Assay-level annotation",
                 "Assay component-level annotation",
                 "Assay endpoint-level annotation",
                 "Assay component source names and their corresponding assay component ids",
                 "Assay reagent information",
                 "Map of citations to assay",
                 "Assay source-level annotation",
                 "List of chemicals and associated identifiers",
                 "Map of chemicals to different chemical libraries",
                 "List of citations",
                 "Gene** identifers and descriptions",
                 "Intended assay target at the assay endpoint level",
                 "The level 5 fit categories",
                "Organism identifiers and descriptions",
                "Sample ID information and chemical ID mapping",
                "Technological assay target at the assay component level")

output <- 
  data.frame(Field, Description)

library(htmlTable)
htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        caption="Table 18: List of annotation tables.",
        tfoot = "** indicates tables not currently used by the *tcpl* package",
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em '
)


## ----warning = FALSE, echo = FALSE---------------------------------------
Field <- c("aid", "asid", "assay_name", "assay_desc", "timepoint_hr", "assay_footprint")

Description <- c("Assay ID",
                 "Assay source ID",
                 "Assay name (abbreviated \"anm\" within the package)",
                 "Assay description",
                 "Treatment duration in hours",
                 "Microtiter plate size&dagger;")

output <- 
  data.frame(Field, Description)

library(htmlTable)
htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        caption="Table 19: Fields in assay.",
        tfoot = "&dagger; discussed further in the \"Register and Upload New Data\" section",
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em '
)


## ----warning = FALSE, echo = FALSE---------------------------------------
Field <- c("acid", "aid", "assay_component_name", "assay_component_desc")

Description <- c("Assay component ID",
                 "Assay ID",
                 "Assay component name (abbreviated \"acnm\" within the package)",
                 "Assay component description"
                 )

output <- 
  data.frame(Field, Description)

library(htmlTable)
htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        caption="Table 20: Fields in assay_component."
)


## ----warning = FALSE, echo = FALSE---------------------------------------
Field <- c("asid", "assay_source_name", "assay_source_long_name", "assay_source_description")

Description <- c("Assay source ID",
                 "Assay source name (typically an abbreviation of the
assay_source_long_name, abbreviated \"asnm\" within the package)",
                 "The full assay source name", 
                 "Assay source description"
                 )

output <- 
  data.frame(Field, Description)

library(htmlTable)
htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        caption="Table 21: Fields in assay_source."
)


## ----warning = FALSE, echo = FALSE---------------------------------------
Field <- c("aeid", "acid", "assay_component_endpoint_name", "assay_component_endpoint_desc", "export_ready", "normalized_data_type", "burst_assay", "fit_all")

Description <- c("Assay component endpoint ID",
                 "Assay component ID",
                 "Assay component endpoint name (abbreviated \"aenm\" within the
package)", 
                 "Assay component endpoint description",
"0 or 1, used to flag data as \"done\"",
                 "The units of the normalized data&dagger;",
                 "0 or 1, 1 indicates the assay results should be used in calculating the burst z-score",
                 "0 or 1, 1 indicates all results should be fit, regardless of whether the *max_med* surpasses *3bmad*"
                 )

output <- 
  data.frame(Field, Description)

library(htmlTable)
htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        caption="Table 22: Fields in assay_component_endpoint.",
        tfoot = "&dagger; discussed further in the \"Register and Upload New Data\" section"
)


## ----warning = FALSE, echo = FALSE---------------------------------------
Field <- c("acid", "acsn")

Description <- c("Assay component ID",
                 "Assay component source name"
                 )

output <- 
  data.frame(Field, Description)

library(htmlTable)
htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        caption="Table 23: Fields in assay_component_map table."
)


## ----warning = FALSE, echo = FALSE---------------------------------------
Field <- c("chid", "casn", "chnm")

Description <- c("Chemical ID&dagger;",
                 "CAS Registry Number",
                 "Chemical name"
                 )

output <- 
  data.frame(Field, Description)

library(htmlTable)
htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        caption="Table 24: Fields in chemical table." ,
        tfoot = "&dagger; this is the DSSTox GSID within the ToxCast data, but can be any integer and will be auto-generated (if not explicitly defined) for newly registered
chemicals"
)


## ----warning = FALSE, echo = FALSE---------------------------------------
Field <- c("chid", "clib")

Description <- c("Chemical ID",
                 "Chemical library"
                 )

output <- 
  data.frame(Field, Description)

library(htmlTable)
htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        caption="Table 25: Fields in chemical_library table." 
     
)


## ----warning = FALSE, echo = FALSE---------------------------------------
Field <- c("fitc", "parent_fitc", "name", "xloc", "yloc")

Description <- c("Fit category",
                 "Parent fit category",
                 "Fit category name" ,
                 "x-axis location for plotting purposes",
                 "y-axis location for plotting purposes"
                 )

output <- 
  data.frame(Field, Description)

library(htmlTable)
htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        caption="Table 26: Fields in mc5_fit_categories." 
     
)


## ----warning = FALSE, echo = FALSE---------------------------------------
Field <- c("spid", "chid", "stkc", "stkc_unit", "tested_conc_unit", 
           "spid_legacy")

Description <- c("Sample ID",
                 "Chemical ID",
                 "Stock concentration" ,
                 "Stock concentration unit",
                 "The concentration unit for the concentration values in the data-containing tables",
                 "A place-holder for previous sample ID strings"
                 )

output <- 
  data.frame(Field, Description)

library(htmlTable)
htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        caption="Table 27: Fields in sample table." 
)


## ----warning = FALSE, echo = FALSE---------------------------------------
Field <- c("acsn", "spid", "cpid", "apid", "rowi", 
           "coli", "wllt", "wllq", "conc", "rval", "srcf")

Description <- c("Assay component source name",
                 "Sample ID",
                 "Chemical plate ID" ,
                 "Assay plate ID",
                 "Assay plate row index, as an integer",
                 "Assay plate column index, as an integer",
                 "Well type",
                 "1 if the well quality was good, else 0",
                 "Concentration in micromolar",
                 "Raw assay component value/readout from vendor",
                 "Filename of the source file containing the data"
                 )
`N/A` <- c("No", "No", "Yes", "Yes","Yes","Yes", "No", "No", "No&dagger;", "Yes&ddagger;", "No")

output <- 
  data.frame(Field, Description, `N/A`)

library(htmlTable)
htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        
          caption="Table 28: Required felds in level 0 pre-processing." ,
          tfoot = "The N/A column indicates whether the field can be N/A in the pre-processed data.
 &dagger;Concentration can be N/A for control values only tested at a single
concentration. Concentration cannot be N/A for any test compound (well
type of \"t\") data.
 &ddagger;If the raw value is N/A, well type has to be 0."
)


## ----warning = FALSE, echo = FALSE---------------------------------------
`Well Type` <- c("t", "c", "p", "n", "m", 
           "o", "b", "v")

Description <- c("Test compound",
                 "Gain-of-signal control in multiple concentrations",
                 "Gain-of-signal control in single concentration" ,
                 "Neutral/negative control",
                 "Loss-of-signal control in multiple concentrations",
                 "Loss-of-signal control in single concentration",
                 "Blank well",
                 "Viability control"
                 )


output <- 
  data.frame(`Well Type`, Description)

library(htmlTable)
htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        caption="Table 29: Well types" 
)


