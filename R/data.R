#' A subset of ToxCast data showing changes in transcription factor activity for multiple targets.
#' 
#' The example dataset is used to illustrate how the user can pipeline single-concentration  
#' data from chemical screening using tcplLite.
#'
#' @format A data frame with 320 rows and 10 variables:
#' \describe{
#' 
#'  \item{spid}{sample ID}
#'   \item{apid}{assay plate ID}
#'    \item{rowi}{well-plate row number (N/A)}
#'     \item{coli}{well-plate column number (N/A)}
#'      \item{wllt}{well type (N/A)}
#'       \item{wllq}{well quality (N/A)}
#'        \item{conc}{concentration in micromolar}
#'         \item{rval}{raw assay component readout value}
#'          \item{srcf}{source file containing the data}
#'           \item{acsn}{assay component source name}
#' }
#' @source Toxcast database
"scdat"
#' A subset of ToxCast data showing changes in the activity of the intracellular estrogen receptor.
#' 
#' The example dataset is used to illustrate how the user can pipeline multiple-concentration  
#' data from chemical screening using tcplLite.
#'
#' @format A data frame with 14183 rows and 10 variables:
#' \describe{
#'  \item{spid}{sample ID}
#'   \item{apid}{assay plate ID}
#'    \item{rowi}{well-plate row number}
#'     \item{coli}{well-plate column number}
#'      \item{wllt}{well type}
#'       \item{wllq}{well quality}
#'        \item{conc}{concentration in micromolar}
#'         \item{rval}{raw assay component readout value}
#'          \item{srcf}{source file containing the data}
#'           \item{acsn}{assay component source name}
#' }
#' @source Toxcast database
"mcdat"
#' Chemical library of tested chemicals in the example datasets with the corresponding sample IDs.
#' 
#'
#' @format A data frame with 6 rows and 6 variables:
#' \describe{
#'  \item{spid}{sample ID}
#'   \item{casn}{Chemical Abstract Service(CAS) number}
#'    \item{chnm}{chemical name}
#'     \item{dsstox_substance_id}{chemical-specific DTXSID}
#'      \item{code}{CAS number compressed into numeric string}
#'       \item{chid}{unique chemical ID number for tcpl}
#'
#' }
#' @source Toxcast database
"chdat"


#' List with multi-concentration data for the vignette
#'
#' This dataset is a list with 6 data.tables (mc0,mc1,mc2,mc3,mc4,mc5). 
#'
#' @format 
#' \enumerate{
#' \item \strong{mc0} A data frame with 78 rows and 18 columns
#' \describe{
#'   \item{spid}{sample ID}
#'   \item{chid}{unique chemical ID number for tcpl}
#'   \item{casn}{Chemical Abstract Service(CAS) number}
#'   \item{chnm}{chemical name}
#'   \item{dsstox_substance_id}{chemical-specific DTXSID}
#'   \item{code}{CAS number compressed into numeric string}
#'   \item{acid}{Assay Component ID}
#'   \item{acnm}{Assay Component Name}
#'   \item{m0id}{mc0 ID}
#'   \item{apid}{assay plate ID}
#'   \item{rowi}{Row Index}
#'   \item{coli}{Column Index}
#'   \item{wllt}{Well Type}
#'   \item{wllq}{Well Quality (0 or 1)}
#'   \item{conc}{concentration in micromolar}
#'   \item{rval}{raw assay component readout value}
#'   \item{srcf}{source file containing the raw data}
#'   \item{conc_unit}{Concentration Units}
#'   }
#'
#' \item \strong{mc1} A data frame with 78 rows and 21 columns
#' \describe{
#'   \item{spid}{sample ID}
#'   \item{chid}{unique chemical ID number for tcpl}
#'   \item{casn}{Chemical Abstract Service(CAS) number}
#'   \item{chnm}{chemical name}
#'   \item{dsstox_substance_id}{chemical-specific DTXSID}
#'   \item{code}{CAS number compressed into numeric string}
#'   \item{acid}{Assay Component ID}
#'   \item{acnm}{Assay Component Name}
#'   \item{m0id}{mc0 ID}
#'   \item{m1id}{mc1 ID}
#'   \item{apid}{assay plate ID}
#'   \item{rowi}{Row Index}
#'   \item{coli}{Column Index}
#'   \item{wllt}{Well Type}
#'   \item{wllq}{Well Quality (0 or 1)}
#'   \item{conc}{concentration in micromolar}
#'   \item{rval}{raw assay component readout value}
#'   \item{cndx}{concentration index defined by ranking the unique concentrations, with the lowest concentration starting at 1.}
#'   \item{repi}{Temporary replicate ID is defined, the data are scanned from top to bottom and increment the replicate index every time a replicate ID is duplicated}
#'   \item{srcf}{source file containing the raw data}
#'   \item{conc_unit}{Concentration Units}
#'   }
#' 
#' 
#' \item \strong{mc2} A data frame with 78 rows and 20 columns
#' \describe{
#'   \item{spid}{sample ID}
#'   \item{chid}{unique chemical ID number for tcpl}
#'   \item{casn}{Chemical Abstract Service(CAS) number}
#'   \item{chnm}{chemical name}
#'   \item{dsstox_substance_id}{chemical-specific DTXSID}
#'   \item{code}{CAS number compressed into numeric string}
#'   \item{acid}{Assay Component ID}
#'   \item{acnm}{Assay Component Name}
#'   \item{m0id}{mc0 ID}
#'   \item{m1id}{mc1 ID}
#'   \item{m2id}{mc2 ID}
#'   \item{apid}{assay plate ID}
#'   \item{rowi}{Row Index}
#'   \item{coli}{Column Index}
#'   \item{wllt}{Well Type}
#'   \item{conc}{concentration in micromolar}
#'   \item{cval}{Corrected Value}
#'   \item{cndx}{concentration index defined by ranking the unique concentrations, with the lowest concentration starting at 1.}
#'   \item{repi}{Temporary replicate ID is defined, the data are scanned from top to bottom and increment the replicate index every time a replicate ID is duplicated}
#'   \item{conc_unit}{Concentration Units}
#'   }
#' 
#' 
#' \item \strong{mc3} A data frame with 78 rows and 22 columns
#' \describe{
#'   \item{spid}{sample ID}
#'   \item{chid}{unique chemical ID number for tcpl}
#'   \item{casn}{Chemical Abstract Service(CAS) number}
#'   \item{chnm}{chemical name}
#'   \item{dsstox_substance_id}{chemical-specific DTXSID}
#'   \item{code}{CAS number compressed into numeric string}
#'   \item{aeid}{Assay Component Endpoint ID}
#'   \item{aenm}{assay endpoint name (i.e., assay_component_endpoint_name)}
#'   \item{m0id}{mc0 ID}
#'   \item{m1id}{mc1 ID}
#'   \item{m2id}{mc2 ID}
#'   \item{m3id}{mc3 ID}
#'   \item{logc}{Log base 10 concentration}
#'   \item{resp}{Normalized response value}
#'   \item{cndx}{concentration index defined by ranking the unique concentrations, with the lowest concentration starting at 1.}
#'   \item{wllt}{Well Type}
#'   \item{apid}{assay plate ID}
#'   \item{rowi}{Row Index}
#'   \item{coli}{Column Index}
#'   \item{repi}{Temporary replicate ID is defined, the data are scanned from top to bottom and increment the replicate index every time a replicate ID is duplicated}
#'   \item{resp_unit}{Response Units}
#'   \item{conc_unit}{Concentration Units}
#'   }
#'
#' 
#' \item \strong{mc4} A data frame with 5 rows and 149 columns
#' \describe{
#'   \item{spid}{sample ID}
#'   \item{chid}{unique chemical ID number for tcpl}
#'   \item{casn}{Chemical Abstract Service(CAS) number}
#'   \item{chnm}{chemical name}
#'   \item{dsstox_substance_id}{chemical-specific DTXSID}
#'   \item{code}{CAS number compressed into numeric string}
#'   \item{aeid}{Assay Component Endpoint ID}
#'   \item{aenm}{assay endpoint name (i.e., assay_component_endpoint_name)}
#'   \item{m4id}{mc4 ID}
#'   \item{bmad}{The median absolute deviation of all treatment wells (default option) or blank wells }
#'   \item{resp_max}{maximum observed response}
#'   \item{resp_min}{Minimum observed response}
#'   \item{max_mean}{Maximum mean response}
#'   \item{max_mean_conc}{concentration of the maximum mean response}
#'   \item{max_med}{maximum median response}
#'   \item{max_med_conc}{concentration of the maximum median response}
#'   \item{logc_max}{Maximum concentration on the log scale}
#'   \item{logc_min}{Minimum concentration on the log scale}
#'   \item{nconc}{The total number of concentration groups}
#'   \item{npts}{Total number of observed responses (i.e. data points in the concentration series) }
#'   \item{nrep}{Number of replicates in concentration groups}
#'   \item{nmed_gtbl}{The number of median responses greater than 3BMAD}
#'   \item{cnst_success}{}
#'   \item{cnst_aic}{}
#'   \item{cnst_rme}{}
#'   \item{cnst_er}{}
#'   \item{hill_success}{}
#'   \item{hill_aic}{}
#'   \item{hill_cov}{}
#'   \item{hill_rme}{}
#'   \item{hill_tp}{}
#'   \item{hill_ga}{}
#'   \item{hill_p}{}
#'   \item{hill_er}{}
#'   \item{hill_tp_sd}{}
#'   \item{hill_ga_sd}{}
#'   \item{hill_p_sd}{}
#'   \item{hill_er_sd}{}
#'   \item{hill_top}{}
#'   \item{hill_ac50}{}
#'   \item{gnls_success}{}
#'   \item{gnls_aic}{}
#'   \item{gnls_cov}{}
#'   \item{gnls_rme}{}
#'   \item{gnls_tp}{}
#'   \item{gnls_ga}{}
#'   \item{gnls_p}{}
#'   \item{gnls_la}{}
#'   \item{gnls_q}{}
#'   \item{gnls_er}{}
#'   \item{gnls_tp_sd}{}
#'   \item{gnls_ga_sd}{}
#'   \item{gnls_p_sd}{}
#'   \item{gnls_la_sd}{}
#'   \item{gnls_q_sd}{}
#'   \item{gnls_er_sd}{}
#'   \item{gnls_top}{}
#'   \item{gnls_ac50}{}
#'   \item{gnls_ac50_loss}{}
#'   \item{poly1_success}{}
#'   \item{poly1_aic}{}
#'   \item{poly1_cov}{}
#'   \item{poly1_rme}{}
#'   \item{poly1_a}{}
#'   \item{poly1_er}{}
#'   \item{poly1_a_sd}{}
#'   \item{poly1_er_sd}{}
#'   \item{poly1_top}{}
#'   \item{poly1_ac50}{}
#'   \item{poly2_success}{}
#'   \item{poly2_aic}{}
#'   \item{poly2_cov}{}
#'   \item{poly2_rme}{}
#'   \item{poly2_a}{}
#'   \item{poly2_b}{}
#'   \item{poly2_er}{}
#'   \item{poly2_a_sd}{}
#'   \item{poly2_b_sd}{}
#'   \item{poly2_er_sd}{}
#'   \item{poly2_top}{}
#'   \item{poly2_ac50}{}
#'   \item{pow_success}{}
#'   \item{pow_aic}{}
#'   \item{pow_cov}{}
#'   \item{pow_rme}{}
#'   \item{pow_a}{}
#'   \item{pow_p}{}
#'   \item{pow_er}{}
#'   \item{pow_a_sd}{}
#'   \item{pow_p_sd}{}
#'   \item{pow_er_sd}{}
#'   \item{pow_top}{}
#'   \item{pow_ac50}{}
#'   \item{exp2_success}{}
#'   \item{exp2_aic}{}
#'   \item{exp2_cov}{}
#'   \item{exp2_rme}{}
#'   \item{exp2_a}{}
#'   \item{exp2_b}{}
#'   \item{exp2_er}{}
#'   \item{exp2_a_sd}{}
#'   \item{exp2_b_sd}{}
#'   \item{exp2_er_sd}{}
#'   \item{exp2_top}{}
#'   \item{exp2_ac50}{}
#'   \item{exp3_success}{}
#'   \item{exp3_aic}{}
#'   \item{exp3_cov}{}
#'   \item{exp3_rme}{}
#'   \item{exp3_a}{}
#'   \item{exp3_b}{}
#'   \item{exp3_p}{}
#'   \item{exp3_er}{}
#'   \item{exp3_a_sd}{}
#'   \item{exp3_b_sd}{}
#'   \item{exp3_p_sd}{}
#'   \item{exp3_er_sd}{}
#'   \item{exp3_top}{}
#'   \item{exp3_ac50}{}
#'   \item{exp4_success}{}
#'   \item{exp4_aic}{}
#'   \item{exp4_cov}{}
#'   \item{exp4_rme}{}
#'   \item{exp4_tp}{}
#'   \item{exp4_ga}{}
#'   \item{exp4_er}{}
#'   \item{exp4_tp_sd}{}
#'   \item{exp4_ga_sd}{}
#'   \item{exp4_er_sd}{}
#'   \item{exp4_top}{}
#'   \item{exp4_ac50}{}
#'   \item{exp5_success}{}
#'   \item{exp5_aic}{}
#'   \item{exp5_cov}{}
#'   \item{exp5_rme}{}
#'   \item{exp5_tp}{}
#'   \item{exp5_ga}{}
#'   \item{exp5_p}{}
#'   \item{exp5_er}{}
#'   \item{exp5_tp_sd}{}
#'   \item{exp5_ga_sd}{}
#'   \item{exp5_p_sd}{}
#'   \item{exp5_er_sd}{}
#'   \item{exp5_top}{}
#'   \item{exp5_ac50}{}
#'   \item{all_onesd}{}
#'   \item{all_bmed}{}
#'   \item{resp_unit}{Response Units}
#'   \item{conc_unit}{Concentration Units}
#'   }
#'
#' 
#' \item \strong{mc5} A data frame with 5 rows and 54 columns
#' \describe{
#'   \item{spid}{sample ID}
#'   \item{chid}{unique chemical ID number for tcpl}
#'   \item{casn}{Chemical Abstract Service(CAS) number}
#'   \item{chnm}{chemical name}
#'   \item{dsstox_substance_id}{chemical-specific DTXSID}
#'   \item{code}{CAS number compressed into numeric string}
#'   \item{aeid}{Assay Component Endpoint ID}
#'   \item{aenm}{assay endpoint name (i.e., assay_component_endpoint_name)}
#'   \item{m5id}{mc5 ID}
#'   \item{m4id}{mc4 ID}
#'   \item{bmad}{The median absolute deviation of all treatment wells (default option) or blank wells }
#'   \item{resp_max}{maximum observed response}
#'   \item{resp_min}{Minimum observed response}
#'   \item{max_mean}{Maximum mean response}
#'   \item{max_mean_conc}{concentration of the maximum mean response}
#'   \item{max_med}{maximum median response}
#'   \item{max_med_conc}{concentration of the maximum median response}
#'   \item{logc_max}{Maximum concentration on the log scale}
#'   \item{logc_min}{Minimum concentration on the log scale}
#'   \item{nconc}{The total number of concentration groups}
#'   \item{npts}{Total number of observed responses (i.e. data points in the concentration series) }
#'   \item{nrep}{Number of replicates in concentration groups}
#'   \item{nmed_gtbl}{The number of median responses greater than 3BMAD}
#'   \item{hitc}{Hitcall}
#'   \item{modl}{}
#'   \item{fitc}{}
#'   \item{coff}{Cutoff}
#'   \item{top_over_cutoff}{}
#'   \item{rmse}{}
#'   \item{a}{}
#'   \item{er}{}
#'   \item{bmr}{}
#'   \item{bmdl}{}
#'   \item{caikwt}{}
#'   \item{mll}{}
#'   \item{hitcall}{}
#'   \item{ac50}{}
#'   \item{top}{}
#'   \item{ac5}{}
#'   \item{ac10}{}
#'   \item{ac20}{}
#'   \item{acc}{}
#'   \item{ac1sd}{}
#'   \item{bmd}{}
#'   \item{bmdu}{}
#'   \item{tp}{}
#'   \item{ga}{}
#'   \item{p}{}
#'   \item{q}{}
#'   \item{la}{}
#'   \item{ac50_loss}{}
#'   \item{b}{}
#'   \item{resp_unit}{Response Units}
#'   \item{conc_unit}{Concentration Units}
#'   }
#' }
'mc_vignette'

#' List with single-concentration data for the vignette
#'
#' This dataset is a list with 3 data.tables (sc0,sc1,sc2). 
#'
#' @format 
#' \enumerate{
#' \item \strong{sc0} A data frame with 10 rows and 18 columns
#' \describe{
#'   \item{spid}{sample ID}
#'   \item{chid}{unique chemical ID number for tcpl}
#'   \item{casn}{Chemical Abstract Service(CAS) number}
#'   \item{chnm}{chemical name}
#'   \item{dsstox_substance_id}{chemical-specific DTXSID}
#'   \item{code}{CAS number compressed into numeric string}
#'   \item{acid}{Assay Component ID}
#'   \item{acnm}{Assay Component Name}
#'   \item{s0id}{SC0 ID}
#'   \item{apid}{assay plate ID}
#'   \item{rowi}{Row Index}
#'   \item{coli}{Column Index}
#'   \item{wllt}{Well Type}
#'   \item{wllq}{Well Quality (0 or 1)}
#'   \item{conc}{concentration in micromolar}
#'   \item{rval}{raw assay component readout value}
#'   \item{srcf}{source file containing the raw data}
#'   \item{conc_unit}{Concentration Units}
#'   }
#'
#'
#' \item \strong{sc1} A data frame with 10 rows and 20 columns
#' \describe{
#'   \item{spid}{sample ID}
#'   \item{chid}{unique chemical ID number for tcpl}
#'   \item{casn}{Chemical Abstract Service(CAS) number}
#'   \item{chnm}{chemical name}
#'   \item{dsstox_substance_id}{chemical-specific DTXSID}
#'   \item{code}{CAS number compressed into numeric string}
#'   \item{aeid}{Assay Component Endpoint ID}
#'   \item{aenm}{assay endpoint name (i.e., assay_component_endpoint_name)}
#'   \item{acid}{Assay Component ID}
#'   \item{acnm}{Assay Component Name}
#'   \item{s0id}{SC0 ID}
#'   \item{s1id}{SC1 ID}
#'   \item{apid}{assay plate ID}
#'   \item{rowi}{Row Index}
#'   \item{coli}{Column Index}
#'   \item{wllt}{Well Type}
#'   \item{logc}{Log base 10 concentration}
#'   \item{resp}{Normalized response value}
#'   \item{resp_unit}{Response Units}
#'   \item{conc_unit}{Concentration Units}
#'   }
#'
#' 
#' \item \strong{sc2} A data frame with 10 rows and 15 columns
#' \describe{
#'   \item{spid}{sample ID}
#'   \item{chid}{unique chemical ID number for tcpl}
#'   \item{casn}{Chemical Abstract Service(CAS) number}
#'   \item{chnm}{chemical name}
#'   \item{dsstox_substance_id}{chemical-specific DTXSID}
#'   \item{code}{CAS number compressed into numeric string}
#'   \item{aeid}{Assay Component Endpoint ID}
#'   \item{aenm}{assay endpoint name (i.e., assay_component_endpoint_name)}
#'   \item{s2id}{SC2 ID}
#'   \item{bmad}{The median absolute deviation of all treatment wells (default option) or blank wells }
#'   \item{max_med}{maximum median response}
#'   \item{hitc}{Hitcall}
#'   \item{coff}{Cutoff}
#'   \item{resp_unit}{Response Units}
#'   \item{conc_unit}{Concentration Units}
#'   }
#' }
'sc_vignette'

