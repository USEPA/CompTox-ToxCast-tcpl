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

