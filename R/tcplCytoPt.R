#-------------------------------------------------------------------------------
# tcplCytoPt: Calculate the cytotoxicity point based on the "burst assays"
#-------------------------------------------------------------------------------

#' @title Calculate the cytotoxicity point based on the "burst" endpoints
#' 
#' @description
#' \code{tcplCytoPt} calculates the cytotoxicity point and average cytotoxicity 
#' distribution based on the acitivty in the "burst" assay endpoints.
#' 
#' @param chid Integer, chemical ID values to subset on
#' @param aeid Integer, assay endpoint ID values to override the "burst assay"
#' definitions
#' @param flag Integer, mc6_mthd_id values to be passed to 
#' \code{\link{tcplSubsetChid}}
#' @param min.test Integer or Boolean, the number of tested assay endpoints
#' required for a cheimcal to be used in calculating the "global MAD."
#' @param default.pt Numeric of length 1, the default cytotoxicity point value
#' 
#' @details
#' \code{tcplCytoPt} provides estimates for chemical-specific cytotoxicity 
#' distributions (more information available in the vignette.) Before 
#' calculating the cytotoxicity distributions, the level 5 data is subsetted
#' by the \code{\link{tcplSubsetChid}} function. 
#' 
#' The 'chid' parameter specifies a subset of chemicals to use in the 
#' calculations, given by chemical ID (chid). The 'aeid' parameter specifies
#' which assays to use in calculating the cytotoxicity point and distribution.
#' By default \code{tcplCytoPt} will use all available chemicals and the 
#' assay endpoints defined by the 'burst_assay' field in the 
#' "assay_component_endpoint" table. The examples show how to identify the 
#' "burst" endpoints.
#' 
#' \code{tcplCytoPt} returns the cytotoxicity point (the AC50 values of the 
#' active "burst" endpoints), the corresponding MAD, and the global MAD (median
#' of the calculated MAD values). Not every chemical must be tested in every
#' "burst" endpoint. The 'min.test' parameter allows the user to specify a 
#' minimum number of tested assay endpoints as a requirement for MAD values to 
#' be included in the global MAD calculation. For example, suppose the user 
#' supplies 10 "burst" assays. The user can choose to require a chemical to be
#' tested in at least 5 of those assays for it's MAD value to be included in 
#' the global MAD calculation. Having chemicals with many less "burst" endpoints
#' tested may inflate or deflate the global MAD calculation. By default (values 
#' of \code{TRUE} or \code{NULL}), \code{tcplCytoPt} requires a chemical to be 
#' tested in at least 80\% of the given "burst" assays. The user can also 
#' provide 'min.test' values of \code{FALSE} (indicating to include all MAD 
#' values), or a number (indicating a specific number of endpoints). 
#' 
#' Chemicals without at least 2 active "burst" assays do not have a MAD value, 
#' and the cytotoxicity point is defined by the 'default.pt' parameter. The
#' default value for 'default.pt' is 3.
#' 
#' The resulting data.table has the following fields:
#' \enumerate{
#'  \item "chid" -- The chemical ID.
#'  \item "code" -- The chemcial code.
#'  \item "chnm" -- The chemical name.
#'  \item "casn" -- The chemical CASRN.
#'  \item "med" -- The median of the "burst" endpoint log(AC50) ("modl_ga" in 
#'  the level 5 output) values.
#'  \item "mad" -- The MAD of the "burst" endpoint log(AC50) values.
#'  \item "ntst" -- The number of "burst" endpoints tested.
#'  \item "nhit" -- The number of acive "burst" endpoints.
#'  \item "use_global_mad" -- TRUE/FALSE, whether the mad value was used in the
#'  global MAD calculation.
#'  \item "global_mad" -- The median of the "mad" values where "use_global_mad" 
#'  is TRUE.
#'  \item "cyto_pt" -- The cytotoxicity point, or the value in "med" when 
#'  "nhit" is at least 2.
#'  \item "cyto_pt_um" -- \eqn{10^\mathit{cyto\_pt}}{10^cyto_pt}  
#'  \item "lower_bnd_um" -- \eqn{10^{\mathit{cyto\_pt} - 3\mathit{global\_mad}}}{10^(cyto_pt - 3*global_mad)}
#' }
#' 
#' 
#' @examples 
#' ## Store the current config settings, so they can be reloaded at the end 
#' ## of the examples
#' conf_store <- tcplConfList()
#' tcplConfDefault()
#' 
#' ## Load the "burst" endpoints -- none are defined in the example dataset
#' tcplLoadAeid(fld = "burst_assay", val = 1)
#' 
#' ## Calculate the cytotoxicity distributions using both example endpoints
#' tcplCytoPt(aeid = 1:2)
#' 
#' ## The above example does not calculate a global MAD, because no chemical
#' ## hit both endpoints. (This makes sense, because both endpoints are 
#' ## derived from one component, where one endpoint is acitivity in the
#' ## up direction, and the other is acitivty in the down direction.)
#' ## Note, the cyto_pt is also 3 for all chemicals, because the function
#' ## requires at least two endpoints to calculate a cytotoxicity point. If 
#' ## the user wishes to use one assay, this function is not necessary. 
#' 
#' ## Changing 'default.pt' will change cyto_pt in the resulting data.table
#' tcplCytoPt(aeid = 1:2, default.pt = 6)
#' 
#' ## Reset configuration
#' options(conf_store)
#' 
#' @return A data.table with the cytotoxicity distribution for each chemical.
#' The definition of the field names are listed under "details."
#' 
#' @import data.table
#' @export

tcplCytoPt <- function(chid = NULL, aeid = NULL, flag = TRUE, 
                       min.test = TRUE, default.pt = 3) {
  
  ## Variable-binding to pass R CMD Check
  modl_ga <- hitc <- code <- chnm <- casn <- use_global_mad <- nhit <- NULL
  ntst <- global_mad <- cyto_pt <- med <- cyto_pt_um <- lower_bnd_um <- NULL
  
  if (!is.null(aeid) & !is.vector(aeid)) {
    stop("'aeid' must be NULL or a vector.")
  }
  
  if (!is.null(chid) & !is.vector(chid)) {
    stop("'chid' must be NULL or a vector.")
  }
  
  mt_type <- (is.numeric(min.test) | is.null(min.test) | is.logical(min.test))
  if (!(length(min.test) == 1 & mt_type)) {
    stop("Invalid 'min.test' input. See details.")
  }
  
  if (is.null(aeid)) {
    ae <- suppressWarnings(tcplLoadAeid("burst_assay", 1)$aeid)
  } else {
    ae <- aeid
  }
  
  if (length(ae) == 0) stop("No burst assays defined.")
  
  if (is.null(min.test)) {
    min.test <- TRUE
    warning("'min.test' input was NULL and interpreted as TRUE.")
  }
  
  if (min.test) {
    mtst <- if (is.logical(min.test)) floor(0.8 * length(ae)) else min.test 
  } else {
    mtst <- 0
  }
  
  zdat <- tcplLoadData(lvl = 5L, fld = "aeid", val = ae, type = "mc") 
  zdat <- tcplPrepOtpt(dat = zdat)
  
  if (!is.null(chid)) {ch <- chid; zdat <- zdat[chid %in% ch]}
  
  zdat <- tcplSubsetChid(dat = zdat, flag = flag)  
  
  zdst <- zdat[ , 
                list(med = median(modl_ga[hitc == 1]),
                     mad = mad(modl_ga[hitc == 1]),
                     ntst = .N,
                     nhit = lw(hitc == 1)),
                by = list(chid, code, chnm, casn)]
  zdst[ , use_global_mad := nhit > 1 & ntst > mtst]
  zdst[ , global_mad := median(mad[use_global_mad])]
  zdst[ , cyto_pt := med]
  zdst[nhit < 2, cyto_pt := default.pt]
  zdst[ , cyto_pt_um := 10^cyto_pt]
  zdst[ , lower_bnd_um := 10^(cyto_pt - 3*global_mad)]
  
  zdst[]
  
}

#-------------------------------------------------------------------------------
