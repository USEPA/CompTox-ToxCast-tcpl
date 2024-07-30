#-------------------------------------------------------------------------------
# tcplCytoPt: Calculate the cytotoxicity point based on the "burst assays"
#-------------------------------------------------------------------------------

#' @title Calculate the cytotoxicity point based on the "burst" endpoints
#' 
#' @description
#' \code{tcplCytoPt} calculates the cytotoxicity point and average cytotoxicity 
#' distribution based on the activity in the "burst" assay endpoints.
#' 
#' @param chid Integer, chemical ID values to subset on
#' @param aeid Integer, assay endpoint ID values to override the "burst assay"
#' definitions
#' @param flag Integer, mc6_mthd_id values to be passed to 
#' \code{\link{tcplSubsetChid}}
#' @param min.test Integer or Boolean, the number of tested assay endpoints
#' required for a chemical to be used in calculating the "global MAD."
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
#'  \item "code" -- The chemical code.
#'  \item "chnm" -- The chemical name.
#'  \item "casn" -- The chemical CASRN.
#'  \item "med" -- The median of the "burst" endpoint log(AC50)
#'  \item "mad" -- The MAD of the "burst" endpoint log(AC50) values.
#'  \item "ntst" -- The number of "burst" endpoints tested.
#'  \item "nhit" -- The number of active "burst" endpoints.
#'  \item "used_in_global_mad_calc" -- TRUE/FALSE, whether the mad value was used in the
#'  global MAD calculation.
#'  \item "global_mad" -- The median of the "mad" values where "used_in_global_mad_calc" 
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
#' ## Can only calculate the cytotox burst if using the MySQL database and
#' ## TCPL_DRVR == 'MySQL'
#' 
#' if (getOption("TCPL_DRVR") == "MySQL") {
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
#' ## up direction, and the other is activity in the down direction.)
#' ## Note, the cyto_pt is also 3 for all chemicals, because the function
#' ## requires at least two endpoints to calculate a cytotoxicity point. If 
#' ## the user wishes to use one assay, this function is not necessary. 
#' 
#' ## Changing 'default.pt' will change cyto_pt in the resulting data.table
#' tcplCytoPt(aeid = 1:2, default.pt = 6)
#' }
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
  ac50var <- hitc <- code <- chnm <- casn <- used_in_global_mad_calc <- nhit <- modl <- NULL
  top <- ntst <- global_mad <- cyto_pt <- med <- cyto_pt_um <- lower_bnd_um <- burstpct <- NULL
  
  cat("1: Checking if aeid or chid is specified\n")
  if (!is.null(aeid) & !is.vector(aeid)) {
    stop("'aeid' must be NULL or a vector.")
  }
  
  if (!is.null(chid) & !is.vector(chid)) {
    stop("'chid' must be NULL or a vector.")
  }
  
  cat("2: Checking if min.test is specified\n")
  mt_type <- (is.numeric(min.test) | is.null(min.test) | is.logical(min.test))
  if (!(length(min.test) == 1 & mt_type)) {
    stop("Invalid 'min.test' input. See details.")
  }
  cat("3: Checking if aeid values are set to override the 'burst assay' definitions\n")
  if (is.null(aeid)) {
    cat("3.1: Loading burst assays from database\n")
    ae <- suppressWarnings(tcplLoadAeid("burst_assay", 1)$aeid)
  } else {
    ae <- aeid
  }
  cat("4: Confirming burst assays and minimum tested assays required\n")
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
  cat("5: Loading level 5 data\n")
  zdat <- tcplLoadData(lvl = 5L, fld = "aeid", val = ae, type = "mc")
  cat("6: Appending chemical information to level 5 data\n")
  zdat <- tcplPrepOtpt(dat = zdat)
  cat("7: Filtering chemical data (if necessary)\n")
  if (!is.null(chid)) {
    ch <- chid
    zdat <- zdat[chid %in% ch]
  }
  
  # set list of assays where we should only consider negative direction
  burst_down <- c(26, 46, 158, 160, 178, 198, 222, 226, 252, 254, 270, 292, 316, 318,1091, 2873, 2929, 2931)
  
  # set down burst to -1 hitc but do not filter so it is still included in Ntested.
  zdat[aeid %in% burst_down & top>0,hitc:=-1]
 
  cat("8: Determining representative sample\n")
  zdat <- tcplSubsetChid(dat = zdat, flag = flag)

  #filter out null chids
  zdat <- zdat[!is.na(chid)]
  cat("9: Calculating intermediate summary statistics\n")
  # prior to version 4.0 modl_ga was used as ac50 variable
  # check schema and if using new schema use ac50 instead.
  ac50var <- ifelse(check_tcpl_db_schema(),quote(ac50),quote(modl_ga))
  hitc_num <- ifelse(check_tcpl_db_schema(),.9,1)
  zdst <- zdat[, list(med = median(log10(eval(ac50var))[hitc >= hitc_num]),
                      mad = mad(log10(eval(ac50var))[hitc >= hitc_num]), 
                      ntst = .N, 
                      nhit = lw(hitc >= hitc_num), 
                      burstpct = (lw(hitc>=hitc_num)/.N)), # added burst percent as condition instead of number of hits
               by = list(chid,code, chnm, casn)]
  
  cat("10: Calculating the cytotoxicity point based on the 'burst' endpoints\n")
  #add chems that are not tested in the burst
  total_chem <- tcplLoadChem() |> select(chid,code,chnm,casn) |> as.data.table() |> unique()
  total_chem <- total_chem[!chid %in% zdst$chid,][,`:=`(ntst = 0, nhit = 0, burstpct = 0)]
  zdst <- rbindlist(list(zdst,total_chem), fill = TRUE)

  zdst[, `:=`(used_in_global_mad_calc, burstpct > 0.05 & ntst>=60)] 
  gb_mad <- median(zdst[used_in_global_mad_calc=='TRUE', mad])  #calculate global mad
  zdst[,global_mad := gb_mad] # add column for global mad
  zdst[, cyto_pt := med]
  zdst[burstpct < 0.05 | nhit < 2, `:=`(cyto_pt, default.pt)] # if the burst percent is less than .05 and at least 2 hits use the default pt instead
  zdst[, `:=`(cyto_pt_um, 10^cyto_pt)]
  zdst[, `:=`(lower_bnd_um, 10^(cyto_pt - 3 * global_mad))]
  zdst[,burstpct:=NULL] # remove burstpct from final results to match previous iterations of tcplCytopt
  zdst[]
  
}

#-------------------------------------------------------------------------------
