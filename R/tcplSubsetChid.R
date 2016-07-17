#-------------------------------------------------------------------------------
# tcplSubsetChid: Subset level 5 data to a single sample per chemical
#-------------------------------------------------------------------------------

#' @title Subset level 5 data to a single sample per chemical
#' 
#' @description
#' \code{tcplSubsetChid} subsets level 5 data to a single tested sample per 
#' chemical. In other words, if a chemical is tested more than once (a chid
#' has more than one spid) for a given assay endpoint, the function uses a 
#' series of logic to select a single "representative" sample.
#' 
#' @param dat data.table, a data.table with level 5 data
#' @param flag Integer, the mc6_mthd_id values to go into the flag count, see
#' details for more information
#'  
#' @details
#' \code{tcplSubsetChid} is intended to work with level 5 data that has 
#' chemical and assay information mapped with \code{\link{tcplPrepOtpt}}.
#' 
#' To select a single sample, first a "consensus hit-call" is made by majority 
#' rule, with ties defaulting to active. After the chemical-wise hit call is 
#' made, the samples corresponding to to chemical-wise hit call are logically 
#' ordered using the fit category, the number of the flags, and the modl_ga, 
#' then the first sample for every chemical is selected.
#' 
#' The \code{flag} param can be used to specify a subset of flags to be used in 
#' the flag count. Leaving \code{flag} TRUE utilize all the available flags.
#' Setting \code{flag} to \code{FALSE} will do the subsetting without 
#' considering any flags.
#' 
#' @examples
#' ## Store the current config settings, so they can be reloaded at the end 
#' ## of the examples
#' conf_store <- tcplConfList()
#' tcplConfDefault()
#' 
#' ## Load the example level 5 data
#' d1 <- tcplLoadData(lvl = 5, fld = "aeid", val = 2)
#' d1 <- tcplPrepOtpt(d1)
#' 
#' ## Subset to an example of a duplicated chid
#' d2 <- d1[chid == 559]
#' d2[ , list(m4id, hitc, fitc, modl_ga)]
#' 
#' ## Here the consensus hit-call is 1 (active), and the fit categories are 
#' ## all equal. Therefore, if the flags are ignored, the selected sample will
#' ## be the sample with the lowest modl_ga.
#' tcplSubsetChid(dat = d2, flag = FALSE)[ , list(m4id, modl_ga)]
#' 
#' ## Reset configuration
#' options(conf_store)
#' 
#' @return A data.table with a single sample for every given chemical-assay 
#' pair. 
#' 
#' @seealso \code{\link{tcplPrepOtpt}}
#' 
#' @import data.table
#' @export

tcplSubsetChid <- function(dat, flag = TRUE) {
  
  ## Variable-binding to pass R CMD Check
  chit <- hitc <- aeid <- casn <- fitc <- fitc.ordr <- m4id <- nflg <- NULL
  chid <- NULL
  
  if (!"m5id" %in% names(dat)) {
    stop("'dat' must be a data.table with level 5 data. See ?tcplLoadData for",
         " more information.")
  }
  if (!"casn" %in% names(dat)) dat <- tcplPrepOtpt(dat)
  
  dat[ , chit := mean(hitc[hitc %in% 0:1]) >= 0.5, by = list(aeid, chid)]
  dat <- dat[hitc == chit | (is.na(chit) & (hitc == -1 | is.na(m4id)))]
  
  dat[ , fitc.ordr := NA_integer_]  
  dat[fitc %in% c(37, 41, 46, 50), fitc.ordr := 0]
  dat[fitc %in% c(38, 42, 47, 51), fitc.ordr := 1]
  dat[fitc %in% c(36, 40, 45, 49), fitc.ordr := 2]
    
  if (is.null(flag)) flag <- TRUE
  
  if (flag[1] | length(flag) > 1) {
    
    tst <- is.logical(flag)
    prs <- if (tst) list() else list(fld = "mc6_mthd_id", val = flag) 
    flg <- do.call(tcplLoadData, c(lvl = 6L, prs))
    flg <- flg[ , list(nflg = .N), by = m4id]
    setkey(flg, m4id)
    setkey(dat, m4id)
    
    dat <- flg[dat]
    
  } else {
    
    dat[ , nflg := FALSE]
    
  }
  
  setkeyv(dat, c("aeid", "chid", "fitc.ordr", "nflg", "modl_ga"))
  min_modl_ga <- dat[ , list(ind = .I[1]), by = list(aeid, casn)]
  dat <- dat[min_modl_ga$ind]
  
  dat[]
  
}

#-------------------------------------------------------------------------------
