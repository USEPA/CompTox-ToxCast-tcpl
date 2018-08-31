#-------------------------------------------------------------------------------
# tcplMultiplot: Plot fits based on mc4/5 and mc4_agg
#-------------------------------------------------------------------------------

#' @title Plot summary fits based on fit and dose-response data
#' 
#' @description
#' \code{tcplMultiplot} takes the dose-response and fit data and produces
#' summary plot figures.
#' 
#' @param dat data.table, level 4 or level 5 data, see details.
#' @param agg data.table, concentration-response aggregate data, see details.
#' @param flg data.table, level 6 data, see details.
#' @param hitc.all Logical, if FALSE, only plots with hitc==1 will be displayed
#' @param browse Logical, should \code{browser()} be called after every plot?
#' @param boot data.table, level 7 data, see details.
#' 
#' @details
#' The data for 'dat', 'agg', and 'flg' should be loaded using the 
#' \code{\link{tcplLoadData}} function with the appropriate 'lvl' parameter.
#' See help page for \code{tcplLoadData} for more information.
#' 
#' If dat contains only one aeid, plots will be ordered by chemical name (chnm). 
#' Otherwise, plots are ordered by assay endpoint name (aenm).
#' ## While it is most likely the user will want to just save all of the plots 
#' ## to view in a PDF, the 'browse' parameter can be used to quickly view 
#' ## some plots. 
#' @import data.table
#' @export

tcplMultiplot <- function (dat, agg, flg = NULL, boot = NULL, browse = FALSE, hitc.all) {
  chid <- chnm <- spid <- aenm <- aeid <- m4id <- fitc <- fval <- modl <- hitc <- NULL
  flgo <- mc6_mthd_id <- J <- NULL
  
  if (!is.null(flg) & !"m5id" %in% names(dat)) {
    stop("Must supply level 5 data with a non-null 'flg' input.")
  }
  
  dat <- tcplPrepOtpt(dat)
  dat <- dat[!is.na(modl)]
  dat[is.na(chid), `:=`(chnm, paste(spid, "(spid not in DB)"))]
  dat[, `:=`(aenm, paste0(aenm, " (", "AEID:",aeid, ")"))]
  
  setkey(dat, m4id)
  setkey(agg, m4id)
  if (!hitc.all) dat <- dat[hitc==1]
  #Setting plotting order
  if (length(unique(dat$aenm)) == 1){
  m4ids <- dat[order(chnm), unique(m4id)]
  } else{
    m4ids <- dat[order(aenm), unique(m4id)]
  }
  if (!is.null(flg)) {
    if (nrow(flg) > 0) {
      flg[is.na(fval), `:=`(flgo, as.character(mc6_mthd_id))]
      flg[!is.na(fval), `:=`(flgo, paste0(mc6_mthd_id, 
                                          " (", signif(fval, 3), ")"))]
      flg <- flg[, list(flgo = paste(unique(flgo), collapse = "; ")), 
                 by = m4id]
      setkey(flg, m4id)
      dat <- flg[dat]
    }    else {
      dat[, `:=`(flgo, NA)]
    }
  }
  if (!is.null(boot)) {
    setkey(boot, m4id)
    dat <- boot[dat]
    dat$toxboot <- 1
  }
  for (i in m4ids) {
    resp <- agg[J(i), resp]
    logc <- agg[J(i), logc]
    pars <- as.list(dat[J(i)])
    multiPlotfit(resp = resp, logc = logc, pars = pars)
    if (browse) 
      browser(skipCalls = 4)
  }
}