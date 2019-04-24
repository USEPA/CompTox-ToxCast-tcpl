#-------------------------------------------------------------------------------
# tcplPlotFits: Plot fits based on mc4/5 and mc4_agg
#-------------------------------------------------------------------------------

#' @title Plot summary fits based on fit and dose-response data
#' 
#' @description
#' \code{tcplPlotFits} takes the dose-response and fit data and produces
#' summary plot figures.
#' 
#' @param dat data.table, level 4 or level 5 data, see details.
#' @param agg data.table, concentration-response aggregate data, see details.
#' @param flg data.table, level 6 data, see details.
#' @param ordr.fitc Logical, should the fits be ordered by fit category?
#' @param browse Logical, should \code{browser()} be called after every plot?
#' @param boot data.table, level 7 data, see details.
#' 
#' @details
#' The data for 'dat', 'agg', and 'flg' should be loaded using the 
#' \code{\link{tcplLoadData}} function with the appropriate 'lvl' parameter.
#' See help page for \code{tcplLoadData} for more information.
#' 
#' Supplying level 4 data for the 'dat' parameter will result in level 4 plots. 
#' Similarly, supp
#' 
#' If fits are not ordered by fit category, they will be ordered by chemical 
#' ID. Inputs with multiple assay endpoints will first be ordered by assay 
#' endpoint ID.
#' 
#' @examples 
#' 
#' ## Store the current config settings, so they can be reloaded at the end 
#' ## of the examples
#' conf_store <- tcplConfList()
#' tcplConfDefault()
#' 
#' ## tcplPlotFits needs data.tables supplying the concentration/response
#' ## data stored in mc4_agg, as well as the fit information from mc4 or mc5.
#' ## Additionally, tcplPlotFits can take level 6 data from mc6 and add the
#' ## flag information to the plots. The following shows how to make level 5
#' ## plots. Adding the 'flg' parameter would result in level 6 plots, and 
#' ## loading level 4, rather than level 5 data, would result in level 4 plots.
#'  
#' l5 <- tcplLoadData(lvl = 5, fld = "m4id", val = 18609966)
#' l4_agg <- tcplLoadData(lvl = "agg", fld = "m4id", val = 18609966)
#' 
#' \dontrun{
#' pdf(file = "tcplPlotFits.pdf", height = 6, width = 10, pointsize = 10)
#' tcplPlotFits(dat = l5, agg = l4_agg)
#' graphics.off()
#' }
#' 
#' ## While it is most likely the user will want to just save all of the plots 
#' ## to view in a PDF, the 'browse' parameter can be used to quickly view 
#' ## some plots. 
#' 
#' ## Start by identifying some sample IDs to plot, then call tcplPlotFits with
#' ## a subset of the data. This browse function is admittedly clunky. 
#' bpa <- tcplLoadChem(field = "chnm", val = "Bisphenol A")[ , spid]
#' l5_sub <- l5[spid %in% bpa] 
#' \dontrun{
#' tcplPlotFits(dat = l5_sub, 
#'              agg = l4_agg[m4id %in% l5_sub$m4id], 
#'              browse = TRUE)
#' }
#' 
#' ## Reset configuration
#' options(conf_store)
#'  
#' @import data.table
#' @export

tcplPlotFits <- function(dat, agg, flg = NULL, boot = NULL, ordr.fitc = FALSE, 
                         browse = FALSE, cnst=NULL, orig.aeid=NULL) {
  
  ## Variable-binding to pass R CMD Check
  chid <- chnm <- spid <- aenm <- aeid <- m4id <- fitc <- fval <- NULL
  flgo <- mc6_mthd_id <- J <- NULL
  
  if (!is.null(flg) & !"m5id" %in% names(dat)) {
    stop("Must supply level 5 data with a non-null 'flg' input.")
  }
  
  dat <- tcplPrepOtpt(dat)
  dat[is.na(chid), chnm := paste(spid, "(spid not in DB)")]
  dat[ , aenm := paste0("AEID", aeid, " (", aenm, ")")]
  
  if (length(unique(dat$aeid)) == 1) {
    # Original plot using unique panel for each m4id
  
    setkey(dat, m4id)
    setkey(agg, m4id)
    
    ## Set the plotting order
    if (ordr.fitc && "fitc" %in% names(dat)) {
      m4ids <- dat[order(aeid, fitc, chid), unique(m4id)]
    } else {
      m4ids <- dat[order(aeid, chid), unique(m4id)]
    }
    
    if (!is.null(flg)) {
      if (nrow(flg) > 0) {
        flg[is.na(fval),  flgo := as.character(mc6_mthd_id)]
        flg[!is.na(fval), 
            flgo := paste0(mc6_mthd_id, " (", signif(fval, 3), ")")]
        #LINE BELOW THIS IS WHERE DATA.TABLE ERRORS
        flg <- flg[ , 
                   list(flgo = paste(unique(flgo), collapse = "; ")), 
                   by = m4id]
        setkey(flg, m4id)
        dat <- flg[dat]
      } else {
        dat[ , flgo := NA]
      }
    }
    
    if (!is.null(boot)) {
      setkey(boot, m4id)
      #Join boot table with dat table
      dat <- boot[dat]
      dat$toxboot <- 1
    }
    
    for (i in m4ids) {
      
      resp <- agg[J(i), resp]
      logc <- agg[J(i), logc]
      pars <- as.list(dat[J(i)])
      .plotFit(resp = resp, logc = logc, pars = pars)
      if (browse) browser(skipCalls = 4)
      
    }
  } else {
    # Compare AIED plotting for 2 endpoints
    
    
    
    ## Set the plotting order
    if (ordr.fitc && "fitc" %in% names(dat)) {
      spids <- dat[order(aeid, fitc, chid), unique(spid)]
    } else {
      spids <- dat[order(aeid, chid), unique(spid)]
    }
    
    if (!is.null(flg)) {
      setkey(dat, m4id)
      if (nrow(flg) > 0) {
        flg[is.na(fval),  flgo := as.character(mc6_mthd_id)]
        flg[!is.na(fval), 
            flgo := paste0(mc6_mthd_id, " (", signif(fval, 3), ")")]
        #LINE BELOW THIS IS WHERE DATA.TABLE ERRORS
        flg <- flg[ , 
                    list(flgo = paste(unique(flgo), collapse = "; ")), 
                    by = m4id]
        setkey(flg, m4id)
        dat <- flg[dat]
      } else {
        dat[ , flgo := NA]
      }
    }
    
    if (!is.null(boot)) {
      setkey(boot, m4id)
      #Join boot table with dat table
      dat <- boot[dat]
      dat$toxboot <- 1
    }
    
    # Make sure the order is correct for 'dat' and 'agg'
    dat <- dat[order(match(aeid, orig.aeid))]
    setkey(dat, spid)
    agg <- agg[order(match(aeid, orig.aeid))]
    setkey(agg, spid)
    
    for (i in spids) {
      resp <- agg[J(i), resp]
      logc <- agg[J(i), logc]
      aeids <- agg[J(i), aeid]
      #tmp <- dat[J(i)]
      #pars <- as.list(tmp[match(orig.aeid,tmp$aeid),]) # maintain order of user aeid input
      pars <- as.list(dat[J(i)])
      #.plotFit(resp = resp, logc = logc, pars = pars)
      
      if (!any(is.na(pars$spid)) & all(orig.aeid %in% aeids)) {
        .plotCompareFit(all.resp = resp, all.logc = logc, aeids = aeids, all.pars=pars, cnst=cnst, orig.aeid=orig.aeid)
        if (browse) browser(skipCalls = 4)
      }
    }
  }
  
  
}

#-------------------------------------------------------------------------------
