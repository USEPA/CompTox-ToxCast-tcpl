#------------------------------------------------------------------------------------
# tcplMakeChidMultiPlts: Create a .pdf with all dose-response plots for a given chid
#------------------------------------------------------------------------------------

#' @title Create a .pdf with all dose-response plots for a given chid, 6 per page
#' 
#' @description
#' \code{tcplMakeChidMultiPlts} Create a .pdf with all dose-response plots for a given chid
#' 
#' @param chid Integer of length 1, the chemical id
#' @param lvl Integer of length 1, the data level to use (4-7)
#' @param fname Character, the filename
#' @param odir The directory to save the .pdf file in
#' @param clib Character, the chemical library to subset on, see 
#' \code{\link{tcplLoadClib}} for more information. 
#' @param hitc.all If FALSE, only plots with hitc==1 will be displayed
#' 
#' @details 
#' \code{tcplMakeChidMultiPlts} provides a wrapper for \code{\link{tcplMultiplot}},
#' allowing the user to produce PDFs with the curve plots without having to 
#' separately load all of the data and establish the PDF device.
#' 
#' If 'fname' is \code{NULL}, a default name is given by concatenating together
#' assay information. 
#' @import data.table
#' @importFrom grDevices graphics.off pdf
#' @export 
tcplMakeChidMultiPlts <- function (chid, lvl = 4L, fname = NULL, odir = getwd(), clib = NULL, hitc.all = TRUE) {
  if (check_tcpl_db_schema()) stop("This function is no longer supported in this
                                   version of invitrodb. Consider tcplPlot() instead.")
  spid <- m4id <- NULL
  on.exit(graphics.off())
  if (length(chid) > 1) 
    stop("'chid' must be of length 1.")
  if (length(lvl) > 1 | !lvl %in% 4:7) 
    stop("Invalid 'lvl' input.")
  spids <- tcplLoadChem("chid",chid)$spid
  prs <- list(type = "mc", fld = "spid", val = spids)
  if (lvl < 5L) {
    dat <- do.call(tcplLoadData, args = c(lvl = 4L, prs))
  }  else {
    dat <- do.call(tcplLoadData, args = c(lvl = 5L, prs))
  }
  if (nrow(dat) == 0) 
    stop("No data for chid", chid)
  if (!is.null(clib)) {
    csub <- tcplLoadClib(field = "clib", val = clib)
    dat <- dat[spid %in% tcplLoadChem(field = "chid", val = csub$chid)$spid]
  }
  prs <- list(type = "mc", fld = "m4id", val = dat[, unique(m4id)])
  agg <- do.call(tcplLoadData, args = c(lvl = "agg", prs))
  flg <- if (lvl < 6L) {
    NULL
  }  else do.call(tcplLoadData, args = c(lvl = 6L, prs))
  boot <- if (lvl < 7L) {
    NULL
  }  else do.call(tcplLoadData, args = c(lvl = 7L, prs))
  if (is.null(fname)) {
    fname <- file.path(odir, paste(paste0("CHID", chid), 
                                   paste0("L", lvl), unique(tcplLoadChem("chid", chid)$chnm), 
                                   format(Sys.Date(), "%y%m%d"), "multiplot.pdf",sep = "_"))
  }
  
  
  graphics.off()
  pdf(file = fname, height = 10, width = 6, pointsize = 10)
  par(mfrow=c(3,2))
  tcplMultiplot(dat = dat, agg = agg, flg = flg, boot = boot, hitc.all = hitc.all)
  graphics.off()
  cat(fname, "complete.")
  TRUE
}