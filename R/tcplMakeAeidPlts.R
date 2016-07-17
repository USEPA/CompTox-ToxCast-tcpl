#-------------------------------------------------------------------------------
# tcplMakeAeidPlts: Create a .pdf with dose-response plots
#-------------------------------------------------------------------------------

#' @title Create a .pdf with dose-response plots
#' 
#' @description
#' \code{tcplMakeAeidPlts} creates a .pdf file with the dose-response plots for 
#' the given aeid.
#' 
#' @param aeid Integer of length 1, the assay endpoint id
#' @param lvl Integer of lengh 1, the data level to use (4-6)
#' @param fname Character, the filename
#' @param odir The directory to save the .pdf file in
#' @param clib Character, the chemical library to subset on, see 
#' \code{\link{tcplLoadClib}} for more information. 
#' @inheritParams tcplPlotFits
#' 
#' @details 
#' \code{tcplMakeAeidPlts} provides a wrapper for \code{\link{tcplPlotFits}},
#' allowing the user to produce PDFs with the curve plots without having to 
#' separately load all of the data and establish the PDF device.
#' 
#' If 'fname' is \code{NULL}, a default name is given by concatenating together
#' assay information. 
#' 
#' Note, the default value for ordr.fitc is \code{TRUE} in 
#' \code{tcplMakeAeidPlts}, but \code{FALSE} in \code{tcplPlotFits}
#' 
#' @examples
#' \dontrun{
#' ## Will produce the same result as the example for tcplPlotFits
#' tcplMakeAeidPlts(aeid = 1, lvl = 6, ordr.fitc = FALSE)
#' }
#' 
#' @import data.table
#' @importFrom grDevices graphics.off pdf
#' @export

tcplMakeAeidPlts <- function(aeid, lvl = 4L, fname = NULL, odir = getwd(), 
                             ordr.fitc = TRUE, clib = NULL) {
  
  ## Variable-binding to pass R CMD Check
  spid <- m4id <- NULL
  
  on.exit(graphics.off())
  
  if (length(aeid) > 1) stop("'aeid' must be of length 1.")
  if (length(lvl) > 1 | !lvl %in% 4:6) stop("Invalid 'lvl' input.")
  
  prs <- list(type = "mc", fld = "aeid", val = aeid)
  
  if (lvl < 5L) {
    dat <- do.call(tcplLoadData, args = c(lvl = 4L, prs))
  } else {
    dat <- do.call(tcplLoadData, args = c(lvl = 5L, prs))
  }
  
  if (nrow(dat) == 0) stop("No data for AEID", aeid)
  
  if (!is.null(clib)) {
    csub <- tcplLoadClib(field = "clib", val = clib)
    dat <- dat[spid %in% tcplLoadChem(field = "chid", val = csub$chid)$spid]
  }
  
  prs <- list(type = "mc", fld = "m4id", val = dat[ , unique(m4id)])
  
  agg <- do.call(tcplLoadData, args = c(lvl = "agg", prs))
  flg <- if (lvl < 6L) NULL else do.call(tcplLoadData, args = c(lvl = 6L, prs))
  
  if (is.null(fname)) {
    fname <- file.path(odir,
                       paste(paste0("AEID", aeid),
                             paste0("L", lvl),
                             tcplLoadAeid("aeid", aeid)$aenm,
                             format(Sys.Date(), "%y%m%d.pdf"),
                             sep = "_"))
  }
  
  graphics.off()
  pdf(file = fname, height = 6, width = 10, pointsize = 10)
  tcplPlotFits(dat, agg, flg, ordr.fitc = ordr.fitc)
  graphics.off()
  
  cat(fname, "complete.")
  
  TRUE
  
}

#-------------------------------------------------------------------------------
