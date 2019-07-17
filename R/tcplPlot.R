#'#-------------------------------------------------------------------------------
# tcplPlot: plot tcpl data
#-------------------------------------------------------------------------------

#'  Generic Plotting Function for tcpl
#'
#' @description
#' \code{tcplLoadData} queries the tcpl databases and returns a plot
#' for the given level and data type.
#'
#' @param lvl Integer of length 1, the level of data to load
#' @param type Character of length 1, the data type, "sc" or "mc"
#' @param fld Character, the field(s) to query on
#' @param val List, vectors of values for each field to query on. Must be in
#' the same order as 'fld'.
#'
#' @details
#' The data type can be either 'mc' for mutliple concentration data, or 'sc'
#' for single concentration data. Multiple concentration data will be loaded
#' into the 'mc' tables, whereas the single concentration will be loaded into
#' the 'sc' tables.
#'
#' Setting 'lvl' to "agg" will return an aggregate table containing the m4id
#' with the concentration-response data and m3id to map back to well-level
#' information.
#'
#' Leaving \code{fld} NULL will return all data.
#'
#' @return
#' @export
#'
#' @examples
tcplPlot <- function(lvl = 4, fld = NULL, val = NULL, type = "mc", output = NULL) {
  
  if (length(lvl) > 1 | !lvl %in% 4:7) stop("invalid lvl input.")
  
  prs <- list(type = "mc", fld = fld, val = val)
  
  if (lvl == 4L) dat <- do.call(tcplLoadData, args = c(lvl = 4L, prs))
  if (lvl >= 5L) dat <- do.call(tcplLoadData, args = c(lvl = 5L, prs))
  if (lvl >= 6L) {
    flg <- do.call(tcplLoadData, args = c(lvl = 6L, prs))
  } else {
    flg <- NULL
  }
  if (lvl == 7L) {
    boot <- do.call(tcplLoadData, args = c(lvl = 7L, prs))
  } else {
    boot <- NULL
  }
  
  if (nrow(dat) == 0) stop("No data for fld/val provided ", m4id)
  
  agg <- do.call(tcplLoadData, args = c(lvl = "agg", prs))
  
  if(nrow(dat) == 1 & is.null(output)){
  tcplPlotFits(dat = dat, agg = agg, flg = flg, boot = boot)
  }
  if (nrow(dat) > 1 & is.null(output)) stop("More than 1 concentration series returned for given field/val combination.  Set output to PDF or reduce the number of curves to 1. Current number of curves: ", nrow(dat))
  
  
}