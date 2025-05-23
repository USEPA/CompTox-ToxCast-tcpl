#-------------------------------------------------------------------------------
# tcplPlotM4ID: Plot dose-response by m4id
#-------------------------------------------------------------------------------

#' @title Plot fit summary plot by m4id
#' 
#' @description
#' \code{tcplPlotM4ID} creates a summary plots for the given m4id(s) by loading
#' the appropriate data from the tcpl databases and sending it to 
#' \code{\link{tcplPlotFits}}
#' 
#' @param m4id Integer, m4id(s) to plot
#' @param lvl Integer, the level of data to plot
#' 
#' @details 
#' A level 4 plot ('lvl' = 4) will plot the concentration series and the 
#' applicable curves, without an indication of the activity call or the 
#' winning model. Level 4 plots can be created without having done subsequent
#' processing.
#' 
#' Level 5 plots include the level 4 information with the activity call and 
#' model selection. The winning model will be highlighted red in the side panel
#' containing the summary statistics. Level 6 plots, in addition the all of the 
#' level 4 and 5 information, include the positive flag IDs. If the flag has 
#' an associated value, the value will be in parentheses following the flag ID.
#' Level 7 plots in addition to all of the level 4, 5, and 6 information, include
#' the AC50 confidence interval and hit percentage information from bootstrapping.
#' 
#' @examples 
#' \dontrun{
#' tcplPlotM4ID(m4id = 18609966, lvl = 4) ## Create a level 4 plot
#' tcplPlotM4ID(m4id = 18609966, lvl = 5) ## Create a level 5 plot
#' tcplPlotM4ID(m4id = 18609966, lvl = 6) ## Create a level 6 plot
#' }
#' 
#' @seealso \code{\link{tcplPlotFits}}, \code{\link{tcplMakeAeidPlts}}
#' @import data.table
#' @export


tcplPlotM4ID <- function(m4id, lvl = 4L) {
  
  if (check_tcpl_db_schema()) stop("This function is no longer supported in this
                                   version of invitrodb. Consider tcplPlot() instead.")
  
  if (length(lvl) > 1 | !lvl %in% 4:7) stop("invalid lvl input.")
  
  prs <- list(type = "mc", fld = "m4id", val = m4id)
  
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
  
  if (nrow(dat) == 0) stop("No data for m4id ", m4id)
  
  agg <- do.call(tcplLoadData, args = c(lvl = "agg", prs))
  
  tcplPlotFits(dat = dat, agg = agg, flg = flg, boot = boot)
  
}

#-------------------------------------------------------------------------------
