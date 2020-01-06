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
#' @param output how should the output be presnted
#' @param multi Boolean, if multi is TRUE output 6 plots per page
#' @param fileprefix prefix of filename
#' @param by Paramater to divide files into e.g. aeid
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
#' @import data.table
#' @export
#'
#' @examples
#' ## Store the current config settings, so they can be reloaded at the end 
#' ## of the examples
#' conf_store <- tcplConfList()
#' tcplConfExample()
#' 
#' tcplPlot(lvl = 4, fld = "m4id", val = c(18609966))## Create a level 4 plot
#' 
#' ## Reset configuration
#' options(conf_store)
#' 
#' 
tcplPlot <- function(lvl = 4, fld = NULL, val = NULL, type = "mc", by = NULL, output = c("console","pdf"), fileprefix = paste0("tcplPlot_",Sys.Date()), multi = FALSE) {
  
  if (length(lvl) > 1 | !lvl %in% 4:7) stop("invalid lvl input.")
  if (length(output) > 1) output <- output[1]
  
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
  
  if (nrow(dat) == 0) stop("No data for fld/val provided")
  
  agg <- do.call(tcplLoadData, args = c(lvl = "agg", prs))
  
  if(nrow(dat) == 1 & output=="console"){
  tcplPlotFits(dat = dat, agg = agg, flg = flg, boot = boot)
  }
  if (nrow(dat) > 1 & output=="console") stop("More than 1 concentration series returned for given field/val combination.  Set output to pdf or reduce the number of curves to 1. Current number of curves: ", nrow(dat))
  
  
  if(is.null(by)){
  if(output == "pdf" & !multi){
    graphics.off()
    pdf(
      file = file.path(
        getwd(),
        paste0(fileprefix,".",output)
      ),
      height = 6,
      width = 10,
      pointsize = 10
    )
    tcplPlotFits(dat = dat, agg = agg, flg = flg, boot = boot)
    graphics.off()
  }
  # plotting if using multiplot function
  hitc.all = TRUE
  #browser()
  if(multi){
    graphics.off()
    pdf(file = file.path(getwd(),paste0(fileprefix,".",output)), height = 10, width = 6, pointsize = 10)
    par(mfrow=c(3,2))
    tcplMultiplot(dat = dat, agg = agg, flg = flg, boot = boot, hitc.all = hitc.all)
    graphics.off()
  }
  }else{
    if(!by %in% names(dat)) stop("grouping variable unavailable.")
    subset <- unlist(unique(dat[,by, with = FALSE]))
    for(s in subset){
      if(output == "pdf" & !multi){
        graphics.off()
        pdf(
          file = file.path(
            getwd(),
            paste0(fileprefix,"_",by,"_",s,".",output)
          ),
          height = 6,
          width = 10,
          pointsize = 10
        )
        tcplPlotFits(dat = dat[get(by) == s], agg = agg, flg = flg, boot = boot)
        graphics.off()
      }
      # plotting if using multiplot function
      hitc.all = TRUE
      #browser()
      if(multi){
        graphics.off()
        pdf(file = file.path(getwd(),paste0(fileprefix,"_",by,"_",s,".",output)), height = 10, width = 6, pointsize = 10)
        par(mfrow=c(3,2))
        tcplMultiplot(dat = dat[get(by) == s], agg = agg, flg = flg, boot = boot, hitc.all = hitc.all)
        graphics.off()
      }
      
    }
    
  }
  
  
  
  
}