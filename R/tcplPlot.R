#' #-------------------------------------------------------------------------------
# tcplPlot: plot tcpl data
#-------------------------------------------------------------------------------

#'  Generic Plotting Function for tcpl
#'
#' @description
#' \code{tcplPlot} queries the tcpl databases and returns a plot
#' for the given level and data type.
#'
#' @param dat data.table containing plot-prepared data, used for stand-alone 
#' (non-ToxCast data like other tcplfit2-fit data) or advanced plotting 
#' (generating comparison plots across multiple database configurations) and not
#' required. See \code{tcplPlotLoadData}.
#' @param type Character of length 1, the data type, "sc" or "mc".
#' @param fld Character, the field(s) to query on.
#' @param val List, vectors of values for each field to query on. Must be in
#' the same order as 'fld'.
#' @param compare.val List, vectors of values for each field to query on to 
#' compare with val. Must be in the same order as 'fld'. Must have the same
#' length as val (1:1 comparison). Must be set to compare plots; otherwise leave
#' NULL
#' @param output How should the plot be presented. To work with the plot in 
#' environment, use "ggplot"; to interact with the plot in application, use 
#' "console"; or to save as a file type, use "pdf", "jpg", "png", "svg", or "tiff".
#' @param multi Boolean, by default TRUE for "pdf". If multi is TRUE, output
#' by  default 4 plots per page for 'verbose' = TRUE and 6 plots per page for
#' 'verbose' = FALSE.
#' @param fileprefix Prefix of file when saving.
#' @param by Parameter to divide files into e.g. "aeid".
#' @param verbose Boolean, by default FALSE. If TRUE, a table with fitting parameters
#'  is included with the plot.
#' @param nrow Integer, number of rows in multiplot. By default 2.
#' @param ncol Integer, number of columns in multiplot. By default 3, 2 if verbose.
#' @param dpi Integer, image print resolution. By default 600.
#' @param flags Boolean, by default FALSE. If TRUE, level 6 flags are displayed
#' below annotations on plot
#' @param yuniform Boolean, by default FALSE. If TRUE, all plots will have uniform
#' y axis scaling, automatically determined.
#' @param yrange Integer of length 2, for directly setting the y-axis range, 
#' c(<min>,<max>). By default, c(NA,NA).
#'
#' @details
#' The data type can be either 'mc' for multiple concentration data, or 'sc'
#' for single concentration data. Multiple concentration data will be loaded
#' into the 'mc' tables, whereas the single concentration will be loaded into
#' the 'sc' tables.
#'
#' Leaving \code{fld} NULL will return all data.
#' @import data.table
#' @importFrom gridExtra marrangeGrob
#' @importFrom ggplot2 ggsave
#' @importFrom dplyr %>% all_of pull
#' @importFrom grDevices pdf.options
#' @export
#'
#' @examples
#' \dontrun{
#' tcplPlot(fld = "m4id", val = c(18609966)) ## Create a level 4 plot
#' }
tcplPlot <- function(dat = NULL, type = "mc", fld = "m4id", val = NULL, compare.val = NULL, by = NULL, output = c("console", "pdf", "png", "jpg", "svg", "tiff"), fileprefix = paste0("tcplPlot_", Sys.Date()), multi = NULL, verbose = FALSE, nrow = NULL, ncol = NULL, dpi = 600, flags = FALSE, yuniform = FALSE, yrange=c(NA,NA)) {
  #variable binding
  conc_unit <- bmd <- resp <- compare.dat <- lvl <- compare <- NULL
  
  #set pdf options
  enc <- pdf.options()$encoding
  pdf.options(encoding="CP1253.enc")
  on.exit(pdf.options(encoding = enc))
  
  # Validate vars based on some assumed properties
  validated_vars <- tcplPlotValidate(type = type,flags = flags,output = output,multi = multi,verbose = verbose)
  # take list of validated vars and add them to the function's environment
  list2env(validated_vars, envir = environment())

  # check_tcpl_db_schema is a user-defined function found in v3_schema_functions.R file
  if (check_tcpl_db_schema() | !is.null(dat) | getOption("TCPL_DRVR") == "API") {
    # check if user supplied data.  If not, load from db connection
    if(is.null(dat)){
      dat <- tcplPlotLoadData(type = type, fld = fld, val = val, flags = flags) #code defined in tcplPlotUtils.R
    } 
    # if user supplies dat we still need to add compare indicator
    dat <- dat[,compare := FALSE]
    if(!is.null(compare.val)){
      compare.dat <- tcplPlotLoadData(type = type,fld = fld, val = compare.val, flags = flags)[,compare := TRUE] #code defined in tcplPlotUtils.R
      if (nrow(compare.dat) == 0) stop("No compare data for fld/val provided")
    }
    
    # join with given val/compare.val if lengths don't match
    if (!is.null(compare.val) && nrow(dat) + nrow(compare.dat) != length(val) + length(compare.val)) {
      val_dt <- as.data.table(val)
      colnames(val_dt) <- fld
      compare.val_dt <- as.data.table(compare.val)
      colnames(compare.val_dt) <- fld
      dat <- val_dt %>% inner_join(dat, by = fld)
      compare.dat <- compare.val_dt %>% inner_join(compare.dat, by = fld)
    } 
    
    # if you have compare data, join it back to main datatable
    if(!is.null(compare.dat)){
      # check that dat and compare.dat are the same length 
      if (nrow(dat) != nrow(compare.dat)) stop("'compare.val' must be of equal length to 'val'")
      dat <- rbind(dat,compare.dat, fill = TRUE)
    }
    
    # preserve user-given order
    setorder(dat, order)
    
    # set yrange from tcplPlotUtils.R
    yrange <- tcplPlotSetYRange(dat,yuniform,yrange,type)
    
    # check for null bmd in dat table
    if (verbose){
      dat <- dat[is.null(dat$bmd), bmd:=NA]
    }
    
    # assign nrow = ncol = 1 for output="pdf" and multi=FALSE to plot one plot per page
    if(nrow(dat) > 1 && output == "pdf" && multi == FALSE) {
      nrow = ncol = 1
    }
    # error message for output="console" and multi=FALSE to avoid multiple plots in console
    if(nrow(dat[compare == FALSE]) != 1 && output == "console" && multi == FALSE) stop("More than 1 concentration series returned for given field/val combination.  Set output to pdf or reduce the number of curves to 1. Current number of curves: ", nrow(dat))
    if(is.null(nrow)){
      nrow <- ifelse(verbose,2,2)
    }
    if(is.null(ncol)){
      ncol <- ifelse(!verbose | type == "sc",3,2)
    }
    
    
    
    
    if (nrow(dat[compare == FALSE]) == 1) {
      # plot single graph
      # this needs to be fixed to be more succinct about users selected option
      ifelse(output[1] == "console",
      # tcplPlotlyplot is the user-defined function found in tcplPlot.R file used to connect tcpl and plotly packages
      # tcplggplot is the user-defined function found in tcplPlot.R file used to connect tcpl and ggplot2 packages
        return(tcplPlotlyPlot(dat, lvl)),
        return(ggsave(filename=paste0(fileprefix,"_",paste0(ifelse(type=="mc",dat$m4id,dat$s2id), collapse = "_"),".",output),
                      plot= if(is.null(compare.val)) tcplggplot(dat,verbose = verbose, lvl = lvl, flags = flags, yrange = yrange) else tcplggplotCompare(dat[compare == FALSE],dat[compare == TRUE],verbose = verbose, lvl = lvl, flags = flags, yrange = yrange), width = 7, height = 5, dpi=dpi))
      )
    } else {
      split_dat <- list(dat)
      if(!is.null(by)){
        split_dat <- split(dat,f = factor(dat %>% pull(all_of(by))))
      }
      for(d in split_dat){
        if (is.null(compare.val)) {
          plot_list <- by(d,seq(nrow(d)),tcplggplot,verbose = verbose, lvl = lvl, flags = flags, yrange = yrange)
        } else {
          plot_list <- mapply(tcplggplotCompare, asplit(d[compare == FALSE],1), asplit(d[compare == TRUE],1), MoreArgs = list(verbose = verbose, lvl = lvl, flags = flags, yrange = yrange))
        }
        m1 <- marrangeGrob(plot_list, nrow = nrow, ncol = ncol)
        if(output=="pdf"){
          w <- ifelse(type == "mc", ncol*7, ncol*5)
          h <- ifelse(type == "mc", nrow*5, nrow*6)
          ggsave(paste0(fileprefix,ifelse(is.null(by),"",paste0("_",by,"_",d %>% pull(all_of(by)) %>% unique())), ".pdf"), m1,width = w, height = h)
        } else {
          names(plot_list) <- d$m4id
          w <- ifelse(type == "mc", 7, 4)
          h <- ifelse(type == "mc", 5, 6)
          lapply(names(plot_list), function(x)ggsave(filename=paste0(fileprefix,"_",x,".",output),
                                                     plot=arrangeGrob(grobs=plot_list[x]), width = 7, height = 5, dpi=dpi))
        }
      }
    }

  } else {
    tcplLegacyPlot()
  }
}
