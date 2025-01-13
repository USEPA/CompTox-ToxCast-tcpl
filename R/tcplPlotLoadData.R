#-------------------------------------------------------------------------------
# tcplPlotLoadData: Utility function to load data for tcplPlot
#-------------------------------------------------------------------------------

#' @title Utility function to load data for tcplPlot
#'
#' @description
#' \code{tcplPlotLoadData} queries the tcpl databases and returns a data.table
#' with data for the given field, value, level, and data type prepared in a
#' format tcplPlot can use to generate plots.
#'
#' @param type Character of length 1, the data type, "sc" or "mc"
#' @param fld Character, the field(s) to query on.
#' @param val List, vectors of values for each field to query on. Must be in
#' the same order as 'fld'.
#' @param flags Boolean, by default FALSE. If TRUE, level 6 flags are loaded
#' for use in tcplPlot. Must be set to TRUE if tcplPlot 'flags' also is/will be
#' set to TRUE
#' 
#' @details
#' This utility function is used by \code{tcplPlot} to load and prepare data from 
#' \code{tcplLoadData} for use in generating plots. It is exported for use in 
#' advanced comparison plots where users create plots using multiple data sources. 
#' After saving the response from \code{tcplPlotLoadData}, switch data source
#' config and pass the data to \code{tcplPlot} \code{dat} parameter.
#' 
#' The data \code{type} can be either 'mc' for multiple concentration data, or
#' 'sc' for single concentration data. 
#'
#' @examples
#' \dontrun{
#' ## load mc plot data for an entire endpoint
#' dat <- tcplPlotLoadData(fld = "aeid", val = 703)
#' 
#' ## load sc plot data for an entire endpoint
#' dat <- tcplPlotLoadData(type = "sc", fld = "aeid", val = 703)
#' 
#' ## load plot data for two endpoint-samples and include loading of flags
#' ## flags must equal TRUE if tcplPlot will/does
#' dat <- tcplPlotLoadData(fld = c("spid", "aeid"), 
#'                         val = list(c("TP0000269F11", "TP0000395A09"),703),
#'                         flags = TRUE)
#'                         
#' ## if desired, switch connections
#' tcplConf()
#' 
#' ## use dat in tcplPlot
#' tcplPlot(dat = dat, 
#'          fld = c("spid", "aeid"), 
#'          val = list(c("TP0000269F11", "TP0000395A09"),703), 
#'          compare.val = list(c("LEGTV002B01", "LEGTV003A06"),703),
#'          output = "pdf", 
#'          flags = TRUE,
#'          fileprefix="example")
#' }
#' 
#' @return A data.table containing plot-ready data for the given fields.
#'
#' @seealso \code{\link{tcplPlot}}
#'
#' @import data.table
#' @importFrom dplyr mutate group_by summarize rowwise
#' @importFrom stringr str_count
#' @export
tcplPlotLoadData <- function(type = "mc", fld = "m4id", val, flags = FALSE){
  #variable binding
  lvl <- m4id <- conc <- resp <- conc_unit <- NULL
  
  # Validate vars based on some assumed properties
  validated_vars <- tcplPlotValidate(type = type,flags = flags)
  # take list of validated vars and add them to the function's environment
  list2env(validated_vars, envir = environment())
  
  # check that input combination is unique
  dat <- tcplLoadData(lvl = lvl, fld = fld, val = val, type = type)
  if (nrow(dat) == 0) stop("No data for fld/val provided")
  
  # set order to given order
  dat <- dat[order(match(get(fld[1]), if(is.list(val)) val[[1]] else val))]
  if (getOption("TCPL_DRVR") == "API" && tolower(fld) == "aeid") {
    dat <- dat %>% arrange(m4id)
  }
  dat$order <- 1:nrow(dat)
  
  mcLoadDat <- function(m4id = NULL,flags) {
    l4 <- tcplLoadData(lvl = 4, fld = "m4id", val = m4id, add.fld = T)
    dat <- l4[dat, on = "m4id"]
    if (flags == TRUE) {
      l6 <- tcplLoadData(lvl=6, fld='m4id', val=m4id, type='mc')
      if (nrow(l6) > 0) {
        l6 <- l6[ , .( flag = paste(flag, collapse=";\n")), by = m4id]
        no_flags <- setdiff(m4id, l6$m4id)
        if (length(no_flags) > 0) {
          l6 <- rbindlist(list(l6, data.table("m4id" = no_flags, "flag" = "None")))
        } 
      } else {
        l6 <- data.table(m4id, "flag" = "None")
      }
      dat <- dat[l6, on = "m4id"]
      dat$flag_count <- 0
      dat[flag != "None", flag_count := stringr::str_count(flag, "\n") + 1]
    }
    dat
  }
  
  # load dat
  if (getOption("TCPL_DRVR") != "API") {
    if (type == "mc") {
      dat <- mcLoadDat(dat$m4id,flags = flags)
      agg <- tcplLoadData(lvl = "agg", fld = "m4id", val = dat$m4id)
    } else { # type == 'sc'
      agg <- tcplLoadData(lvl = "agg", fld = "s2id", val = dat$s2id, type = "sc")
    }
    
    # unlog concs
    if (!("conc" %in% colnames(agg))) agg <- mutate(agg, conc = 10^logc)
    
    #determine if we're single conc or multiconc based on dat
    join_condition <- c("m4id","s2id")[c("m4id","s2id") %in% colnames(dat)]
    conc_resp_table <- agg %>% group_by(.data[[join_condition]]) %>% summarize(conc = list(conc), resp = list(resp)) %>% as.data.table()
    dat <- dat[conc_resp_table, on = join_condition]
    
    # get chemical and sample information
    dat <- tcplPrepOtpt(dat)
    
    # determine missing chem info and replace with string description of well type(s)
    dat <- tcplPlotLoadWllt(dat, type)
    
  } else {
    # fix flags from API for plotting
    if (flags == TRUE) {
      if (is.null(dat$flag)) {
        flag <- NA
      }
      dat <- dat %>% rowwise() %>% mutate(flag = ifelse(is.na(flag[1]) || flag[1] == "NULL" || is.null(flag[1]), "None", paste(flag, collapse = ';\n'))) %>% ungroup() %>% as.data.table()
    }
    dat$conc_unit <- dat$tested_conc_unit
  }
  
  # add normalized data type for y axis
  ndt <- tcplLoadAeid(fld = "aeid", val = dat$aeid, add.fld = "normalized_data_type")
  dat <- dat[ndt, on = "aeid"]
  
  # correct concentration unit label for x-axis
  dat <- dat[is.na(conc_unit), conc_unit:="\u03BCM"]
  dat <- dat[conc_unit=="uM", conc_unit:="\u03BCM"]
  dat <- dat[conc_unit=="mg/l", conc_unit:="mg/L"]
  
  # replace null bmd in dat table
  dat <- dat[is.null(dat$bmd), bmd:=NA]
  
  #replace null top with 0
  dat[is.null(dat$top), top := 0]
  dat[is.na(top), top := 0]
  
  # correct bmr and coff direction
  if (type == "mc") {
    # if top if less than 0, flip bmr no matter what
    dat[top < 0, bmr := bmr * -1]
    # if model type is loss, flip cut off
    dat[model_type == 4, coff := coff * -1]
    # if model type is bidirectional, flip cut off if top is less than 0
    dat[model_type == 2 & top < 0, coff := coff * -1]
  } else { # sc
    # if max median is less than 0, flip cut off to align with it
    dat[max_med < 0, coff := coff * -1]
    # if hitc is less than 0, max median is in the opposite of intended direction, 
    # so flip cut off (possibly again)
    dat[max_med < 0, coff := coff * -1]
  }
  
  dat
}