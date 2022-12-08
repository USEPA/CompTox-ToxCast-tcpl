#-------------------------------------------------------------------------------
# mc6: Perform level 6 multiple-concentration processing
#-------------------------------------------------------------------------------

#' @template proclvl
#' @templateVar LVL 6
#' @templateVar type mc
#' 
#' @inheritParams mc4
#' 
#' @details
#' Level 6 multiple-concentration flagging uses both the plate level 
#' concentration-response data and the modeled parameters to flag potential 
#' false positives and false negative results.
#' 
#' @seealso \code{\link{Method functions}}, \code{\link{MC6_Methods}}
#'
#' @import data.table

mc6 <- function(ae, wr = FALSE) {
  
  ## Variable-binding to pass R CMD Check
  mthd_id <- m4id <- m5id <- lval <- rval <- J <- mthd <- bmad <- cell_viability_assay <- NULL
  
  #owarn <- getOption("warn")
  #options(warn = 1)
  #on.exit(options(warn = owarn))
  
  ## Check the ae input
  if (length(ae) > 1) {
    warning("ae must be of length 1. Level 6 processing incomplete; no updates",
            "\n  made to the mc6 table for AEIDS ", 
            paste(ae, collapse = ", "), ".")
    if(wr) return(FALSE) else return(list(FALSE, NULL))
  }
  
  stime <- Sys.time()
  
  ## Load the mc6 flags
  ms <- tcplMthdLoad(lvl = 6L, id = ae, type = "mc")
  if (nrow(ms) == 0) {
    warning("No level 6 methods assigned to AEID", ae, ".")
    if(wr) return(TRUE) else return(list(TRUE, NULL))
  }
  # TODO check that any methods that are included in ms are only present once per 
  
  
  setkey(ms, mthd_id)
  
  ## Load level 5 and, if needed, level 3 data 
  ft <- tcplLoadData(lvl = 5L, type = "mc", fld = "aeid", val = ae, add.fld = TRUE)
  setkey(ft, m4id)
  if (any(ms$nddr)) {
    dr <- .load6DR(ae)
    setkey(dr, m4id, aeid, spid)
    setkey(ft, m4id, aeid, spid)
    #dr <- dr[ft[ , list(m4id, m5id)]]
    #dr[ , bmad := unique(ft$bmad)]
    #unique_ids <- unique(c(dr[ , m4id], ft[ , m4id]))
    ft <- dr[ft]
  } 
  
  ## Check if any level 5 data was loaded
  if (nrow(ft) == 0) {
    warning("No level 5 data for AEID", ae, ". Level 6 processing incomplete;",
            " no updates\n  made to the mc6 table for AEID", ae, ".")
    if(wr) return(FALSE) else return(list(FALSE, NULL))
  }
  
  ## Load assay information to check for cell viability. Fill with 0 if unavailable.
  tbl <- "assay_component_endpoint"
  drvr = getOption("TCPL_DRVR")
  if (drvr == "MySQL") {
    flds <- tcplQuery(paste0("DESCRIBE ", tbl, ";"))$Field 
  } else if (drvr == "tcplLite") {
    flds <- tcplQuery(paste0("PRAGMA table_info(", tbl, ");"),tbl=c(tbl))$name
  }
  
  if("cell_viability_assay" %in% flds){
    cell_viability <- tcplLoadAeid(fld = "aeid",val = ae,add.fld = "cell_viability_assay")
    ft <- cell_viability[ft, on = "aeid"]
    setkey(ft,m4id)
  } else{
    ft[,cell_viability_assay:=0]
  }
  
  ttime <- round(difftime(Sys.time(), stime, units = "sec"), 2)
  ttime <- paste(unclass(ttime), units(ttime))
  
  cat("Loaded L5 AEID", ae, " (", nrow(ft), " rows; ", ttime,")\n", sep = "")
  
  stime <- Sys.time()
  
  ## Initialize f, the list of data.tables containing the flag information
  f <- vector(mode = "list", length = max(ms$mthd_id))
  
  browser()

  ## Generate and evaluate flag expressions
  mthd_funcs <- mc6_mthds()
  exprs <- lapply(ms$mthd_id, function(x) mthd_funcs[[ms[J(x), mthd]]](x)) # XXX this breaks if functions appear more than once in mc6_mthds() 
  fenv <- environment()
  invisible(rapply(exprs, eval, envir = fenv))
  
  f <- rbindlist(f)
  
  ttime <- round(difftime(Sys.time(), stime, units = "sec"), 2)
  ttime <- paste(unclass(ttime), units(ttime))
  cat("Processed L6 AEID", ae, " (", nrow(ft), 
      " rows; ", ttime, ")\n", sep = "")
  
  res <- TRUE
  
  ## Load into mc6 table -- else return results
  if (wr) {
    
    stime <- Sys.time()
    
    if (nrow(f) == 0) {
      ttime <- round(difftime(Sys.time(), stime, units = "sec"), 2)
      ttime <- paste(unclass(ttime), units(ttime))
      cat("No flags to write for AEID", ae, sep = "")
      return(TRUE)
    }
    
    tcplWriteData(dat = f, lvl = 6L, type = "mc")
    
    ttime <- round(difftime(Sys.time(), stime, units = "sec"), 2)
    ttime <- paste(unclass(ttime), units(ttime))
    cat("Wrote L6 AEID", ae, " (", nrow(f), " rows; ", ttime, ")\n", sep = "")
  } else {
    res <- c(list(res), list(f))
  }
  
  return(res)
  
}

#-------------------------------------------------------------------------------