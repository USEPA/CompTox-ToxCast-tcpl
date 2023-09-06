#-------------------------------------------------------------------------------
# mc1: Perform level 1 processing
#-------------------------------------------------------------------------------

#' @template proclvl
#' @templateVar LVL 1
#' @templateVar type mc
#' 
#' @param ac Integer of length 1, assay component id (acid) for processing.
#' @param wr Logical, whether the processed data should be written to the tcpl
#' database
#' 
#' @details
#' Level 1 processing includes defining the concentration and replicate index, 
#' cndx and repi, respectively.
#' 
#' @import data.table

mc1 <- function(ac, wr = FALSE) {
  
  ## Variable-binding to pass R CMD Check
  conc <- wllt <- acid <- apid <- spid <- n <- rpid <- srcf <- NULL
  cndx <- repi <- NULL
  
  owarn <- getOption("warn")
  options(warn = 1)
  on.exit(options(warn = owarn))
  
  ## Check the ac input
  if (length(ac) > 1) {
    warning("ac must be of length 1. Level 1 processing incomplete; no ",
            "updates\n  made to the mc1 table for ACIDS ", 
            paste(ac, collapse = ", "), ".")
    if(wr) return(FALSE) else return(list(FALSE, NULL))
  }
  
  stime <- Sys.time()
  
  ## Load level 0 data
  dat <- tcplLoadData(lvl = 0L, type = "mc", fld = "acid", val = ac)
  
  ## Check if any level 0 data was loaded
  if (nrow(dat) == 0) {
    warning("No level 0 data for ACID", ac, ". Level 1 processing incomplete;",
            " no updates\n  made to the mc1 table for ACID", ac, ".")
    if(wr) return(FALSE) else return(list(FALSE, NULL))
  }
  
  ttime <- round(difftime(Sys.time(), stime, units = "sec"), 2)
  ttime <- paste(unclass(ttime), units(ttime))
  
  cat("Loaded L0 ACID", ac, " (", nrow(dat), " rows; ", ttime,")\n", sep = "")
  
  stime <- Sys.time()
  
  ## Set conc to three significant figures
  dat[ , conc := signif(conc, 3)]
  
  # set concentration index
  dat[, cndx := frank(conc, ties.method = "dense"), by = .(acid, spid, wllt, srcf, apid)]
  
  # set replicate index
  dat[, repi:=rowid(conc),by= .(acid, spid, wllt, srcf, apid)]
  
  ttime <- round(difftime(Sys.time(), stime, units = "sec"), 2)
  ttime <- paste(unclass(ttime), units(ttime))
  cat("Processed L1 ACID", ac, " (", nrow(dat), 
      " rows; ", ttime, ")\n", sep = "")
  
  res <- TRUE
  
  outcols <- c("m0id", "acid", "cndx", "repi")
  dat <- dat[ , .SD, .SDcols = outcols]
  
  ## Load into mc1 table -- else return results
  if (wr) {
    stime <- Sys.time()
    tcplWriteData(dat = dat, lvl = 1L, type = "mc")
    
    ttime <- round(difftime(Sys.time(), stime, units = "sec"), 2)
    ttime <- paste(unclass(ttime), units(ttime))
    cat("Wrote L1 ACID", ac, " (", nrow(dat), 
        " rows; ", ttime, ")\n", sep = "")
  } else {
    res <- c(res, list(dat))
  }
  
  return(res)
  
}

#-------------------------------------------------------------------------------
