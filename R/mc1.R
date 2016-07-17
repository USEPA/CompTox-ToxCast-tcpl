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
  
  ## Define replicate id
  # Order by the following columns
  setkeyv(dat, c('acid', 'srcf', 'apid', 'coli', 'rowi', 'spid', 'conc')) 
  # Define rpid column for test compound wells
  nconc <- dat[wllt == "t" , 
               list(n = lu(conc)), 
               by = list(acid, apid, spid)][ , list(nconc = min(n)), by = acid]
  dat[wllt == "t" & acid %in% nconc[nconc > 1, acid],
      rpid := paste(acid, spid, wllt, srcf, apid, "rep1", conc, sep = "_")]
  dat[wllt == "t" & acid %in% nconc[nconc == 1, acid],
      rpid := paste(acid, spid, wllt, srcf, "rep1", conc, sep = "_")]
  # Define rpid column for non-test compound wells
  dat[wllt != "t", 
      rpid := paste(acid, spid, wllt, srcf, apid, "rep1", conc, sep = "_")] 
  # Increment rpid 
  dat_rpid <- dat[ , rpid]
  j = 2L
  while (any(duplicated(dat_rpid))) {
    ind <- duplicated(dat_rpid)
    dat_rpid[ind] <- sub("_rep[0-9]+", paste0("_rep", j), dat_rpid[ind])
    j <- j + 1
  }
  dat[ , rpid := dat_rpid]
  rm(dat_rpid)
  # Remove conc values from rpid
  dat[ , rpid := sub("_([^_]+)$", "", rpid, useBytes = TRUE)]
  
  ## Define concentration index
  indexfunc <- function(x) as.integer(rank(unique(x))[match(x, unique(x))])
  dat[ , cndx := indexfunc(conc), by = list(rpid)]
  
  ## Define replicate index
  # Create temporary table containing the unique replicate ids
  trdt <- unique(dat[wllt %in% c("t", "c") , list(acid, spid, wllt, rpid)])
  trdt_rpid <- trdt[ , rpid]
  trdt[ , rpid := NULL]
  trdt[ , repi := 1]
  # Increment repi
  while (any(duplicated(trdt))) {
    trdt[duplicated(trdt), repi := repi + 1]
  }
  trdt[ , rpid := trdt_rpid]
  rm(trdt_rpid)
  # Map replicate index back to dat
  setkey(dat, rpid)
  setkey(trdt, rpid)
  dat[ , repi := trdt[dat, repi]]
  
  ## Remove rpid column
  dat[, rpid := NULL]
  
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
