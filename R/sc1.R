#-------------------------------------------------------------------------------
# sc1: Perform level 1 single-concentration processing
#-------------------------------------------------------------------------------

#' @template proclvl
#' @templateVar LVL 1
#' @templateVar type sc
#' 
#' @param ac Integer of length 1, assay component id (acid) for processing.
#' @param wr Logical, whether the processed data should be written to the tcpl
#' database
#'  
#' @details
#' Level 1 single-concentration processing includes mapping assay component 
#' to assay endpoint, duplicating the data when the assay component has 
#' multiple assay endpoints, and any normalization of the data. Data 
#' normalization based on methods listed in sc1_aeid and sc1_methods tables.
#' 
#' @seealso \code{\link{Method functions}}, \code{\link{SC1_Methods}}
#' 
#' @import data.table

sc1 <- function(ac, wr = FALSE) {
  
  ## Variable binding to pass R CMD Check
  wllq <- conc <- logc <- acid <- aeid <- mthd <- ordr <- nassays <- NULL
  mthd <- aeid <- pval <- bval <- resp <- NULL
  
  owarn <- getOption("warn")
  options(warn = 1)
  on.exit(options(warn = owarn))
  
  ## Check the ac input
  if (length(ac) > 1) {
    warning("ac must be of length 1. Level 1 processing incomplete; no ",
            "updates\n  made to the sc1 table for ACIDS ",
            paste(ac, collapse = ", "), ".")
    if(wr) return(FALSE) else return(list(FALSE, NULL))
  }
  
  stime <- Sys.time()
  
  ## Load level 0 data
  dat <- tcplLoadData(lvl = 0L, type = "sc", fld = "acid", val = ac)
  
  ## Check if any level 0 data was loaded
  if (nrow(dat) == 0) {
    warning("No level 0 data for ACID", ac, ". Level 1 processing incomplete;",
            " no updates\n  made to the sc1 table for ACID", ac, ".")
    if(wr) return(FALSE) else return(list(FALSE, NULL))
  }
  
  ttime <- round(difftime(Sys.time(), stime, units = "sec"), 2)
  ttime <- paste(unclass(ttime), units(ttime))
  cat("Loaded L0 ACID", ac, " (", nrow(dat), " rows; ", ttime,")\n", sep = "")
  
  stime <- Sys.time()
  
  ## Remove data with well quality of 0
  dat <- dat[wllq == 1]
  
  ## Force all concentrations to 1 significant figure
  dat[ , conc := signif(conc, 1)]
  
  ## Add column for log10 concentration
  dat[ , logc := log10(conc)]
  
  ## Load aeid mapping information.
  aeid_info <- tcplLoadAeid("acid", ac)[ , list(acid, aeid)]
  setkey(aeid_info, acid)
  
  ## Check for acids for aeids
  if (nrow(aeid_info) == 0) {
    warning("No assay endpoint listed for ACID", ac, ". Level 1 processing ",
            "incomplete; no\n  updates made to the sc1 table for ACID", 
            ac, ".")
    if(wr) return(FALSE) else return(list(FALSE, NULL))
  }
  
  ## Merge dat with aeid_info to duplicate data for every aeid
  setkey(dat, acid)
  dat <- aeid_info[dat, allow.cartesian = TRUE]
  setkey(dat, aeid)
  
  ## Load normalization methods
  ms <- tcplMthdLoad(lvl = 1L, id = dat[ , unique(aeid)], type = "sc")
  ms <- ms[ , list(aeid, mthd, ordr)]
  
  ## Check for aeids for methods
  if (!all(dat[ , unique(aeid)] %in% ms[ , aeid])) {
    miss_aeid <- dat[ , unique(aeid)[!unique(aeid) %in% ms[ , aeid]]]
    warning("AEIDS(S) ", paste(miss_aeid, collapse = ", "), " (mapped to ACID", 
            ac, ") do not have aeid\n  methods listed in the sc1_aeid table. ",
            "Level 1 processing incomplete; no updates\n  made to the sc1 ",
            "table for ACID", ac, ".")
    if(wr) return(FALSE) else return(list(FALSE, NULL))
  }
  
  ## Load the functions to generate normalization expressions
  mthd_funcs <- sc1_mthds()
  
  ## Reshape ms
  ms <- setkey(ms, ordr)
  ms <- ms[ , lapply(.SD, list), by = list(mthd, ordr)]
  ms[ , nassays := unlist(lapply(aeid, length))]
  
  ## Initialize the bval, pval, and resp columns
  dat[ , c('bval', 'pval', 'resp') := NA_real_]
  
  ## Apply the normalization methods
  exprs <- lapply(1:nrow(ms),
                  function(x) {
                    do.call(mthd_funcs[[ms[x, mthd]]], 
                            list(aeids = ms[ , aeid][[x]]))
                  })
  fenv <- environment()
  invisible(rapply(exprs, eval, envir = fenv))
  
  ## Check for infinite pval or bval values
  if (dat[ , lw(is.infinite(pval))] > 0 | dat[ , lw(is.infinite(bval))] > 0) {
    in_aeid <- dat[is.infinite(pval) | is.infinite(bval), unique(aeid)]
    warning("AEID(S) ", paste(in_aeid, collapse = ", "), " (mapped to ACID",
            ac, ") contain infinite values in the bval or pval column. Level ",
            "3 processing incomplete; no updates\n  made to the mc3 table ",
            "for ACID", ac, ".")
    if(wr) return(FALSE) else return(list(FALSE, NULL))
  }
  
  ## Check for NA response values
  if (dat[ , lw(is.na(resp))] > 0) {
    na_aeid <- dat[lw(is.na(resp)) > 0, unique(aeid)]
    warning("AEID(S) ", paste(na_aeid, collapse = ", "), " (mapped to ACID",
            ac, ") contain NA in the response column. Level 1 processing ",
            "incomplete; no updates\n  made to the sc1 table for ACID", ac, ".")
    if(wr) return(FALSE) else return(list(FALSE, NULL))
  }
  
  ttime <- round(difftime(Sys.time(), stime, units = "sec"), 2)
  ttime <- paste(unclass(ttime), units(ttime))
  cat("Processed L1 ACID", ac, " (", nrow(dat), 
      " rows; ", ttime, ")\n", sep = "")
      
  res <- TRUE
  
  outcols <- c("s0id", "acid", "aeid", "logc", "bval", "pval", "resp")
  dat <- dat[ , .SD, .SDcols = outcols]
  
  ## Load into sc1 table -- else return results
  if (wr) {
    stime <- Sys.time()
    tcplWriteData(dat = dat, lvl = 1L, type = "sc")
    
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
