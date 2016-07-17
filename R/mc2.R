#-------------------------------------------------------------------------------
# mc2: Perform level 2 multiple-concentration processing
#-------------------------------------------------------------------------------

#' @template proclvl
#' @templateVar LVL 2
#' @templateVar type mc
#'
#' @inheritParams mc1
#'
#' @details
#' Level 2 multiple-concentration processing includes defining the
#' corrected value, cval, based on the correction methods listed in the
#' mc2_acid and mc2_methods tables.
#'
#' @seealso \code{\link{Method functions}}, \code{\link{MC2_Methods}}
#'
#' @import data.table

mc2 <- function(ac, wr = FALSE) {

  ## Variable-binding to pass R CMD Check
  cval <- rval <- acid <- wllq <- mthd <- NULL
  
  owarn <- getOption("warn")
  options(warn = 1)
  on.exit(options(warn = owarn))

  ## Check the ac input
  if (length(ac) > 1) {
    warning("ac must be of length 1. Level 2 processing incomplete; no ",
            "updates\n  made to the mc2 table for ACIDS ",
            paste(ac, collapse = ", "), ".")
    if(wr) return(FALSE) else return(list(FALSE, NULL))
  }

  stime <- Sys.time()

  ## Load level 1 data
  dat <- tcplLoadData(lvl = 1L, type = "mc", fld = "acid", val = ac)

  ## Check if any level 1 data was loaded
  if (nrow(dat) == 0) {
    warning("No level 1 data for ACID", ac, ". Level 2 processing incomplete;",
            " no updates\n  made to the mc2 table for ACID", ac, ".")
    if(wr) return(FALSE) else return(list(FALSE, NULL))
  }

  ttime <- round(difftime(Sys.time(), stime, units = "sec"), 2)
  ttime <- paste(unclass(ttime), units(ttime))

  cat("Loaded L1 ACID", ac, " (", nrow(dat), " rows; ", ttime,")\n", sep = "")

  stime <- Sys.time()

  ## Add corrected value column
  dat[ , cval := rval]
  setkey(dat, acid)

  ## Set all wllq to 0 for all NA cvals
  dat[is.na(cval), wllq := 0]

  ## Remove data with well quality of 0
  dat <- dat[wllq == 1]

  ## Load correction methods
  ms <- tcplMthdLoad(lvl = 2L, id = ac, type = "mc")
  if (nrow(ms) == 0) {
    warning("ACID", ac, " not listed in the mc2_acid table. Level 2 ",
            "processing\n  incomplete; no updates made to the mc2 table for ",
            "ACID", ac, ".")
    if(wr) return(FALSE) else return(list(FALSE, NULL))
  }

  ## Load the functions to generate correction expressions
  mthd_funcs <- mc2_mthds()

  ## Apply the correction methods
  if (ms[mthd != "none", lu(mthd)] > 0) {
    mthd_funcs <- mthd_funcs[ms[mthd != "none", mthd]]
    exprs <- lapply(mthd_funcs, do.call, args = list())
    fenv <- environment()
    invisible(rapply(exprs, eval, envir = fenv))
  }

  ## Remove data with well quality of 0 after correction methods
  dat <- dat[wllq == 1]

  ## Check for infinite cval values
  if (dat[ , lw(is.infinite(cval))] > 0) {
    warning("ACID", ac, " contains infinite values in the cval column. Level",
            "2 processing incomplete; no updates\n  made to the mc2 table ",
            "for ACID", ac, ".")
    if(wr) return(FALSE) else return(list(FALSE, NULL))
  }

  ttime <- round(difftime(Sys.time(), stime, units = "sec"), 2)
  ttime <- paste(unclass(ttime), units(ttime))
  cat("Processed L2 ACID", ac, " (", nrow(dat),
      " rows; ", ttime, ")\n", sep = "")

  res <- TRUE

  outcols <- c("m0id", "m1id", "acid", "cval")
  dat <- dat[ , .SD, .SDcols = outcols]

  ## Load into mc2 table -- else return results
  if (wr) {
    stime <- Sys.time()
    tcplWriteData(dat = dat, lvl = 2L, type = "mc")

    ttime <- round(difftime(Sys.time(), stime, units = "sec"), 2)
    ttime <- paste(unclass(ttime), units(ttime))
    cat("Wrote L2 ACID", ac, " (", nrow(dat),
        " rows; ", ttime, ")\n", sep = "")
  } else {
    res <- c(res, list(dat))
  }

  return(res)

}

#-------------------------------------------------------------------------------
