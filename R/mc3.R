#-------------------------------------------------------------------------------
# mc3: Perform level 3 multiple-concentration processing
#-------------------------------------------------------------------------------

#' @template proclvl
#' @templateVar LVL 3
#' @templateVar type mc
#'
#' @inheritParams mc1
#'
#' @details
#' Level 3 multiple-concentration processing includes mapping assay component
#' to assay endpoint, duplicating the data when the assay component has
#' multiple assay endpoints, and any normalization of the data. Data
#' normalization based on methods listed in mc3_aeid and mc3_methods tables.
#'
#' @seealso \code{\link{Method functions}}, \code{\link{MC3_Methods}}
#'
#' @import data.table

mc3 <- function(ac, wr = FALSE) {

  ## Variable-binding to pass R CMD Check
  conc <- logc <- acid <- aeid <- mthd <- ordr <- nassays <- resp <- NULL
  pval <- bval <- NULL
  
  owarn <- getOption("warn")
  options(warn = 1)
  on.exit(options(warn = owarn))

  ## Check the ac input
  if (length(ac) > 1) {
    warning("ac must be of length 1. Level 3 processing incomplete; no ",
            "updates\n  made to the mc3 table for ACIDS ",
            paste(ac, collapse = ", "), ".")
    if(wr) return(FALSE) else return(list(FALSE, NULL))
  }

  stime <- Sys.time()

  ## Load level 2 data
  dat <- tcplLoadData(lvl = 2L, type = "mc", fld = "acid", val = ac)

  ## Check if any level 2 data was loaded
  if (nrow(dat) == 0) {
    warning("No level 2 data for ACID", ac, ". Level 3 processing incomplete;",
            " no updates\n  made to the mc3 table for ACID", ac, ".")
    if(wr) return(FALSE) else return(list(FALSE, NULL))
  }

  ttime <- round(difftime(Sys.time(), stime, units = "sec"), 2)
  ttime <- paste(unclass(ttime), units(ttime))

  cat("Loaded L2 ACID", ac, " (", nrow(dat), " rows; ", ttime,")\n", sep = "")

  stime <- Sys.time()

  ## Force all concentrations to 1 significant figure
  dat[ , conc := signif(conc, 1)]

  ## Add column for log10 concentration
  dat[ , logc := log10(conc)]

  ## Load aeid mapping information.
  aeid_info <- tcplLoadAeid("acid", ac)[ , list(acid, aeid)]
  setkey(aeid_info, acid)

  ## Check for acids for aeids
  if (nrow(aeid_info) == 0) {
    warning("No assay endpoint listed for ACID", ac, ". Level 3 processing ",
            "incomplete; no\n  updates made to the mc3 table for ACID",
            ac, ".")
    if(wr) return(FALSE) else return(list(FALSE, NULL))
  }

  ## Merge dat with aeid_info to duplicate data for every aeid
  setkey(dat, acid)
  dat <- aeid_info[dat, allow.cartesian = TRUE]
  setkey(dat, aeid)

  ## Load normalization methods
  ms <- tcplMthdLoad(lvl = 3L, id = dat[ , unique(aeid)], type = "mc")
  ms <- ms[ , list(aeid, mthd, ordr)]

  ## Check for aeids for methods
  if (!all(dat[ , unique(aeid)] %in% ms[ , aeid])) {
    miss_aeid <- dat[ , unique(aeid)[!unique(aeid) %in% ms[ , aeid]]]
    warning("AEIDS(S) ", paste(miss_aeid, collapse = ", "), " (mapped to ACID",
            ac, ") do not have aeid\n  methods listed in the mc3_aeid table. ",
            "Level 3 processing incomplete; no updates\n  made to the mc3 ",
            "table for ACID", ac, ".")
    if(wr) return(FALSE) else return(list(FALSE, NULL))
  }

  ## Reshape ms
  ms <- setkey(ms, ordr)
  ms <- ms[ , lapply(.SD, list), by = list(mthd, ordr)]
  ms[ , nassays := unlist(lapply(aeid, length))]

  ## Load the functions to generate normalization expressions
  mthd_funcs <- mc3_mthds()

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
    na_aeid <- dat[is.na(resp) > 0, unique(aeid)]
    warning("AEID(S) ", paste(na_aeid, collapse = ", "), " (mapped to ACID",
            ac, ") contain NA in the response column. Level 3 processing ",
            "incomplete; no updates\n  made to the mc3 table for ACID", ac, ".")
    if(wr) return(FALSE) else return(list(FALSE, NULL))
  }

  ttime <- round(difftime(Sys.time(), stime, units = "sec"), 2)
  ttime <- paste(unclass(ttime), units(ttime))
  cat("Processed L3 ACID", ac, " (AEIDS: ",
      paste(dat[ , unique(aeid)], collapse = ", "),
      "; ", nrow(dat), " rows; ", ttime, ")\n", sep = "")

  res <- TRUE

  outcols <- c("m0id", "m1id", "m2id", "acid", "aeid",
               "bval", "pval", "logc", "resp")
  dat <- dat[ , .SD, .SDcols = outcols]

  ## Load into mc3 table -- else return results
  if (wr) {
    stime <- Sys.time()
    tcplWriteData(dat = dat, lvl = 3L, type = "mc")

    ttime <- round(difftime(Sys.time(), stime, units = "sec"), 2)
    ttime <- paste(unclass(ttime), units(ttime))
    cat("Wrote L3 ACID", ac, " (AEIDS: ",
        paste(dat[ , unique(aeid)], collapse = ", "),
        "; ", nrow(dat), " rows; ", ttime, ")\n", sep = "")
  } else {
    res <- c(list(res), list(dat))
  }

  return(res)

}

#-------------------------------------------------------------------------------
