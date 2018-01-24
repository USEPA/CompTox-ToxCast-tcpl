#-------------------------------------------------------------------------------
# mc5: Perform level 5 multiple-concentration processing
#-------------------------------------------------------------------------------

#' @template proclvl
#' @templateVar LVL 5
#' @templateVar type mc
#' 
#' @inheritParams mc4
#' 
#' @details
#' Level 5 multiple-concentration hit-calling uses the fit parameters and the 
#' activity cutoff methods from mc5_aeid and mc5_methods to make an activity 
#' call and identify the winning model for each fit.
#' 
#' @seealso \code{\link{Method functions}}, \code{\link{MC5_Methods}}
#' 
#' @import data.table

mc5 <- function(ae, wr = FALSE) {
  
  ## Variable-binding to pass R CMD Check
  coff <- bmad <- adco <- maic <- cnst_aic <- hill_aic <- gnls_aic <- NULL
  modl <- hitc <- hill_tp <- max_med <- gnls_tp <- hill_95 <- hill_ga <- NULL
  hill_gw <- gnls_95 <- gnls_ga <- gnls_gw <- coff_upper <- coff_lower <- NULL
  rgbl <- hill <- hill_gu <- hill_lu <- hill_gl <- hill_ll <- hill_gc <- NULL
  hill_lc <- hill_al <- logc_min <- hill_ai <- logc_max <- hill_au <- NULL
  gnls_gu <- gnls_lu <- gnls_gl <- gnls_ll <- gnls_gc <- gnls_lc <- NULL
  gnls_al <- gnls_ai <- gnls_au <- cnst_win <- hill_win <- gnls_win <- NULL
  cnst <- fitc <- gnls <- modl_er <- cnst_er <- modl_rmse <- cnst_rmse <- NULL
  modl_prob <- cnst_prob <- hill_er <- modl_tp <- modl_ga <- modl_gw <- NULL
  hill_rmse <- hill_prob <- modl_acb <- modl_acc <- gnls_er <- modl_la <- NULL
  gnls_la <- modl_lw <- gnls_lw <- gnls_rmse <- gnls_prob <- actp <- NULL
  modl_ac10 <- NULL
  
  owarn <- getOption("warn")
  options(warn = 1)
  on.exit(options(warn = owarn))
  
  ## Check the ae input
  if (length(ae) > 1) {
    warning("ae must be of length 1. Level 5 processing incomplete; no updates",
            "\n  made to the mc5 table for AEIDS ", 
            paste(ae, collapse = ", "), ".")
    if(wr) return(FALSE) else return(list(FALSE, NULL))
  }
  
  stime <- Sys.time()
  
  ## Load level 4 data
  dat <- tcplLoadData(lvl = 4L, type = "mc", fld = "aeid", val = ae)
  
  ## Check if any level 4 data was loaded
  if (nrow(dat) == 0) {
    warning("No level 4 data for AEID", ae, ". Level 5 processing incomplete;",
            " no updates\n  made to the mc5 table for AEID", ae, ".")
    if(wr) return(FALSE) else return(list(FALSE, NULL))
  }
  
  ttime <- round(difftime(Sys.time(), stime, units = "sec"), 2)
  ttime <- paste(unclass(ttime), units(ttime))
  
  cat("Loaded L4 AEID", ae, " (", nrow(dat), " rows; ", ttime,")\n", sep = "")
  
  stime <- Sys.time()
  
  ## Initialize coff vector
  coff <- 0
  
  ## Load cutoff functions
  mthd_funcs <- mc5_mthds()
  
  ## Load cutoff methods
  ms <- tcplMthdLoad(lvl = 5L, id = ae, type = "mc")
  if (nrow(ms) == 0) {
    warning("No level 5 methods for AEID", ae, " -- cutoff will be 0.")
  }
  
  ## Apply cutoff methods
  exprs <- lapply(mthd_funcs[ms$mthd], do.call, args = list())
  fenv <- environment()
  invisible(rapply(exprs, eval, envir = fenv))
  
  ## Determine final cutoff
  dat[ , coff := max(coff)]
  
  ## Determine winning model
  dat[ , maic := pmin(cnst_aic, hill_aic, gnls_aic, na.rm = TRUE)]
  # Order matters here, because in the case of a tie the simpler model will
  # overwrite the more complex model as the winner.
  dat[gnls_aic == maic, modl := "gnls"]
  dat[hill_aic == maic, modl := "hill"]
  dat[cnst_aic == maic, modl := "cnst"]
  
  ## Make the hitcall
  dat[ , hitc := FALSE]
  dat[modl == "hill" & hill_tp >= coff & max_med >= coff, hitc := TRUE]  
  dat[modl == "gnls" & gnls_tp >= coff & max_med >= coff, hitc := TRUE]
  
  ###--------------------- Bin the Dose-Response Sets -----------------------###
  
  ## Calculate AC05 and AC95
  dat[ , hill_95 := tcplHillACXX(95, hill_tp, hill_ga, hill_gw)]
  dat[ , gnls_95 := tcplHillACXX(95, gnls_tp, gnls_ga, gnls_gw)]
  
  ## Add a few helper columns
  dat[ , coff_upper := 1.2 * coff]
  dat[ , coff_lower := 0.8 * coff]
  dat[ , rgbl := !is.na(hill)]
  dat[ , hill_gu := hill_tp >  coff_upper]
  dat[ , hill_lu := hill_tp <= coff_upper]
  dat[ , hill_gl := hill_tp >= coff_lower]
  dat[ , hill_ll := hill_tp <  coff_lower]
  dat[ , hill_gc := hill_tp >= coff]
  dat[ , hill_lc := hill_tp <  coff]
  dat[ , hill_al := hill_ga <= logc_min]
  dat[ , hill_ai := hill_ga > logc_min & hill_95 < logc_max]
  dat[ , hill_au := hill_ga > logc_min & hill_95 >= logc_max]
  dat[ , gnls_gu := gnls_tp >  coff_upper]
  dat[ , gnls_lu := gnls_tp <= coff_upper]
  dat[ , gnls_gl := gnls_tp >= coff_lower]
  dat[ , gnls_ll := gnls_tp <  coff_lower]
  dat[ , gnls_gc := gnls_tp >= coff]
  dat[ , gnls_lc := gnls_tp <  coff]
  dat[ , gnls_al := gnls_ga <= logc_min]
  dat[ , gnls_ai := gnls_ga > logc_min & gnls_95 < logc_max]
  dat[ , gnls_au := gnls_ga > logc_min & gnls_95 >= logc_max]
  dat[ , cnst_win := modl == "cnst"]
  dat[ , hill_win := modl == "hill"]
  dat[ , gnls_win := modl == "gnls"]
  
  ## Initiate fitc column
  dat[ , fitc := NA_integer_]
  
  ## 02: CANNOT DETERMINE
  dat[is.na(cnst), fitc := 2L]
  ## 04: RESP < BLINE
  dat[!rgbl & !is.na(cnst), fitc := 4L]
  ## 07: NO TP >= 0.8(COFF)
  dat[rgbl & cnst_win & (!hill_gl|is.na(hill_tp)) & (!gnls_gl|is.na(gnls_tp)),
      fitc := 7L]
  ## 09: NO TP >= COFF
  dat[is.na(fitc) & rgbl & cnst_win & 
        (!hill_gc|is.na(hill_tp)) & (!gnls_gc|is.na(gnls_tp)), 
      fitc := 9L]
  ## 10: ANY TP >= COFF
  dat[is.na(fitc) & rgbl & cnst_win & (hill_gc | gnls_gc), fitc := 10L]
  ## 54: GNLS DNC
  dat[!hitc & hill_win & hill_ll & !gnls, fitc := 54]
  ## 13: GNLS < 0.8(COFF)
  dat[is.na(fitc) & !hitc & hill_win & hill_ll & gnls_ll, fitc := 13L]
  ## 15: GNLS < COFF
  dat[is.na(fitc) & !hitc & hill_win & hill_ll & gnls_lc, fitc := 15L]
  ## 16: GNLS >= COFF
  dat[is.na(fitc) & !hitc & hill_win & hill_ll & gnls_gc, fitc := 16L]
  ## 55: GNLS DNC
  dat[!hitc & hill_win & hill_gl & !gnls, fitc := 55]
  ## 18: GNLS < 0.8(COFF)
  dat[is.na(fitc) & !hitc & hill_win & hill_gl & gnls_ll, fitc := 18L]
  ## 20: GNLS < COFF
  dat[is.na(fitc) & !hitc & hill_win & hill_gl & gnls_lc, fitc := 20L]
  ## 21: GNLS >= COFF
  dat[is.na(fitc) & !hitc & hill_win & hill_gl & gnls_gc, fitc := 21L]
  ## 52: HILL DNC
  dat[!hitc & gnls_win & gnls_ll & !hill, fitc := 52]
  ## 24: HILL < 0.8(COFF)
  dat[is.na(fitc) & !hitc & gnls_win & gnls_ll & hill_ll, fitc := 24L]
  ## 26: HILL < COFF
  dat[is.na(fitc) & !hitc & gnls_win & gnls_ll & hill_lc, fitc := 26L]
  ## 27: HILL >= COFF
  dat[is.na(fitc) & !hitc & gnls_win & gnls_ll & hill_gc, fitc := 27L]
  ## 53: HILL DNC
  dat[!hitc & gnls_win & gnls_gl & !hill, fitc := 53]  
  ## 29: HILL < 0.8(COFF)
  dat[is.na(fitc) & !hitc & gnls_win & gnls_gl & hill_ll, fitc := 29L]
  ## 31: HILL < COFF
  dat[is.na(fitc) & !hitc & gnls_win & gnls_gl & hill_lc, fitc := 31L]
  ## 32: HILL >= COFF
  dat[is.na(fitc) & !hitc & gnls_win & gnls_gl & hill_gc, fitc := 32L]
  ## 36: GA <= LCONC
  dat[hitc & hill_win & hill_lu & hill_al, fitc := 36L]
  ## 37: LCONC < GA < HCONC
  dat[hitc & hill_win & hill_lu & hill_ai, fitc := 37L]
  ## 38: GA >= HCONC
  dat[hitc & hill_win & hill_lu & hill_au, fitc := 38L]
  ## 40: GA <= LCONC
  dat[hitc & hill_win & hill_gu & hill_al, fitc := 40L]
  ## 41: LCONC < GA < HCONC
  dat[hitc & hill_win & hill_gu & hill_ai, fitc := 41L]
  ## 42: GA >= HCONC
  dat[hitc & hill_win & hill_gu & hill_au, fitc := 42L]
  ## 45: GA <= LCONC
  dat[hitc & gnls_win * gnls_lu & gnls_al, fitc := 45L]
  ## 46: LCONC < GA < HCONC
  dat[hitc & gnls_win * gnls_lu & gnls_ai, fitc := 46L]
  ## 47: GA >= HCONC
  dat[hitc & gnls_win * gnls_lu & gnls_au, fitc := 47L]
  ## 49: GA <= LCONC
  dat[hitc & gnls_win * gnls_gu & gnls_al, fitc := 49L]
  ## 50: LCONC < GA < HCONC
  dat[hitc & gnls_win * gnls_gu & gnls_ai, fitc := 50L]
  ## 51: GA >= HCONC
  dat[hitc & gnls_win * gnls_gu & gnls_au, fitc := 51L]
  
  ## Update hit-calls with the cannot determine call
  dat[ , hitc := as.integer(hitc)]  
  dat[is.na(cnst), hitc := -1L]
  
  ## Add model fields
  modl_pars <- c("modl_acb", 
                 "modl_acc",
                 "modl_ac10",
                 "modl_er",
                 "modl_tp",
                 "modl_ga",
                 "modl_gw",
                 "modl_la",
                 "modl_lw",
                 "modl_rmse",
                 "modl_prob")
  dat[ , (modl_pars) := NA_real_]
  dat[modl == "cnst", modl_er := cnst_er]
  dat[modl == "cnst", modl_rmse := cnst_rmse]
  dat[modl == "cnst", modl_prob := cnst_prob]
  dat[modl == "hill", modl_er := hill_er]
  dat[modl == "hill", modl_tp := hill_tp]
  dat[modl == "hill", modl_ga := hill_ga]
  dat[modl == "hill", modl_gw := hill_gw]
  dat[modl == "hill", modl_rmse := hill_rmse]
  dat[modl == "hill", modl_prob := hill_prob]
  dat[modl == "hill" & hill_tp >= 3 * bmad, 
      modl_acb := tcplHillConc(3 * bmad, hill_tp, hill_ga, hill_gw)]
  dat[modl == "hill" & hill_tp >= coff,
      modl_acc := tcplHillConc(coff, hill_tp, hill_ga, hill_gw)]
  dat[modl == "hill", modl_ac10 := tcplHillACXX(10, hill_tp, hill_ga, hill_gw)]
  dat[modl == "gnls", modl_er := gnls_er]
  dat[modl == "gnls", modl_tp := gnls_tp]
  dat[modl == "gnls", modl_ga := gnls_ga]
  dat[modl == "gnls", modl_gw := gnls_gw]
  dat[modl == "gnls", modl_la := gnls_la]
  dat[modl == "gnls", modl_lw := gnls_lw]
  dat[modl == "gnls", modl_rmse := gnls_rmse]
  dat[modl == "gnls", modl_prob := gnls_prob]
  dat[modl == "gnls" & gnls_tp >= 3 * bmad, 
      modl_acb := tcplHillConc(3 * bmad, gnls_tp, gnls_ga, gnls_gw)]
  dat[modl == "gnls" & gnls_tp >= coff,
      modl_acc := tcplHillConc(coff,     gnls_tp, gnls_ga, gnls_gw)]
  dat[modl == "gnls", modl_ac10 := tcplHillACXX(10, gnls_tp, gnls_ga, gnls_gw)]
  
  ## Add activity probability
  dat[ , actp := 1 - cnst_prob]
  
  ttime <- round(difftime(Sys.time(), stime, units = "sec"), 2)
  ttime <- paste(unclass(ttime), units(ttime))
  cat("Processed L5 AEID", ae, " (", nrow(dat), 
      " rows; ", ttime, ")\n", sep = "")
  
  res <- TRUE
  
  outcols <- c("m4id", "aeid", "modl", "hitc", "fitc", 
               "coff", "actp", modl_pars)
  dat <- dat[ , .SD, .SDcols = outcols]
  
  ## Load into mc5 table -- else return results
  if (wr) {
    stime <- Sys.time()
    tcplWriteData(dat = dat, lvl = 5L, type = "mc")
    
    ttime <- round(difftime(Sys.time(), stime, units = "sec"), 2)
    ttime <- paste(unclass(ttime), units(ttime))
    cat("Wrote L5 AEID", ae, " (", nrow(dat), 
        " rows; ", ttime, ")\n", sep = "")
  } else {
    res <- c(list(res), list(dat))
  }
  
  return(res)
  
}

#-------------------------------------------------------------------------------
