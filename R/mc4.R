#-------------------------------------------------------------------------------
# mc4: Perform level 4 multiple-concentration processing
#-------------------------------------------------------------------------------

#' @template proclvl
#' @templateVar LVL 4
#' @templateVar type mc
#' 
#' @param ae Integer of length 1, assay endpoint id (aeid) for processing.
#' @param wr Logical, whether the processed data should be written to the tcpl
#' database
#' 
#' @details
#' Level 4 multiple-concentration modeling takes the dose-response data for 
#' chemical-assay pairs, and fits three models to the data: constant, hill, 
#' and gain-loss. For more information about the models see 
#' \code{\link{Models}}. When a chemical has more than one sample, the function 
#' fits each sample seperately.
#' 
#' @seealso \code{\link{tcplFit}}, \code{\link{Models}}
#' 
#' @import data.table
#' @importFrom stats mad

mc4 <- function(ae, wr = FALSE) {
  
  ## Variable-binding to pass R CMD Check
  bmad <- resp <- cndx <- wllt <- logc <- spid <- cnst_aic <- hill_aic <- NULL
  gnls_aic <- NULL
  
  owarn <- getOption("warn")
  options(warn = 1)
  on.exit(options(warn = owarn))
  
  ## Check the ae input
  if (length(ae) > 1) {
    warning("ae must be of length 1. Level 4 processing incomplete; no updates",
            "\n  made to the mc4 table for AEIDS ", 
            paste(ae, collapse = ", "), ".")
    if(wr) return(FALSE) else return(list(FALSE, NULL))
  }
  
  stime <- Sys.time()
  
  ## Load level 3 data
  dat <- tcplLoadData(lvl = 3L, type = "mc", fld = "aeid", val = ae)
  dat <- dat[wllt %in% c("t", "c", "o")]
  
  ## Check if any level 3 data was loaded
  if (nrow(dat) == 0) {
    warning("No level 3 data for AEID", ae, ". Level 4 processing incomplete;",
            " no updates\n  made to the mc4 table for AEID", ae, ".")
    if(wr) return(FALSE) else return(list(FALSE, NULL))
  }
  
  ttime <- round(difftime(Sys.time(), stime, units = "sec"), 2)
  ttime <- paste(unclass(ttime), units(ttime))
  
  cat("Loaded L3 AEID", ae, " (", nrow(dat), " rows; ", ttime,")\n", sep = "")
  
  stime <- Sys.time()
  
  ## Calculate the baseline mad
  dat[ , bmad := mad(resp[cndx %in% 1:2 & wllt == "t"], na.rm = TRUE)]
  
  ## Check to see if all samples should be fit
  fit_all <- as.logical(tcplLoadAeid("aeid", ae, "fit_all")$fit_all)
  
  fitpars <- c("resp_max",
               "resp_min",
               "max_mean",
               "max_mean_conc",
               "max_med",
               "max_med_conc",
               "logc_max",
               "logc_min",
               "cnst",
               "hill",
               "hcov",
               "gnls",
               "gcov",
               "cnst_er",
               "cnst_aic",
               "cnst_rmse",
               "hill_tp",
               "hill_tp_sd",
               "hill_ga",
               "hill_ga_sd",
               "hill_gw",
               "hill_gw_sd",
               "hill_er",
               "hill_er_sd",
               "hill_aic",
               "hill_rmse",
               "gnls_tp",
               "gnls_tp_sd",
               "gnls_ga",
               "gnls_ga_sd",
               "gnls_gw",
               "gnls_gw_sd",
               "gnls_la",
               "gnls_la_sd",
               "gnls_lw",
               "gnls_lw_sd",
               "gnls_er",
               "gnls_er_sd",
               "gnls_aic",
               "gnls_rmse",
               "nconc", 
               "npts",
               "nrep",
               "nmed_gtbl")
  
  ## Fit the data by spid
  dat[ , 
      c("tmpi", fitpars) := c(.GRP,
                              tcplFit(logc = logc,
                                      resp = resp,
                                      bmad = bmad,
                                      force.fit = fit_all)),
      by = spid]
  
  ## Calculate the aic probabilities
  aic_probs <- c("cnst_prob", "hill_prob", "gnls_prob")
  dat[ , (aic_probs) := tcplAICProb(cnst_aic, hill_aic, gnls_aic)]
  
  ttime <- round(difftime(Sys.time(), stime, units = "sec"), 2)
  ttime <- paste(unclass(ttime), units(ttime))
  cat("Processed L4 AEID", ae, " (", nrow(dat), 
      " rows; ", ttime, ")\n", sep = "")
  
  res <- TRUE
    
  ## Load into mc4 & mc4_agg tables -- else return results
  if (wr) {
    stime <- Sys.time()
    tcplWriteData(dat = dat, lvl = 4L, type = "mc")
    
    ttime <- round(difftime(Sys.time(), stime, units = "sec"), 2)
    ttime <- paste(unclass(ttime), units(ttime))
    cat("Wrote L4 AEID", ae, " (", nrow(dat), 
        " rows; ", ttime, ")\n", sep = "")
  } else {
    res <- c(list(res), list(dat))
  }
  
  return(res)
  
}

#-------------------------------------------------------------------------------
