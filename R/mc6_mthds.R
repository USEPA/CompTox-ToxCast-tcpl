#-------------------------------------------------------------------------------
# mc6_mthds: Load list of flag methods (to be used at level 6)
#-------------------------------------------------------------------------------

#' @name MC6_Methods
#' @title Load list of level 6 multiple-concentration flag methods
#' 
#' @description 
#' \code{mc6_mthds} returns a list of flag methods to be used 
#' during level 6 multiple-concentration processing.
#' 
#' @return A list functions
#' 
#' @seealso \code{\link{mc6}}, \code{\link{Method functions}} to query what
#' methods get applied to each aeid
#' 
#' @section Available Methods:
#' 
#' More information about the level 6 multiple-concentration processing is 
#' available in the package vignette, "Pipeline_Overview."
#' 
#' \describe{
#'   \item{singlept.hit.high}{The singlept.hit.high flag identifies 
#'   concentration series where the median response was greater than 3*bmad 
#'   only at the highest tested concentration and the series had an active 
#'   hit-call.}
#'   \item{singlept.hit.mid}{The singlept.hit.mid flag identifies concentration 
#'   series where the median response was greater than 3*bmad at only one 
#'   concentration (not the highest tested concentration) and the series had 
#'   an active hit-call.}
#'   \item{multipoint.neg}{The multipoint.neg flag identifies concentration 
#'   series with response medians greater than 3*bmad at multiple 
#'   concentrations and an inactive hit-call.}
#'   \item{gnls.lowconc}{The gnls.lowconc flag identifies concentration series
#'   where the gain-loss model won, the gain AC50 is less than the minimum 
#'   tested concentration, and the loss AC50 is less than the mean tested 
#'   concentration.}
#'   \item{noise}{The noise flag attempts to identify noisy concentration
#'   series by flagging series where the root mean square error for the series
#'   is greater than the cutoff for the assay endpoint.}
#'   \item{border.hit}{The border.hit flag identifies active concentration 
#'   series where the top parameter of the winning model was less than or equal 
#'   to 1.2*cut-off or the the activity probablity was less than 0.9.}
#'   \item{border.miss}{The border.miss flag identifies inactive concentration
#'   series where either the Hill or gain-loss top parameter was greater than 
#'   or equal to 0.8*cut-off and the activity probability was greater than 0.5.}
#'   \item{overfit.hit}{The overfit.hit flag recalculates the model winner 
#'   after applying a small sample correction factor to the AIC values. If the 
#'   hit-call would be changed after applying the small sample correction 
#'   factor the series is flagged. Series with less than 5 concentrations where
#'   the hill model won and series with less than 7 concentrations where the 
#'   gain-loss model won are automatically flagged.}
#'   \item{efficacy.50}{The efficacy.50 flag identifies concentration series 
#'   with efficacy values (either the modeled top parameter for the winning
#'   model or the maximum median response) are less than 50 for percent activity
#'   data or log2(1.5) for fold induction data}
#'   \item{modlga.lowconc}{The modlga.lowconc flag identifies concentration series 
#'   with modl_ga (AC50) values less than the minimum tested concentration.}
#' }

mc6_mthds <- function() {
  
  list(
    
    singlept.hit.high = function(mthd) {
      
      flag <- "Only highest conc above baseline, active"
      out  <- c("m5id", "m4id", "aeid", "mc6_mthd_id", 
                "flag", "fval", "fval_unit")
      init <- bquote(list(.(mthd), .(flag), NA_real_, NA_character_, FALSE))
      e1 <- bquote(ft[ , .(c(out[4:7], "test")) := .(init)])
      e2 <- bquote(ft[ , lstc := max_med_conc == logc_max])
      e3 <- bquote(ft[ , test := nmed_gtbl == 1 & hitc == 1 & lstc])
      e4 <- bquote(f[[.(mthd)]] <- ft[which(test), .SD, .SDcols = .(out)])
      cr <- c("mc6_mthd_id", "flag", "fval", "fval_unit", "test", "lstc")
      e5 <- bquote(ft[ , .((cr)) := NULL])
      list(e1, e2, e3, e4, e5)
      
    },
    
    singlept.hit.mid = function(mthd) {
      
      flag <- "Only one conc above baseline, active"
      out  <- c("m5id", "m4id", "aeid", "mc6_mthd_id", 
                "flag", "fval", "fval_unit")
      init <- bquote(list(.(mthd), .(flag), NA_real_, NA_character_, FALSE))
      e1 <- bquote(ft[ , .(c(out[4:7], "test")) := .(init)])
      e2 <- bquote(ft[ , lstc := max_med_conc == logc_max])
      e3 <- bquote(ft[ , test := nmed_gtbl == 1 & hitc == 1 & !lstc])
      e4 <- bquote(f[[.(mthd)]] <- ft[which(test), .SD, .SDcols = .(out)])
      cr <- c("mc6_mthd_id", "flag", "fval", "fval_unit", "test", "lstc")
      e5 <- bquote(ft[ , .((cr)) := NULL])
      list(e1, e2, e3, e4, e5)
      
    },
    
    multipoint.neg = function(mthd) {
      
      flag <- "Multiple points above baseline, inactive"
      out  <- c("m5id", "m4id", "aeid", "mc6_mthd_id", 
                "flag", "fval", "fval_unit")
      init <- bquote(list(.(mthd), .(flag), NA_real_, NA_character_, FALSE))
      e1 <- bquote(ft[ , .(c(out[4:7], "test")) := .(init)])
      e2 <- bquote(ft[ , test := nmed_gtbl > 1 & hitc == 0])
      e3 <- bquote(f[[.(mthd)]] <- ft[which(test), .SD, .SDcols = .(out)])
      cr <- c("mc6_mthd_id", "flag", "fval", "fval_unit", "test")
      e4 <- bquote(ft[ , .((cr)) := NULL])
      list(e1, e2, e3, e4)
      
    },
    
    gnls.lowconc = function(mthd) {
      
      flag <- "Gain AC50 < lowest conc & loss AC50 < mean conc"
      out  <- c("m5id", "m4id", "aeid", "mc6_mthd_id", 
                "flag", "fval", "fval_unit")
      init <- bquote(list(.(mthd), .(flag), NA_real_, NA_character_, FALSE))
      e1 <- bquote(ft[ , .(c(out[4:7], "test")) := .(init)])
      conc_cols <- c("logc_max", "logc_min")
      e2 <- bquote(ft[ , cmen := rowMeans(.SD), .SDcols = .(conc_cols)])
      e3 <- bquote(ft[ , test := modl_ga < logc_min & modl_la < cmen])
      e4 <- bquote(f[[.(mthd)]] <- ft[which(test), .SD, .SDcols = .(out)])
      cr <- c("mc6_mthd_id", "flag", "fval", "fval_unit", "test", "cmen")
      e5 <- bquote(ft[ , .((cr)) := NULL])
      list(e1, e2, e3, e4, e5)
      
    },
    
    noise = function(mthd) {
      
      flag <- "Noisy data"
      out  <- c("m5id", "m4id", "aeid", "mc6_mthd_id", 
                "flag", "fval", "fval_unit")
      init <- bquote(list(.(mthd), .(flag), NA_real_, NA_character_, FALSE))
      e1 <- bquote(ft[ , .(c(out[4:7], "test")) := .(init)])
      e2 <- bquote(ft[ , test := modl_rmse > coff])
      e3 <- bquote(f[[.(mthd)]] <- ft[which(test), .SD, .SDcols = .(out)])
      cr <- c("mc6_mthd_id", "flag", "fval", "fval_unit", "test")
      e4 <- bquote(ft[ , .((cr)) := NULL])
      list(e1, e2, e3, e4)
      
    },    
    
    border.hit = function(mthd) {
      
      flag <- "Borderline active"
      out  <- c("m5id", "m4id", "aeid", "mc6_mthd_id", 
                "flag", "fval", "fval_unit")
      init <- bquote(list(.(mthd), .(flag), NA_real_, NA_character_, FALSE))
      e1 <- bquote(ft[ , .(c(out[4:7], "test")) := .(init)])
      e2 <- bquote(ft[ , t1 := actp < 0.9])
      e3 <- bquote(ft[ , t2 := modl_tp <= 1.2*coff | max_med <= 1.2*coff])
      e4 <- bquote(ft[ , test := hitc == 1 & (t1 | t2)])
      e5 <- bquote(f[[.(mthd)]] <- ft[which(test), .SD, .SDcols = .(out)])
      cr <- c("mc6_mthd_id", "flag", "fval", "fval_unit", "test")
      e6 <- bquote(ft[ , .((cr)) := NULL])
      list(e1, e2, e3, e4, e5, e6)
      
    },
    
    border.miss = function(mthd) {
      
      flag <- "Borderline inactive"
      out  <- c("m5id", "m4id", "aeid", "mc6_mthd_id", 
                "flag", "fval", "fval_unit")
      init <- bquote(list(.(mthd), .(flag), NA_real_, NA_character_, FALSE))
      e1 <- bquote(ft[ , .(c(out[4:7], "test")) := .(init)])
      e2 <- bquote(ft[ , tp.8 := gnls_tp >= 0.8*coff | hill_tp >= 0.8*coff])
      e3 <- bquote(ft[ , test := hitc == 0L & actp > 0.5 & tp.8])
      e4 <- bquote(f[[.(mthd)]] <- ft[which(test), .SD, .SDcols = .(out)])
      cr <- c("mc6_mthd_id", "flag", "fval", "fval_unit", "test", "tp.8")
      e5 <- bquote(ft[ , .((cr)) := NULL])
      list(e1, e2, e3, e4, e5)
      
    },
    
    overfit.hit = function(mthd) {
      
      flag <- "Hit-call potentially confounded by overfitting"
      out  <- c("m5id", "m4id", "aeid", "mc6_mthd_id", 
                "flag", "fval", "fval_unit")
      init <- bquote(list(.(mthd), .(flag), NA_real_, NA_character_, FALSE))
      e1  <- bquote(ft[ , .(c(out[4:7], "test")) := .(init)])
      e2  <- bquote(ft[modl == "hill" & npts < 5 & hitc == 1, test := TRUE])
      e3  <- bquote(ft[modl == "gnls" & npts < 7 & hitc == 1, test := TRUE])
      e4  <- bquote(ft[npts > 1, cna := cnst_aic +  4/(npts - 2)])
      e5  <- bquote(ft[npts > 4, hna := hill_aic + 40/(npts - 4)])
      e6  <- bquote(ft[npts > 6, gna := gnls_aic + 84/(npts - 7)])
      e7  <- bquote(ft[ , nma := pmin(cna, hna, gna, na.rm = TRUE)])
      e8  <- bquote(ft[gna == nma, nmdl := "gnls"])
      e9  <- bquote(ft[hna == nma, nmdl := "hill"])
      e10 <- bquote(ft[cna == nma, nmdl := "cnst"])
      e11 <- bquote(ft[ , nhc := FALSE])
      e12 <- bquote(ft[nmdl == "hill" & hill_tp >= coff & max_med >= coff, 
                       nhc := TRUE]) 
      e13 <- bquote(ft[nmdl == "gnls" & gnls_tp >= coff & max_med >= coff, 
                       nhc := TRUE])
      e14 <- bquote(ft[hitc == 1 & !nhc, test := TRUE])
      e15 <- bquote(f[[.(mthd)]] <- ft[which(test), .SD, .SDcols = .(out)])
      cr <- c("mc6_mthd_id", "flag", "fval", "fval_unit", "test", 
              "cna", "hna", "gna", "nma", "nmdl", "nhc")
      e16 <- bquote(ft[ , .((cr)) := NULL])
      list(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, 
           e11, e12, e13, e14, e15, e16)
      
    },
    
    efficacy.50 = function(mthd) {
      
      flag <- "Less than 50% efficacy"
      out  <- c("m5id", "m4id", "aeid", "mc6_mthd_id", 
                "flag", "fval", "fval_unit")
      init <- bquote(list(.(mthd), .(flag), NA_real_, NA_character_, FALSE))
      e1 <- bquote(ft[ , .(c(out[4:7], "test")) := .(init)])
      e2 <- bquote(ft[hitc == 1 & coff >= 5,
                      test := modl_tp < 50 | max_med < 50])
      e3 <- bquote(ft[hitc == 1 & coff < 5,
                      test := modl_tp < log2(1.5) | max_med < log2(1.5)])
      e4 <- bquote(f[[.(mthd)]] <- ft[which(test), .SD, .SDcols = .(out)])
      cr <- c("mc6_mthd_id", "flag", "fval", "fval_unit", "test")
      e5 <- bquote(ft[ , .((cr)) := NULL])
      list(e1, e2, e3, e4, e5)
      
    },
    
    modlga.lowconc = function(mthd) {
      
      flag <- "AC50 less than lowest concentration tested"
      out  <- c("m5id", "m4id", "aeid", "mc6_mthd_id", 
                "flag", "fval", "fval_unit")
      init <- bquote(list(.(mthd), .(flag), NA_real_, NA_character_, FALSE))
      e1 <- bquote(ft[ , .(c(out[4:7], "test")) := .(init)])
      e2 <- bquote(ft[hitc == 1, test := modl_ga < logc_min])
      e3 <- bquote(f[[.(mthd)]] <- ft[which(test), .SD, .SDcols = .(out)])
      cr <- c("mc6_mthd_id", "flag", "fval", "fval_unit", "test")
      e4 <- bquote(ft[ , .((cr)) := NULL])
      list(e1, e2, e3, e4)
      
    }   
    
  )
  
}

#-------------------------------------------------------------------------------
