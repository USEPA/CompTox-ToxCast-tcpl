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
#'   \item{modl.directionality.fail}{The modl.directionality.fail flag 
#'   identifies concentration series where, had the model direction been 
#'   opposite, more responses would have exceeded the cutoff, i.e., series 
#'   where the number of responses in absolute value which are greater than the 
#'   absolute value of the cutoff exceeds twice the number of responses which 
#'   are more extreme than the cutoff (< cutoff when cutoff is negative, 
#'   > cutoff when cutoff is positive).}
#'   \item{low.nrep}{The low.nrep flag identifies concentration series where 
#'   the average number of replicates per concentration tested is less than 2.}
#'   \item{low.nconc}{The low.nconc flag identifies concentration series where 
#'   the number of concentrations tested is greater or equal to 4.}
#'   \item{bmd.highconc}{The bmd.highconc flag identifies concentration series 
#'   with a steep curve, i.e., series where the bmd is greater than 50% of the 
#'   range of concentrations tested.}
#'   \item{bmd.lowconc}{The bmd.lowconc flag identifies concentration series 
#'   with a shallow curve, i.e., series where the bmd is less than 50% of the 
#'   range of concentrations tested.}
#'   \item{bmd.high}{The bmd.high flag attempts to identify noisy concentration
#'   series by flagging series where the bmd is greater than the ac50.}
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
#'   \item{border}{The border flag identifies concentration 
#'   series which have borderline activity, i.e., series where the top 
#'   parameter of the winning model was greater than or equal to 0.8*cutoff 
#'   and less than or equal to 1.2*cutoff.}
#'   \item{overfit.hit}{The overfit.hit flag recalculates the model winner 
#'   after applying a small sample correction factor to the AIC values. If the 
#'   hit-call would be changed after applying the small sample correction 
#'   factor the series is flagged. Series with less than 5 concentrations where
#'   the hill model won and series with less than 7 concentrations where the 
#'   gain-loss model won are automatically flagged.}
#'   \item{efficacy.50}{The efficacy.50 flag identifies concentration series 
#'   with efficacy values (either the modeled top parameter for the winning
#'   model or the maximum median response) less than 50 for percent activity
#'   data or log2(1.5) for fold induction data}
#'   \item{ac50.lowconc}{The ac50.lowconc flag identifies concentration series 
#'   with ac50 values less than the minimum tested concentration.}
#'   \item{viability.gnls}{The viability.gnls flag identifies concentration series 
#'   of cell viability assays which were fit with gain-loss as the winning 
#'   model and the series had an active hit-call.}
#' }

mc6_mthds <- function() {
  
  list(
    
    modl.directionality.fail = function(mthd) {
      
      flag <- "Model directionality questionable"
      out  <- c("m5id", "m4id", "aeid", "mc6_mthd_id", 
                "flag", "fval", "fval_unit")
      init <- bquote(list(.(mthd), .(flag), NA_real_, NA_character_, FALSE))
      e1 <- bquote(dr[ , .(c(out[4:7], "test")) := .(init)])
      e2 <- bquote(dr[ , coffsign := ifelse(top < 0, -1*coff, coff)])
      e3 <- bquote(dr[ , gtabscoff := abs(resp) > abs(coffsign)])
      e4 <- bquote(dr[ , nrsp_gtabscoff := sum(gtabscoff), by = m4id])
      e5 <- bquote(dr[ , gtcoff := resp > coffsign])
      e6 <- bquote(dr[ , ltcoff := resp < coffsign])
      e7 <- bquote(dr[ , nrsp_gtcoff := sum(gtcoff), by = m4id])
      e8 <- bquote(dr[ , nrsp_ltcoff := sum(ltcoff), by = m4id])
      e9 <- bquote(dr[ , test := ifelse(coffsign > 0, nrsp_gtabscoff > 2*nrsp_gtcoff, nrsp_gtabscoff > 2*nrsp_ltcoff)])
      e10 <- bquote(f[[.(mthd)]] <- unique(dr[which(test), .SD, .SDcols = .(out)], by = NULL))
      cr <- c("mc6_mthd_id", "flag", "fval", "fval_unit", "test", "coffsign", "gtabscoff", "nrsp_gtabscoff", "gtcoff", "ltcoff", "nrsp_gtcoff", "nrsp_ltcoff")
      e11 <- bquote(dr[ , .(cr) := NULL])
      list(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11)
      
    },
    
    low.nrep = function(mthd) { 
      
      flag <- "Average number of replicates per conc is less than 2"
      out  <- c("m5id", "m4id", "aeid", "mc6_mthd_id", 
                "flag", "fval", "fval_unit")
      init <- bquote(list(.(mthd), .(flag), NA_real_, NA_character_, FALSE))
      e1 <- bquote(ft[ , .(c(out[4:7], "test")) := .(init)])
      e2 <- bquote(ft[ , test := nrep < 2])
      e3 <- bquote(f[[.(mthd)]] <- ft[which(test), .SD, .SDcols = .(out)])
      cr <- c("mc6_mthd_id", "flag", "fval", "fval_unit", "test")
      e4 <- bquote(ft[ , .(cr) := NULL])
      list(e1, e2, e3, e4)
      
    },
    
    low.nconc = function(mthd) { 
      
      flag <- "Number of concentrations tested is less than 4"
      out  <- c("m5id", "m4id", "aeid", "mc6_mthd_id", 
                "flag", "fval", "fval_unit")
      init <- bquote(list(.(mthd), .(flag), NA_real_, NA_character_, FALSE))
      e1 <- bquote(ft[ , .(c(out[4:7], "test")) := .(init)])
      e2 <- bquote(ft[ , test := modl != "none" & nconc <= 4])
      e3 <- bquote(f[[.(mthd)]] <- ft[which(test), .SD, .SDcols = .(out)])
      cr <- c("mc6_mthd_id", "flag", "fval", "fval_unit", "test")
      e4 <- bquote(ft[ , .(cr) := NULL])
      list(e1, e2, e3, e4)
      
    },
    
    #bmd.highconc = function(mthd) {
      
      #flag <- "Bmd falling > 50% conc range tested, indicative of a steep curve"
      #out  <- c("m5id", "m4id", "aeid", "mc6_mthd_id", 
                #"flag", "fval", "fval_unit")
      #init <- bquote(list(.(mthd), .(flag), NA_real_, NA_character_, FALSE))
      #e1 <- bquote(ft[ , .(c(out[4:7], "test")) := .(init)])
      #e2 <- bquote(ft[ , crng := 10^logc_max - 10^logc_min])
      #e3 <- bquote(ft[ , test :=  bmd > 0.5*crng & hitc >= 0.9])
      #e4 <- bquote(f[[.(mthd)]] <- ft[which(test), .SD, .SDcols = .(out)])
      #cr <- c("mc6_mthd_id", "flag", "fval", "fval_unit", "test", "crng")
      #e5 <- bquote(ft[ , .(cr) := NULL])
      #list(e1, e2, e3, e4, e5)
      
    #},
    
    #bmd.lowconc = function(mthd) { 
      
      #flag <- "Bmd falling < 50% conc range tested, indicative of a shallow curve"
      #out  <- c("m5id", "m4id", "aeid", "mc6_mthd_id", 
                #"flag", "fval", "fval_unit")
      #init <- bquote(list(.(mthd), .(flag), NA_real_, NA_character_, FALSE))
      #e1 <- bquote(ft[ , .(c(out[4:7], "test")) := .(init)])
      #e2 <- bquote(ft[ , crng := 10^logc_max - 10^logc_min])
      #e3 <- bquote(ft[ , test :=  bmd < 0.5*crng & hitc >= 0.9])
      #e4 <- bquote(f[[.(mthd)]] <- ft[which(test), .SD, .SDcols = .(out)])
      #cr <- c("mc6_mthd_id", "flag", "fval", "fval_unit", "test", "crng")
      #e5 <- bquote(ft[ , .(cr) := NULL])
      #list(e1, e2, e3, e4, e5)
      
    #},
    
    bmd.high = function(mthd) {
      
      flag <- "Bmd > ac50, indication of high baseline variability"
      out  <- c("m5id", "m4id", "aeid", "mc6_mthd_id", 
                "flag", "fval", "fval_unit")
      init <- bquote(list(.(mthd), .(flag), NA_real_, NA_character_, FALSE))
      e1 <- bquote(ft[ , .(c(out[4:7], "test")) := .(init)])
      e2 <- bquote(ft[ , test := bmd > ac50])
      e3 <- bquote(f[[.(mthd)]] <- ft[which(test), .SD, .SDcols = .(out)])
      cr <- c("mc6_mthd_id", "flag", "fval", "fval_unit", "test")
      e4 <- bquote(ft[ , .(cr) := NULL])
      list(e1, e2, e3, e4)
      
    },
    
    singlept.hit.high = function(mthd) {
      
      flag <- "Only highest conc above baseline, active"
      out  <- c("m5id", "m4id", "aeid", "mc6_mthd_id", 
                "flag", "fval", "fval_unit")
      init <- bquote(list(.(mthd), .(flag), NA_real_, NA_character_, FALSE))
      e1 <- bquote(ft[ , .(c(out[4:7], "test")) := .(init)])
      e2 <- bquote(ft[ , lstc := max_med_conc == logc_max])
      e3 <- bquote(ft[ , test := nmed_gtbl == 1 & hitc >= 0.9 & lstc])
      e4 <- bquote(f[[.(mthd)]] <- ft[which(test), .SD, .SDcols = .(out)])
      cr <- c("mc6_mthd_id", "flag", "fval", "fval_unit", "test", "lstc")
      e5 <- bquote(ft[ , .(cr) := NULL])
      list(e1, e2, e3, e4, e5)
      
    },
    
    singlept.hit.mid = function(mthd) {
      
      flag <- "Only one conc above baseline, active"
      out  <- c("m5id", "m4id", "aeid", "mc6_mthd_id", 
                "flag", "fval", "fval_unit")
      init <- bquote(list(.(mthd), .(flag), NA_real_, NA_character_, FALSE))
      e1 <- bquote(ft[ , .(c(out[4:7], "test")) := .(init)])
      e2 <- bquote(ft[ , lstc := max_med_conc == logc_max])
      e3 <- bquote(ft[ , test := nmed_gtbl == 1 & hitc >= 0.9 & !lstc])
      e4 <- bquote(f[[.(mthd)]] <- ft[which(test), .SD, .SDcols = .(out)])
      cr <- c("mc6_mthd_id", "flag", "fval", "fval_unit", "test", "lstc")
      e5 <- bquote(ft[ , .(cr) := NULL])
      list(e1, e2, e3, e4, e5)
      
    },
    
    multipoint.neg = function(mthd) {
      
      flag <- "Multiple points above baseline, inactive"
      out  <- c("m5id", "m4id", "aeid", "mc6_mthd_id", 
                "flag", "fval", "fval_unit")
      init <- bquote(list(.(mthd), .(flag), NA_real_, NA_character_, FALSE))
      e1 <- bquote(ft[ , .(c(out[4:7], "test")) := .(init)])
      e2 <- bquote(ft[ , test := nmed_gtbl > 1 & hitc < 0.9])
      e3 <- bquote(f[[.(mthd)]] <- ft[which(test), .SD, .SDcols = .(out)])
      cr <- c("mc6_mthd_id", "flag", "fval", "fval_unit", "test")
      e4 <- bquote(ft[ , .(cr) := NULL])
      list(e1, e2, e3, e4)
      
    },
    
    gnls.lowconc = function(mthd) {
      
      flag <- "Gain AC50 < lowest conc & loss AC50 < mean conc"
      out  <- c("m5id", "m4id", "aeid", "mc6_mthd_id", 
                "flag", "fval", "fval_unit")
      init <- bquote(list(.(mthd), .(flag), NA_real_, NA_character_, FALSE))
      e1 <- bquote(ft[ , .(c(out[4:7], "test")) := .(init)])
      e2 <- bquote(ft[ , c_min := 10^logc_min])
      e3 <- bquote(ft[ , c_max := 10^logc_max])
      conc_cols <- c("c_min", "c_max")
      e4 <- bquote(ft[ , cmen := rowMeans(.SD), .SDcols = .(conc_cols)])
      e5 <- bquote(ifelse("ac50_loss" %in% names(ft), ft[ , test := modl == "gnls" & ac50 < c_min & ac50_loss < cmen], ft))
      e6 <- bquote(f[[.(mthd)]] <- ft[which(test), .SD, .SDcols = .(out)])
      cr <- c("mc6_mthd_id", "flag", "fval", "fval_unit", "test", "c_min", "c_max","cmen")
      e7 <- bquote(ft[ , .(cr) := NULL])
      list(e1, e2, e3, e4, e5, e6, e7)
      
    },
    
    noise = function(mthd) {
      
      flag <- "Noisy data"
      out  <- c("m5id", "m4id", "aeid", "mc6_mthd_id", 
                "flag", "fval", "fval_unit")
      init <- bquote(list(.(mthd), .(flag), NA_real_, NA_character_, FALSE))
      e1 <- bquote(ft[ , .(c(out[4:7], "test")) := .(init)])
      e2 <- bquote(ft[ , test := rmse > coff])
      e3 <- bquote(f[[.(mthd)]] <- ft[which(test), .SD, .SDcols = .(out)])
      cr <- c("mc6_mthd_id", "flag", "fval", "fval_unit", "test")
      e4 <- bquote(ft[ , .(cr) := NULL])
      list(e1, e2, e3, e4)
      
    },    
    
    border = function(mthd) {
      
      flag <- "Borderline"
      out  <- c("m5id", "m4id", "aeid", "mc6_mthd_id", 
                "flag", "fval", "fval_unit")
      init <- bquote(list(.(mthd), .(flag), NA_real_, NA_character_, FALSE))
      e1 <- bquote(ft[ , .(c(out[4:7], "test")) := .(init)])
      e2 <- bquote(ft[ , test := abs(top) <= 1.2*coff & abs(top) >= 0.8*coff])
      e3 <- bquote(f[[.(mthd)]] <- ft[which(test), .SD, .SDcols = .(out)])
      cr <- c("mc6_mthd_id", "flag", "fval", "fval_unit", "test")
      e4 <- bquote(ft[ , .(cr) := NULL])
      list(e1, e2, e3, e4)
      
    },
    
    #overfit.hit = function(mthd) {
      
      #flag <- "Hit-call potentially confounded by overfitting"
      #out  <- c("m5id", "m4id", "aeid", "mc6_mthd_id", 
                #"flag", "fval", "fval_unit")
      #init <- bquote(list(.(mthd), .(flag), NA_real_, NA_character_, FALSE))
      #e1  <- bquote(ft[ , .(c(out[4:7], "test")) := .(init)])
      #e2  <- bquote(ft[modl == "hill" & npts < 5 & hitc == 1, test := TRUE])
      #e3  <- bquote(ft[modl == "gnls" & npts < 7 & hitc == 1, test := TRUE])
      #e4  <- bquote(ft[npts > 1, cna := cnst_aic +  4/(npts - 2)])
      #e5  <- bquote(ft[npts > 4, hna := hill_aic + 40/(npts - 4)])
      #e6  <- bquote(ft[npts > 6, gna := gnls_aic + 84/(npts - 7)])
      #e7  <- bquote(ft[ , nma := pmin(cna, hna, gna, na.rm = TRUE)])
      #e8  <- bquote(ft[gna == nma, nmdl := "gnls"])
      #e9  <- bquote(ft[hna == nma, nmdl := "hill"])
      #e10 <- bquote(ft[cna == nma, nmdl := "cnst"])
      #e11 <- bquote(ft[ , nhc := FALSE])
      #e12 <- bquote(ft[nmdl == "hill" & hill_tp >= coff & max_med >= coff, 
                       #nhc := TRUE]) 
      #e13 <- bquote(ft[nmdl == "gnls" & gnls_tp >= coff & max_med >= coff, 
                       #nhc := TRUE])
      #e14 <- bquote(ft[hitc == 1 & !nhc, test := TRUE])
      #e15 <- bquote(f[[.(mthd)]] <- ft[which(test), .SD, .SDcols = .(out)])
      #cr <- c("mc6_mthd_id", "flag", "fval", "fval_unit", "test", 
              #"cna", "hna", "gna", "nma", "nmdl", "nhc")
      #e16 <- bquote(ft[ , .(cr) := NULL])
      #list(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, 
           #e11, e12, e13, e14, e15, e16)
      
    #},
    
    efficacy.50 = function(mthd) {
      
      flag <- "Less than 50% efficacy"
      out  <- c("m5id", "m4id", "aeid", "mc6_mthd_id", 
                "flag", "fval", "fval_unit")
      init <- bquote(list(.(mthd), .(flag), NA_real_, NA_character_, FALSE))
      e1 <- bquote(ft[ , .(c(out[4:7], "test")) := .(init)])
      e2 <- bquote(ft[hitc >= 0.9 & coff >= 5,
                      test := top < 50 | max_med < 50])
      e3 <- bquote(ft[hitc >= 0.9 & coff < 5,
                      test := top < log2(1.5) | max_med < log2(1.5)])
      e4 <- bquote(f[[.(mthd)]] <- ft[which(test), .SD, .SDcols = .(out)])
      cr <- c("mc6_mthd_id", "flag", "fval", "fval_unit", "test")
      e5 <- bquote(ft[ , .(cr) := NULL])
      list(e1, e2, e3, e4, e5)
      
    },
    
    ac50.lowconc = function(mthd) {
      
      flag <- "AC50 less than lowest concentration tested"
      out  <- c("m5id", "m4id", "aeid", "mc6_mthd_id", 
                "flag", "fval", "fval_unit")
      init <- bquote(list(.(mthd), .(flag), NA_real_, NA_character_, FALSE))
      e1 <- bquote(ft[ , .(c(out[4:7], "test")) := .(init)])
      e2 <- bquote(ft[hitc >= 0.9, test := ac50 < 10^logc_min]) 
      e3 <- bquote(f[[.(mthd)]] <- ft[which(test), .SD, .SDcols = .(out)])
      cr <- c("mc6_mthd_id", "flag", "fval", "fval_unit", "test")
      e4 <- bquote(ft[ , .(cr) := NULL])
      list(e1, e2, e3, e4)
      
    },
    
    viability.gnls = function(mthd) {
      
      flag <- "Cell viability assay fit with gnls winning model"
      out  <- c("m5id", "m4id", "aeid", "mc6_mthd_id", 
                "flag", "fval", "fval_unit")
      init <- bquote(list(.(mthd), .(flag), NA_real_, NA_character_, FALSE))
      e1 <- bquote(ft[ , .(c(out[4:7], "test")) := .(init)])
      e2 <- bquote(ft[hitc >= 0.9, test := modl=="gnls" & cell_viability_assay == 1])
      e3 <- bquote(f[[.(mthd)]] <- ft[which(test), .SD, .SDcols = .(out)])
      cr <- c("mc6_mthd_id", "flag", "fval", "fval_unit", "test")
      e4 <- bquote(ft[ , .(cr) := NULL])
      list(e1, e2, e3, e4)
      
    }
    
  )
  
}

#-------------------------------------------------------------------------------
