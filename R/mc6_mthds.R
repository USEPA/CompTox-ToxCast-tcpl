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
#' methods get applied to each aeid.
#' 
#' @section Available Methods:
#' 
#' More information about the level 6 multiple-concentration processing is 
#' available in the package vignette, "Data_processing."
#' 
#' \describe{
#'   \item{modl.directionality.fail}{Flag series if model directionality is questionable, i.e. if 
#'   the winning model direction was opposite, more responses (resp) would have exceeded the cutoff 
#'   (coff). If loss was winning directionality (top < 0), flag if 
#'    \eqn{count(resp<-1*coff)<2*count(resp>coff)}{count(resp < -1(coff)) < 2(count(resp > coff))}. 
#'   If gain was winning directionality (top > 0), flag if 
#'    \eqn{count(resp>coff)<2*count(resp<-1*coff)}.}
#'   \item{low.nrep}{Flag series if the average number of replicates per concentration is less than 
#'   2; \eqn{nrep < 2}{nrep < 2}.}
#'   \item{low.nconc}{Flag series if 4 concentrations or less were tested; \eqn{nconc<=4}{nconc<=4}.
#'   }
#'   \item{bmd.high}{Flag series if modeled benchmark dose (BMD) is greater than AC50 
#'   (concentration at 50 percent maximal response). This is indicates high variability in baseline 
#'   response in excess of more than half of the maximal response.}
#'   \item{singlept.hit.high}{Flag single-point hit that's only at the highest conc tested, where 
#'   series is an active hit call (hitc >= 0.9) with the median response observed above baseline 
#'   occurring only at the highest tested concentration tested.}
#'   \item{singlept.hit.mid}{Flag single-point hit that's not at the highest conc tested, where 
#'   series is an active hit call (hitc >= 0.9) with the median response observed above baseline 
#'   occurring only at one concentration and not the highest concentration tested.}
#'   \item{multipoint.neg}{Flag multi-point miss, where series is an inactive hit call (hitc < 0.9) 
#'   with multiple median responses observed above baseline.}
#'   \item{gnls.lowconc}{Flag series where winning model is gain-loss (gnls) and the gain AC50 is 
#'   less than the minimum tested concentration, and the loss AC50 is less than the mean tested 
#'   concentration.}
#'   \item{noise}{Flag series as noisy if the quality of fit as calculated by the root mean square 
#'   error (rmse) for the series is greater than the cutoff (coff); \eqn{rmse > coff}{rmse > coff}.}
#'   \item{border}{Flag series if borderline activity is suspected based on modeled top parameter 
#'   (top) relative to cutoff (coff); \eqn{0.8*coff<=|top|<=1.2*coff}.}
#'   \item{overfit.hit}{Method not yet updated for tcpl implementation. Flag hit-calls that would 
#'   get changed after doing the small N correction to the aic values.}
#'   \item{efficacy.50}{Flag low efficacy hits if series has an active hit call (hitc >= 0.9) and 
#'   efficacy values (e.g. top and maximum median response) less than 50 percent; intended for 
#'   biochemical assays. If \eqn{hitc>=0.9}{hitc>=0.9} and \eqn{coff>=5}{coff>=5}, then flag when 
#'    \eqn{top<50}{top<50} or \eqn{maxmed < 50}{ma_med < 50}. If \eqn{hitc>=0.9}{hitc>=0.9} and 
#'    \eqn{coff<5}{coff<5}, then flag when \eqn{top<\log_{2}{1.5}}{top<log2(1.5)} or 
#'    \eqn{maxmed<\log_{2}{1.5}}{max_med<log2(1.5)}.}
#'   \item{ac50.lowconc}{Flag series with an active hit call (hitc >= 0.9) if AC50 (concentration 
#'   at 50 percent maximal response) is less than the lowest concentration tested; if 
#'   \eqn{hitc>=0.9}{hitc>=0.9} and \eqn{ac50<10^{log_{c}{min}}}{ac50<10^logc_min}, then flag.}
#'   \item{viability.gnls}{Flag series with an active hit call (hitc >= 0.9) if denoted as cell 
#'   viability assay with winning model is gain-loss (gnls); if hitc >= 0.9, modl = "gnls" and 
#'   cell_viability_assay = 1, then flag.}
#'   \item{no.med.gt.3bmad}{Flag series where no median response values are greater than baseline as 
#'   defined by 3 times the baseline median absolute deviation (bmad); nmed_gtbl_pos and
#'   nmed_gtbl_neg both = 0, where nmed_gtbl_pos/_neg is the number of medians greater than 3 * 
#'   bmad/less than -3 * bmad.}
#' }
#' 
#' @note
#' This function is not exported and is not intended to be used by the user.

mc6_mthds <- function() {
  
  list(
    
    modl.directionality.fail = function(mthd) {
      
      flag <- "Model directionality questionable"
      out  <- c("m5id", "m4id", "aeid", "mc6_mthd_id", 
                "flag")
      init <- bquote(list(.(mthd), .(flag), FALSE))
      e1 <- bquote(dr[ , .(c(out[4:5], "test")) := .(init)])
      e2 <- bquote(dr[ , coffsign := ifelse(top < 0, -1*coff, coff)])
      e3 <- bquote(dr[ , gtabscoff := abs(resp) > abs(coffsign)])
      e4 <- bquote(dr[ , nrsp_gtabscoff := sum(gtabscoff), by = m4id])
      e5 <- bquote(dr[ , gtcoff := resp > coffsign])
      e6 <- bquote(dr[ , ltcoff := resp < coffsign])
      e7 <- bquote(dr[ , nrsp_gtcoff := sum(gtcoff), by = m4id])
      e8 <- bquote(dr[ , nrsp_ltcoff := sum(ltcoff), by = m4id])
      e9 <- bquote(dr[ , test := (hitc > 0.9) & ifelse(coffsign > 0, nrsp_gtcoff > 1, nrsps_ltcoff > 1) & ifelse(coffsign > 0, nrsp_gtabscoff > 2*nrsp_gtcoff, nrsp_gtabscoff > 2*nrsp_ltcoff)])
      e10 <- bquote(f[[.(mthd)]] <- unique(dr[which(test), .SD, .SDcols = .(out)], by = NULL))
      cr <- c("mc6_mthd_id", "flag", "test", "coffsign", "gtabscoff", "nrsp_gtabscoff", "gtcoff", "ltcoff", "nrsp_gtcoff", "nrsp_ltcoff")
      e11 <- bquote(dr[ , .(cr) := NULL])
      list(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11)
      
    },
    
    low.nrep = function(mthd) { 
      
      flag <- "Average number of replicates per conc < 2"
      out  <- c("m5id", "m4id", "aeid", "mc6_mthd_id", 
                "flag")
      init <- bquote(list(.(mthd), .(flag), FALSE))
      e1 <- bquote(ft[ , .(c(out[4:5], "test")) := .(init)])
      e2 <- bquote(ft[ , test := nrep < 2])
      e3 <- bquote(f[[.(mthd)]] <- ft[which(test), .SD, .SDcols = .(out)])
      cr <- c("mc6_mthd_id", "flag", "test")
      e4 <- bquote(ft[ , .(cr) := NULL])
      list(e1, e2, e3, e4)
      
    },
    
    low.nconc = function(mthd) { 
      
      flag <- "Number of concentrations tested < 4"
      out  <- c("m5id", "m4id", "aeid", "mc6_mthd_id", 
                "flag")
      init <- bquote(list(.(mthd), .(flag), FALSE))
      e1 <- bquote(ft[ , .(c(out[4:5], "test")) := .(init)])
      e2 <- bquote(ft[ , test := modl != "none" & nconc <= 4])
      e3 <- bquote(f[[.(mthd)]] <- ft[which(test), .SD, .SDcols = .(out)])
      cr <- c("mc6_mthd_id", "flag", "test")
      e4 <- bquote(ft[ , .(cr) := NULL])
      list(e1, e2, e3, e4)
      
    },

    bmd.high = function(mthd) {
      
      flag <- "Bmd > ac50, indication of high baseline variability"
      out  <- c("m5id", "m4id", "aeid", "mc6_mthd_id", 
                "flag")
      init <- bquote(list(.(mthd), .(flag), FALSE))
      e1 <- bquote(ft[ , .(c(out[4:5], "test")) := .(init)])
      e2 <- bquote(ifelse(all(c("ac50","bmd") %in% names(ft)),ft[ , test := bmd > ac50],ft))
      e3 <- bquote(f[[.(mthd)]] <- ft[which(test), .SD, .SDcols = .(out)])
      cr <- c("mc6_mthd_id", "flag", "test")
      e4 <- bquote(ft[ , .(cr) := NULL])
      list(e1, e2, e3, e4)
      
    },
    
    singlept.hit.high = function(mthd) {
      
      flag <- "Active with only highest conc above baseline (3*bmad)"
      out  <- c("m5id", "m4id", "aeid", "mc6_mthd_id", 
                "flag")
      init <- bquote(list(.(mthd), .(flag), FALSE))
      e1 <- bquote(ft[ , .(c(out[4:5], "test")) := .(init)])
      e2 <- bquote(ft[ , lstc := max_med_diff_conc == conc_max])
      e3 <- bquote(ft[ , test := ((nmed_gtbl_pos == 1 & model_type == 2 & top >= 0) |
                                  (nmed_gtbl_neg == 1 & model_type == 2 & top <= 0) | 
                                  (nmed_gtbl_pos == 1 & model_type == 3) | 
                                  (nmed_gtbl_neg == 1 & model_type == 4)) & hitc >= 0.9 & lstc])
      e4 <- bquote(f[[.(mthd)]] <- ft[which(test), .SD, .SDcols = .(out)])
      cr <- c("mc6_mthd_id", "flag", "test", "lstc")
      e5 <- bquote(ft[ , .(cr) := NULL])
      list(e1, e2, e3, e4, e5)
      
    },
    
    singlept.hit.mid = function(mthd) {
      
      flag <- "Active with one conc (not highest) above baseline (3*bmad)"
      out  <- c("m5id", "m4id", "aeid", "mc6_mthd_id", 
                "flag")
      init <- bquote(list(.(mthd), .(flag), FALSE))
      e1 <- bquote(ft[ , .(c(out[4:5], "test")) := .(init)])
      e2 <- bquote(ft[ , lstc := max_med_diff_conc == conc_max])
      e3 <- bquote(ft[ , test := ((nmed_gtbl_pos == 1 & model_type == 2 & top >= 0) |
                                  (nmed_gtbl_neg == 1 & model_type == 2 & top <= 0) | 
                                  (nmed_gtbl_pos == 1 & model_type == 3) | 
                                  (nmed_gtbl_neg == 1 & model_type == 4)) & hitc >= 0.9 & !lstc])
      e4 <- bquote(f[[.(mthd)]] <- ft[which(test), .SD, .SDcols = .(out)])
      cr <- c("mc6_mthd_id", "flag", "test", "lstc")
      e5 <- bquote(ft[ , .(cr) := NULL])
      list(e1, e2, e3, e4, e5)
      
    },
    
    multipoint.neg = function(mthd) {
      
      flag <- "Inactive with multiple concs above baseline (3*bmad)"
      out  <- c("m5id", "m4id", "aeid", "mc6_mthd_id", 
                "flag")
      init <- bquote(list(.(mthd), .(flag), FALSE))
      e1 <- bquote(ft[ , .(c(out[4:5], "test")) := .(init)])
      e2 <- bquote(ft[ , test := ((nmed_gtbl_pos > 1 & model_type == 2 & top >= 0) |
                                  (nmed_gtbl_neg > 1 & model_type == 2 & top <= 0) | 
                                  (nmed_gtbl_pos > 1 & model_type == 3) | 
                                  (nmed_gtbl_neg > 1 & model_type == 4)) & hitc < 0.9 & hitc >= 0])
      e3 <- bquote(f[[.(mthd)]] <- ft[which(test), .SD, .SDcols = .(out)])
      cr <- c("mc6_mthd_id", "flag", "test")
      e4 <- bquote(ft[ , .(cr) := NULL])
      list(e1, e2, e3, e4)
      
    },
    
    gnls.lowconc = function(mthd) {
      
      flag <- "Gain AC50 < lowest conc & loss AC50 < mean conc"
      out  <- c("m5id", "m4id", "aeid", "mc6_mthd_id", 
                "flag")
      init <- bquote(list(.(mthd), .(flag), FALSE))
      e1 <- bquote(ft[ , .(c(out[4:5], "test")) := .(init)])
      e2 <- bquote(ft[ , c_min := conc_min])
      e3 <- bquote(ft[ , c_max := conc_max])
      conc_cols <- c("c_min", "c_max")
      e4 <- bquote(ft[ , cmen := rowMeans(.SD), .SDcols = .(conc_cols)])
      e5 <- bquote(ifelse("ac50_loss" %in% names(ft), ft[ , test := modl == "gnls" & ac50 < c_min & ac50_loss < cmen], ft))
      e6 <- bquote(f[[.(mthd)]] <- ft[which(test), .SD, .SDcols = .(out)])
      cr <- c("mc6_mthd_id", "flag", "test", "c_min", "c_max","cmen")
      e7 <- bquote(ft[ , .(cr) := NULL])
      list(e1, e2, e3, e4, e5, e6, e7)
      
    },
    
    noise = function(mthd) {
      
      flag <- "Noisy data"
      out  <- c("m5id", "m4id", "aeid", "mc6_mthd_id", 
                "flag")
      init <- bquote(list(.(mthd), .(flag), FALSE))
      e1 <- bquote(ft[ , .(c(out[4:5], "test")) := .(init)])
      e2 <- bquote(ft[ , test := rmse > coff])
      e3 <- bquote(f[[.(mthd)]] <- ft[which(test), .SD, .SDcols = .(out)])
      cr <- c("mc6_mthd_id", "flag", "test")
      e4 <- bquote(ft[ , .(cr) := NULL])
      list(e1, e2, e3, e4)
      
    },    
    
    border = function(mthd) {
      
      flag <- "Borderline"
      out  <- c("m5id", "m4id", "aeid", "mc6_mthd_id", 
                "flag")
      init <- bquote(list(.(mthd), .(flag), FALSE))
      e1 <- bquote(ft[ , .(c(out[4:5], "test")) := .(init)])
      e2 <- bquote(ft[ , test := abs(top) <= 1.2*coff & abs(top) >= 0.8*coff])
      e3 <- bquote(f[[.(mthd)]] <- ft[which(test), .SD, .SDcols = .(out)])
      cr <- c("mc6_mthd_id", "flag", "test")
      e4 <- bquote(ft[ , .(cr) := NULL])
      list(e1, e2, e3, e4)
      
    },
    
    #overfit.hit = function(mthd) {
      
      #flag <- "Hit-call potentially confounded by overfitting"
      #out  <- c("m5id", "m4id", "aeid", "mc6_mthd_id", 
                #"flag")
      #init <- bquote(list(.(mthd), .(flag), FALSE))
      #e1  <- bquote(ft[ , .(c(out[4:5], "test")) := .(init)])
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
      #cr <- c("mc6_mthd_id", "flag", "test", 
              #"cna", "hna", "gna", "nma", "nmdl", "nhc")
      #e16 <- bquote(ft[ , .(cr) := NULL])
      #list(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, 
           #e11, e12, e13, e14, e15, e16)
      #
    #},
    
    efficacy.50 = function(mthd) {
      
      flag <- "Efficacy < 50%"
      out  <- c("m5id", "m4id", "aeid", "mc6_mthd_id", 
                "flag")
      init <- bquote(list(.(mthd), .(flag), FALSE))
      e1 <- bquote(ft[ , .(c(out[4:5], "test")) := .(init)])
      e2 <- bquote(ft[hitc >= 0.9 & coff >= 5,
                      test := abs(top) < 50 | abs(max_med_diff) < 50])
      e3 <- bquote(ft[hitc >= 0.9 & coff < 5,
                      test := abs(top) < log2(1.5) | abs(max_med_diff) < log2(1.5)])
      e4 <- bquote(f[[.(mthd)]] <- ft[which(test), .SD, .SDcols = .(out)])
      cr <- c("mc6_mthd_id", "flag", "test")
      e5 <- bquote(ft[ , .(cr) := NULL])
      list(e1, e2, e3, e4, e5)
      
    },
    
    ac50.lowconc = function(mthd) {
      
      flag <- "AC50 < lowest concentration tested"
      out  <- c("m5id", "m4id", "aeid", "mc6_mthd_id", 
                "flag")
      init <- bquote(list(.(mthd), .(flag), FALSE))
      e1 <- bquote(ft[ , .(c(out[4:5], "test")) := .(init)])
      e2 <- bquote(ft[hitc >= 0.9, test := ac50 < conc_min]) 
      e3 <- bquote(f[[.(mthd)]] <- ft[which(test), .SD, .SDcols = .(out)])
      cr <- c("mc6_mthd_id", "flag", "test")
      e4 <- bquote(ft[ , .(cr) := NULL])
      list(e1, e2, e3, e4)
      
    },
    
    viability.gnls = function(mthd) {
      
      flag <- "Cell viability assay fit with gnls winning model"
      out  <- c("m5id", "m4id", "aeid", "mc6_mthd_id", 
                "flag")
      init <- bquote(list(.(mthd), .(flag), FALSE))
      e1 <- bquote(ft[ , .(c(out[4:5], "test")) := .(init)])
      e2 <- bquote(ft[hitc >= 0.9, test := modl=="gnls" & cell_viability_assay == 1])
      e3 <- bquote(f[[.(mthd)]] <- ft[which(test), .SD, .SDcols = .(out)])
      cr <- c("mc6_mthd_id", "flag", "test")
      e4 <- bquote(ft[ , .(cr) := NULL])
      list(e1, e2, e3, e4)
      
    },
    
    no.med.gt.3bmad = function(mthd) {
      
      flag <- "No median responses above baseline (3*bmad)"
      out  <- c("m5id", "m4id", "aeid", "mc6_mthd_id", 
                "flag")
      init <- bquote(list(.(mthd), .(flag), FALSE))
      e1 <- bquote(ft[ , .(c(out[4:5], "test")) := .(init)])
      e2 <- bquote(ft[ , test := (model_type == 2 & top > 0 & nmed_gtbl_pos == 0) | 
                                 (model_type == 2 & top < 0 & nmed_gtbl_neg == 0) | 
                                 (model_type == 3 & nmed_gtbl_pos == 0) | 
                                 (model_type == 4 & nmed_gtbl_neg == 0)])
      e3 <- bquote(f[[.(mthd)]] <- ft[which(test), .SD, .SDcols = .(out)])
      cr <- c("mc6_mthd_id", "flag", "test")
      e4 <- bquote(ft[ , .(cr) := NULL])
      list(e1, e2, e3, e4)
      
    }
    
  )
  
}

#-------------------------------------------------------------------------------
