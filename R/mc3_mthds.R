#-------------------------------------------------------------------------------
# mc3_mthds: List of normalization methods (to be used at level 3)
#-------------------------------------------------------------------------------

#' @name MC3_Methods
#' @title List of level 3 multiple-concentration normalization methods
#' 
#' @description 
#' \code{mc3_mthds} returns a list of normalization methods to be used 
#' during level 3 multiple-concentration processing.
#' 
#' @return A list of functions
#' 
#' @seealso \code{\link{mc3}}, \code{\link{Method functions}} to query what 
#' methods get applied to each aeid
#' 
#' @details
#' The functions contained in the list returned by \code{mc3_mthds} take 
#' \code{aeids} (a numeric vector of aeid values) and returns a list of expressions 
#' to be executed in the \code{mc3} (not exported) function environment. The 
#' functions are described here for reference purposes, The 
#' \code{mc3_mthds} function is not exported, nor is it intended for use.
#' 
#' All available methods are described in the Available Methods section, listed
#' by the type of function and the function/method name. 
#' 
#' @section Available Methods:
#' 
#' The methods are broken into three types, based on what fields they define. 
#' Different methods are used to define "bval" (the baseline value), "pval"
#' (the positive control value), and "resp" (the final response value). 
#' 
#' Although it does not say so specifically in each description, all methods 
#' are applied by aeid.
#' 
#' More information about the level 3 multiple-concentration processing is 
#' available in the package vignette, "Data_processing."
#' 
#' \subsection{bval Methods}{
#'   \describe{
#'     \item{bval.apid.nwlls.med}{Calculate the baseline value (bval) as the plate-wise median, by 
#'     assay plate ID (apid), of the corrected values (cval) for neutral control wells (wllt = n).}
#'     \item{bval.apid.lowconc.med}{Calculate the baseline value (bval) as the plate-wise median, 
#'     by assay plate ID (apid), of the corrected values (cval) for test compound wells (wllt = t) 
#'     with a concentration index (cndx) of 1 or 2.}
#'     \item{bval.apid.twlls.med}{Calculate the baseline value (bval) as the plate-wise median, by 
#'     assay plate ID (apid), of the corrected values (cval) of test compound wells (wllt = t).}
#'     \item{bval.apid.tn.med}{Calculate the baseline value (bval) as the plate-wise median, by 
#'     assay plate ID (apid), of the corrected values (cval) for test compound wells (wllt = t) and 
#'     neutral control wells (wllt = n).}
#'     \item{bval.apid.nwllslowconc.med}{Calculate the baseline value (bval) as the plate-wise 
#'     median, by assay plate ID (apid), of the corrected values (cval) of test compound wells 
#'     (wllt = t) with a concentration index (cndx) of 1 or 2 or neutral control wells (wllt = n).}
#'     \item{bval.spid.lowconc.med}{Calculate the baseline value (bval) as the sample-wise median, 
#'     by sample ID (spid), of the corrected values (cval) of the three lowest concentration test 
#'     compound wells (wllt = t and cndx =  1, 2, & 3).}
#'     \item{bval.apid.nwllstcwllslowconc.med}{Calculate the baseline value (bval) as the 
#'     plate-wise median, by assay plate ID (apid), of the corrected values (cval) for neutral 
#'     control wells (wllt = n) or wells with a concentration index (cndx) of 1 or 2 and well 
#'     type of test compound (wllt = t) or gain-of-signal control in multiple concentrations 
#'     (wllt = c).}
#'     \item{bval.aeid.nwlls.med}{Calculate the baseline value (bval) as the endpoint-wise median, 
#'     by assay component endpoint ID (aeid), corrected value (cval) for neutral control wells 
#'     (wllt = n).}
#'      
#'   }
#' } 
#' 
#' \subsection{pval Methods}{
#'   \describe{
#'     \item{pval.apid.pwlls.med}{Calculate the positive control value (pval) as the plate-wise 
#'     median, by assay plate ID (apid), of the corrected values (cval) for single-concentration 
#'     gain-of-signal positive control wells (wllt = p).}
#'     \item{pval.apid.mwlls.med}{Calculate the positive control value (pval) as the plate-wise 
#'     median, by assay plate ID (apid), of the corrected values (cval) for multiple-concentration 
#'     loss-of-signal negative control wells (wllt = m).}
#'     \item{pval.apid.medpcbyconc.max}{Calculate the positive control value (pval) as the 
#'     plate-wise maximum, by assay plate ID (apid), of the medians of the corrected values (cval) 
#'     for gain-of-signal single- or multiple-concentration negative control wells (wllt = m or o) 
#'     by apid, well type, and concentration.}
#'     \item{pval.apid.medpcbyconc.min}{Calculate the positive control value (pval) as the 
#'     plate-wise minimum, by assay plate ID (apid), of the medians of corrected value (cval) of 
#'     gain-of-signal single- or multiple-concentration positive control wells (wllt = p or c) by 
#'     apid, well type, and concentration.}
#'     \item{pval.apid.medncbyconc.min}{Calculate the positive control value (pval) as the 
#'     plate-wise minimum, by assay plate ID (apid), of the medians of the corrected values (cval) 
#'     for gain-of-signal single- or multiple-concentration negative control wells (wllt = m or o) 
#'     by apid, well type, and concentration.}
#'     \item{pval.apid.pmv.min}{Calculate the positive control value (pval) as the plate-wise 
#'     minimum, by assay plate ID (apid), of the medians of the corrected values (cval) for 
#'     single-concentration gain-of-signal, multiple-concentration loss-of-signal, or viability 
#'     control wells (wllt = p, m, or v) by apid, well type, and concentration.}
#'     \item{pval.apid.pmv.max}{Calculate the positive control value (pval) as the plate-wise 
#'     maximum, by assay plate ID (apid), of the medians of the corrected values (cval) for 
#'     single-concentration gain-of-signal, multiple-concentration loss-of-signal, or viability 
#'     control wells (wllt = p, m, or v) by apid, well type, and concentration.}
#'     \item{pval.apid.f.max}{Calculate the positive control value (pval) as the plate-wise 
#'     maximum, by assay plate ID (apid), of the medians of important reference wells (wllt = f) 
#'     values by apid and concentration.}
#'     \item{pval.apid.f.min}{Calculate the positive control value (pval) as the plate-wise 
#'     minimum, by assay plate ID (apid), of the medians of important reference wells (wllt = f) 
#'     values by apid and concentration.}
#'     \item{pval.apid.p.max}{Calculate the positive control value (pval) as the plate-wise 
#'     maximum, by assay plate ID (apid), of the medians of the corrected values (cval) for 
#'     single-concentration gain-of-signal control wells (wllt = p) by apid.}
#'     \item{pval.apid.p.min}{Calculate the positive control value (pval) as the plate-wise 
#'     minimum, by assay plate ID (apid), of the medians of corrected values (cval) for 
#'     single-concentration gain-of-signal control wells (wllt = p) by apid.}
#'     \item{pval.apid.v.min}{Calculate the positive control value (pval) as the plate-wise 
#'     minimum, by assay plate ID (apid), of the medians of the corrected values (cval) for 
#'     viability control wells (wllt = v) by apid and concentration.}
#'     \item{pval.zero}{Set the positive control value (pval) to 0; \eqn{pval = 0}{pval = 0}.}
#'     \item{pval.apid.owlls.med}{Calculate the positive control value (pval) as the plate-wise 
#'     median, by assay plate ID (apid), of the corrected values (cval) for single-concentration 
#'     negative control wells (wllt = o).}
#'     \item{pval.2bval}{Calculate the positive control value (pval) as the plate-wise median, by 
#'     assay plate ID (apid), of the corrected values (cval) for neutral control wells (wllt = n) 
#'     multiplied by 2.}
#'     \item{pval.maxp}{Calculate the positive control value (pval) as the endpoint-wise maximum, 
#'     by assay component ID (aeid), of the corrected values for single-concentration 
#'     gain-of-signal wells (wllt = p).}
#'     \item{pval.apid.bwlls.med}{Calculate the positive control value (pval) as the plate-wise 
#'     median, by assay plate ID (apid), of the corrected values (cval) for blank wells (wllt= b).}
#'     \item{pval.twlls.99pct}{Calculate positive control value (pval) as the 99th percentile of 
#'     all corrected value (cvals) of the test compound wells (wllt = t).}
#'     \item{pval.neg.100}{Calculate positive control value (pval) as -100 for endpoints in the 
#'     down direction; \eqn{pval = -100}{pval = -100}.}
#'   }
#' } 
#' 
#' \subsection{resp Methods}{
#'   \describe{
#'     \item{resp.pc}{Calculate the normalized response (resp) as a percent of control, i.e. the 
#'     ratio of the difference between the corrected (cval) and baseline (bval) values divided the 
#'     difference between the positive control (pval) and baseline (bval) values multiplied by 100; 
#'     \eqn{resp = (cval-bval)/(pval-bval)*100}{resp = (cval-bval)/(pval-bval)*100}.}
#'     \item{resp.pc.pval.cor}{Calculate the normalized response (resp) as a percent of control, 
#'     i.e. the ratio of the difference between the corrected (cval) and baseline (bval) values 
#'     divided the positive control (pval) value multiplied by 100; 
#'     \eqn{resp = (cval-bval)/pval*100}{resp = (cval-bval)/pval*100}.}
#'     \item{resp.fc}{Calculate the normalized response (resp) as the fold change, i.e. the ratio 
#'     of the corrected (cval) and baseline (bval) values; \eqn{resp = cval/bal}{resp = cval/bal}.}
#'     \item{resp.logfc}{Calculate the normalized response (resp) as the fold change of logged,
#'      i.e. the difference between corrected (cval) and baseline (bval) log-scale values.}
#'     \item{resp.log2}{Transform the response values to log-scale (base 2).}
#'     \item{resp.mult25}{Multiply the normalized response value (resp) by 25; 
#'      \eqn{25*resp}{25*resp}.}
#'     \item{resp.scale.mad.log2fc}{Scale the normalized response value (resp) by the ratio of 
#'     log2(1.2) and 3 multiplied by the baseline median absolute deviation (bmad) of the unscaled 
#'     normalized response values (resp); \eqn{(\log_{2}{1.2})/3*bmad*resp}{log2(1.2)/3*bmad*resp}.}
#'     \item{resp.scale.quant.log2fc}{Scale the normalized response value (resp). First, determine 
#'     the maximum difference (md) by finding the maximum between the absolute difference of the 
#'     1st percentile minus the 50th percentile and the absolute difference of the 99th percentile 
#'     minus the 50th percentile. Then multiply resp by log2(1.2) divided by 20 percent of md; 
#'     \eqn{(\log_{2}{1.2})/0.2*md*resp}{log2(1.2)/0.2*md*resp}.}
#'     \item{resp.multneg1}{Multiply the normalized response value (resp) by -1; 
#'      \eqn{-1*resp}{-1*resp}.}
#'     \item{resp.shiftneg.3bmad}{Shift all the normalized response values (resp) less than -3 
#'     multiplied by the baseline median absolute deviation (bmad) to 0; if 
#'      \eqn{resp < -3*bmad,~resp = 0}{resp < -3*bmad, resp = 0}.}
#'     \item{resp.shiftneg.6bmad}{Shift all the normalized response values (resp) less than -6 
#'     multiplied by the baseline median absolute deviation (bmad) to 0; if 
#'      \eqn{resp < -6*bmad,~resp = 0}{resp < -6*bmad, resp = 0}.}
#'     \item{resp.shiftneg.10bmad}{Shift all the normalized response values (resp) less than 10 
#'     multiplied by the baseline median absolute deviation (bmad) to 0; if 
#'      \eqn{resp < -10*bmad,~resp = 0}{resp < -10*bmad, resp = 0}.}
#'     \item{resp.blineshift.3bmad.repi}{Shift the normalized response value (resp) with a baseline 
#'     correction, by replicate index (repi), with a window of 3 multiplied by the baseline median 
#'     absolute deviation (bmad).}
#'     \item{resp.blineshift.50.repi}{Shift the normalized response value (resp) with a baseline 
#'     correction, by replicate index (repi), with a window of 50.}
#'     \item{resp.blineshift.3bmad.spid}{Shift the normalized response value (resp) with a baseline 
#'     correction, by sample ID (spid), with a window of 3 multiplied by the baseline median 
#'     absolute deviation (bmad).}
#'     \item{resp.blineshift.50.spid}{Shift the normalized response value (resp) with a baseline 
#'     correction, by sample ID (spid), with a window of 50.}
#'     \item{none}{Set the corrected response value (cval) as the normalized response value (resp); 
#'     \eqn{cval = resp}{cval = resp}. No additional mc3 methods needed for endpoint-specific 
#'     normalization.}
#'     \item{resp.zerocenter.fc}{Calculate the normalized response (resp) as a zero center fold 
#'     change, i.e. 1 minus the ratio of corrected (cval) and baseline (bval) values; 
#'      \eqn{resp=1-cval/bval}{resp = 1 - cval/bval}. Typically used for increasing responses.}
#'     \item{resp.incr.zerocenter.fc}{Calculate the normalized response (resp) as a zero center 
#'     fold change, i.e. the ratio of the the corrected (cval) and baseline (bval) values minus 1; 
#'      \eqn{resp=cval/bval-1}{resp = cval/bval - 1}. Typically used for increasing responses.}
#'     \item{resp.mult100}{Multiply the normalized response value (resp) by 100; 
#'      \eqn{100*resp}{100*resp}.}
#'    `\item{resp.censormed.neg25}{Censor (remove) response values from 
#'      concentrations which median falls below -25.}
#'   }
#' }
#'
#' @note
#' This function is not exported and is not intended to be used by the user.


mc3_mthds <- function() {
  
  list(
    
    bval.apid.nwlls.med = function(aeids) {
      e1 <- bquote(dat[J(.(aeids)), 
                       bval := median(cval[wllt == "n"], na.rm = TRUE), 
                       by = list(aeid, apid)])
      list(e1)
      
    },    
    
    bval.apid.lowconc.med = function(aeids) {
            
      e1 <- bquote(dat[J(.(aeids)), 
                       bval := median(cval[cndx %in% 1:2 & wllt == "t"],
                                      na.rm = TRUE),
                       by = list(aeid, apid)])
      list(e1)
      
    },
    
    bval.apid.twlls.med = function(aeids) {
            
      e1 <- bquote(dat[J(.(aeids)), 
                       bval := median(cval[wllt == "t"], na.rm = TRUE),
                       by = list(aeid, apid)])
      list(e1)
      
    },
    
    bval.apid.tn.med = function(aeids) {
            
      e1 <- bquote(dat[J(.(aeids)), 
                       bval := median(cval[wllt %in% c("t", "n")], 
                                      na.rm = TRUE),
                       by = list(aeid, apid)])
      list(e1)
      
    },
    
    bval.apid.nwllslowconc.med = function(aeids) {
      
      e1 <- bquote(dat[J(.(aeids)), 
                       bval := median(cval[(cndx %in% 1:2 & wllt == "t") | 
                                             wllt == "n"],
                                      na.rm = TRUE),
                       by = list(aeid, apid)])
      list(e1)
      
    },
    
    bval.spid.lowconc.med = function(aeids) {
            
      e1 <- bquote(dat[J(.(aeids)), 
                       bval := median(cval[cndx %in% 1:3 & wllt == "t"],
                                      na.rm = TRUE),
                       by = list(aeid, spid, repi)])
      list(e1)
      
    },
    
    pval.apid.pwlls.med = function(aeids) {
      
      e1 <- bquote(dat[J(.(aeids)), 
                       pval := median(cval[wllt == "p"], na.rm = TRUE), 
                       by = list(aeid, apid)])
      list(e1)
      
    },
    
    pval.apid.mwlls.med = function(aeids) {
      
      e1 <- bquote(dat[J(.(aeids)), 
                       pval := median(cval[wllt == "m"], na.rm = TRUE), 
                       by = list(aeid, apid)])
      list(e1)
      
    },
    
    pval.apid.medpcbyconc.max = function(aeids) {
            
      e1 <- bquote(dat[J(.(aeids)),
                       temp := median(cval[wllt %in% c("c", "p")], 
                                      na.rm = TRUE),
                       by = list(aeid, apid, wllt, conc)])
      e2 <- bquote(dat[J(.(aeids)),
                       pval := max(temp, na.rm = TRUE),
                       by = list(aeid, apid)])
      e3 <- bquote(dat[ , temp := NULL])
      list(e1, e2, e3)
      
    },
    
    pval.apid.medpcbyconc.min = function(aeids) {
      
      e1 <- bquote(dat[J(.(aeids)),
                       temp := median(cval[wllt %in% c("c", "p")], 
                                      na.rm = TRUE),
                       by = list(aeid, apid, wllt, conc)])
      e2 <- bquote(dat[J(.(aeids)),
                       pval := min(temp, na.rm = TRUE),
                       by = list(aeid, apid)])
      e3 <- bquote(dat[ , temp := NULL])
      list(e1, e2, e3)
      
    },
    
    pval.apid.medncbyconc.min = function(aeids) {
      
      e1 <- bquote(dat[J(.(aeids)),
                       temp := median(cval[wllt %in% c("m","o")], 
                                      na.rm = TRUE),
                       by = list(aeid, apid, wllt, conc)])
      e2 <- bquote(dat[J(.(aeids)),
                       pval := min(temp, na.rm = TRUE),
                       by = list(aeid, apid)])
      e3 <- bquote(dat[ , temp := NULL])
      list(e1, e2, e3)
      
    },
    
    pval.apid.pmv.min = function(aeids) {
      
      e1 <- bquote(dat[J(.(aeids)),
                       temp := median(cval[wllt %in% c("p", "m", "v")], 
                                      na.rm = TRUE),
                       by = list(aeid, apid, wllt, conc)])
      e2 <- bquote(dat[J(.(aeids)),
                       pval := min(temp, na.rm = TRUE),
                       by = list(aeid, apid)])
      e3 <- bquote(dat[ , temp := NULL])
      list(e1, e2, e3)
      
    },
    
    pval.apid.pmv.max = function(aeids) {
      
      e1 <- bquote(dat[J(.(aeids)),
                       temp := median(cval[wllt %in% c("p", "m", "v")], 
                                      na.rm = TRUE),
                       by = list(aeid, apid, wllt, conc)])
      e2 <- bquote(dat[J(.(aeids)),
                       pval := max(temp, na.rm = TRUE),
                       by = list(aeid, apid)])
      e3 <- bquote(dat[ , temp := NULL])
      list(e1, e2, e3)
      
    },
    
    pval.apid.f.max = function(aeids) {
      
      e1 <- bquote(dat[J(.(aeids)),
                       temp := median(cval[wllt == "f"], na.rm = TRUE),
                       by = list(aeid, apid, conc)])
      e2 <- bquote(dat[J(.(aeids)),
                       pval := max(temp, na.rm = TRUE),
                       by = list(aeid, apid)])
      e3 <- bquote(dat[ , temp := NULL])
      list(e1, e2, e3)
      
    },
    
    pval.apid.f.min = function(aeids) {
      
      e1 <- bquote(dat[J(.(aeids)),
                       temp := median(cval[wllt == "f"], na.rm = TRUE),
                       by = list(aeid, apid, conc)])
      e2 <- bquote(dat[J(.(aeids)),
                       pval := min(temp, na.rm = TRUE),
                       by = list(aeid, apid)])
      e3 <- bquote(dat[ , temp := NULL])
      list(e1, e2, e3)
      
    },
    
    pval.apid.p.max = function(aeids) {
      
      e1 <- bquote(dat[J(.(aeids)),
                       temp := median(cval[wllt == "p"], na.rm = TRUE),
                       by = list(aeid, apid, conc)])
      e2 <- bquote(dat[J(.(aeids)),
                       pval := max(temp, na.rm = TRUE),
                       by = list(aeid, apid)])
      e3 <- bquote(dat[ , temp := NULL])
      list(e1, e2, e3)
      
    },
    
    pval.apid.p.min = function(aeids) {
      
      e1 <- bquote(dat[J(.(aeids)),
                       temp := median(cval[wllt == "p"], na.rm = TRUE),
                       by = list(aeid, apid, conc)])
      e2 <- bquote(dat[J(.(aeids)),
                       pval := min(temp, na.rm = TRUE),
                       by = list(aeid, apid)])
      e3 <- bquote(dat[ , temp := NULL])
      list(e1, e2, e3)
      
    },
    
    pval.apid.v.min = function(aeids) {
      
      e1 <- bquote(dat[J(.(aeids)),
                       temp := median(cval[wllt == "v"], na.rm = TRUE),
                       by = list(aeid, apid, conc)])
      e2 <- bquote(dat[J(.(aeids)),
                       pval := min(temp, na.rm = TRUE),
                       by = list(aeid, apid)])
      e3 <- bquote(dat[ , temp := NULL])
      list(e1, e2, e3)
      
    },
    
    pval.zero = function(aeids) {
      
      e1 <- bquote(dat[J(.(aeids)), pval := 0])
      list(e1)
      
    },
    
    resp.pc = function(aeids) {
      
      e1 <- bquote(dat[J(.(aeids)),
                       resp := (cval - bval)/(pval - bval)*100])
      list(e1)
      
    },
    
    resp.pc.pval.cor = function(aeids) {
      
      e1 <- bquote(dat[J(.(aeids)),
                       resp := (cval - bval)/(pval)*100])
      list(e1)
      
    },
    
    resp.fc = function(aeids) {
      
      e1 <- bquote(dat[J(.(aeids)), resp := cval/bval])
      list(e1)
      
    },
    
    resp.logfc = function(aeids) {
      
      e1 <- bquote(dat[J(.(aeids)), resp := cval - bval])
      list(e1)
      
    },
    
    resp.log2 = function(aeids) {
      
      e1 <- bquote(dat[J(.(aeids)), resp := log2(resp)])
      list(e1)
      
    },
    
    resp.mult25 = function(aeids) {
      
      e1 <- bquote(dat[J(.(aeids)), resp := resp * 25])
      list(e1)
      
    },
    
    resp.scale.mad.log2fc = function(aeids) {
      
      e1 <- bquote(dat[J(.(aeids)), 
                       bmad := mad(resp[cndx %in% 1:2 & wllt == "t"],
                                   na.rm = TRUE),
                       by = aeid])
      e2 <- bquote(dat[J(.(aeids)), resp := log2(1.2)/(3*bmad)*resp])
      e3 <- bquote(dat[ , bmad := NULL])
    
      list(e1, e2, e3)
      
    },
    
    resp.scale.quant.log2fc = function(aeids) {
      
      qv <- c(0.01, 0.5, 0.99)
      e1 <- bquote(dat[J(.(aeids)), 
                       c("q1", "q2", "q3") := as.list(quantile(resp, .(qv))), 
                       by = aeid])
      e2 <- bquote(dat[J(.(aeids)),
                       md := max(abs(c(diff(c(q1, q2)), diff(c(q2, q3))))),
                       by = aeid])
      e3 <- bquote(dat[J(.(aeids)), resp := log2(1.2)/(0.2*md)*resp])
      e4 <- bquote(dat[ , .(c("q1", "q2", "q3", "md")) := NULL])
      list(e1, e2, e3, e4)
      
    },
    
    resp.multneg1 = function(aeids) {
      
      e1 <- bquote(dat[J(.(aeids)), resp := resp * -1])
      list(e1)
      
    },
    
    resp.shiftneg.3bmad = function(aeids) {
      
      e1 <- bquote(dat[J(.(aeids)), 
                       bmad := mad(resp[cndx %in% 1:2 & wllt == "t"], 
                                   na.rm = TRUE),
                       by = aeid])
      e2 <- bquote(dat[aeid %in% .(aeids) & resp < -3 * bmad, resp := 0])
      e3 <- bquote(dat[ , bmad := NULL])
      list(e1, e2, e3)
      
    },
    
    resp.shiftneg.6bmad = function(aeids) {
      
      e1 <- bquote(dat[J(.(aeids)), 
                       bmad := mad(resp[cndx %in% 1:2 & wllt == "t"], 
                                   na.rm = TRUE),
                       by = aeid])
      e2 <- bquote(dat[aeid %in% .(aeids) & resp < -6 * bmad, resp := 0])
      e3 <- bquote(dat[ , bmad := NULL])
      list(e1, e2, e3)
      
    },
    
    resp.shiftneg.10bmad = function(aeids) {
      
      e1 <- bquote(dat[J(.(aeids)), 
                       bmad := mad(resp[cndx %in% 1:2 & wllt == "t"], 
                                   na.rm = TRUE),
                       by = aeid])
      e2 <- bquote(dat[aeid %in% .(aeids) & resp < -10 * bmad, resp := 0])
      e3 <- bquote(dat[ , bmad := NULL])
      list(e1, e2, e3)
      
    },
    
    resp.blineshift.3bmad.repi = function(aeids) {
      
      e1 <- bquote(dat[J(.(aeids)), 
                       wndw := mad(resp[cndx %in% 1:2], 
                                   na.rm = TRUE) * 3,
                       by = aeid])
      e2 <- bquote(dat[aeid %in% .(aeids), 
                       resp := blineShift(resp, conc, wndw), 
                       by = list(aeid, spid, repi)])
      e3 <- bquote(dat[ , wndw := NULL])
      list(e1, e2, e3)
      
    },
    
    resp.blineshift.50.repi = function(aeids) {
      
      e1 <- bquote(dat[aeid %in% .(aeids), 
                       resp := blineShift(resp, conc, wndw = 50), 
                       by = list(aeid, spid, repi)])
      list(e1)
      
    },
    
    resp.blineshift.3bmad.spid = function(aeids) {
      
      e1 <- bquote(dat[J(.(aeids)), 
                       wndw := mad(resp[cndx %in% 1:2], 
                                   na.rm = TRUE) * 3,
                       by = aeid])
      e2 <- bquote(dat[aeid %in% .(aeids), 
                       resp := blineShift(resp, conc, wndw), 
                       by = list(aeid, spid)])
      e3 <- bquote(dat[ , wndw := NULL])
      list(e1, e2, e3)
      
    },
    
    resp.blineshift.50.spid = function(aeids) {
      
      e1 <- bquote(dat[aeid %in% .(aeids), 
                       resp := blineShift(resp, conc, wndw = 50), 
                       by = list(aeid, spid)])
      list(e1)
      
    },
    
    none = function(aeids) {
      
      e1 <- bquote(dat[J(.(aeids)), resp := cval])
      list(e1)
      
    },
    
    bval.apid.nwllstcwllslowconc.med = function(aeids) {
      
      e1 <- bquote(dat[J(.(aeids)), 
                       bval := median(cval[(cndx %in% 1:2 & wllt %in% c("t","c")) | 
                                             wllt == "n"],
                                      na.rm = TRUE),
                       by = list(aeid, apid)])
      list(e1)
    },
    
        pval.apid.owlls.med = function(aeids) {
      
      e1 <- bquote(dat[J(.(aeids)), 
                       pval := median(cval[wllt == "o"], na.rm = TRUE), 
                       by = list(aeid, apid)])
      list(e1)
      
    },
    
     pval.2bval = function(aeids) {
      
      e1 <- bquote(dat[J(.(aeids)), 
                       pval := ((median(cval[wllt == "n"], na.rm = TRUE))*2), 
                       by = list(aeid, apid)])
      list(e1)
      
    },
    
     pval.maxp = function(aeids) {
      
      e1 <- bquote(dat[J(.(aeids)), 
                       pval := max(cval[wllt == "p"], na.rm = TRUE), 
                       by = list(aeid)])
      list(e1)
      
    },
     pval.apid.bwlls.med = function(aeids) {
      
      e1 <- bquote(dat[J(.(aeids)), 
                       pval := median(cval[wllt == "b"], na.rm = TRUE), 
                       by = list(aeid, apid)])
      list(e1)
      
    },
    
      resp.zerocenter.fc = function(aeids) {
      
      e1 <- bquote(dat[J(.(aeids)),
                       resp := 1-(cval/bval)])
      list(e1)
    },    
    
      bval.apid.nullconc.med = function(aeids) {
        
        e1 <- bquote(dat[J(.(aeids)), 
                         bval := median(cval[wllt == "z"],
                                        na.rm = TRUE),
                         by = list(aeid, apid)])
        list(e1)
      
    },
    bval.apid.lowconc.nmad = function(aeids) {
      #nMad = mad(d, constant=1.4826, na.rm=T)
      e1 <- bquote(dat[J(.(aeids)),
                       bval := mad(cval[cndx %in% 1:2 & wllt == "t"],
                                   constant = 1.4826,
                                   na.rm = TRUE),
                       by = list(aeid, apid)])
      list(e1)
    },
    
    resp.apid.lowconc.bmed= function(aeids) {
      #Median = median(d, na.rm=T)
      e1 <- bquote(dat[J(.(aeids)),
                       resp := cval - median(cval[cndx %in% 1:2 & wllt == "t"],na.rm = TRUE),
                       by = list(aeid, apid)])
    },
    
    bval.aeid.nwlls.med = function(aeids) {
      e1 <- bquote(dat[J(.(aeids)), 
                       bval := median(cval[wllt == "n"], na.rm = TRUE), 
                       by = list(aeid)])
      list(e1)
      
    },
    
    resp.incr.zerocenter.fc = function(aeids) {
      
      e1 <- bquote(dat[J(.(aeids)),
                       resp := (cval/bval)-1])
      list(e1)
    },
    
    resp.mult100 = function(aeids) {
      
      e1 <- bquote(dat[J(.(aeids)), resp := resp * 100])
      list(e1)
      
    }, 
    
    pval.twlls.99pct = function(aeids) {
      e1 <- bquote(dat[J(.(aeids)), pval := quantile(cval[wllt=='t'], probs = 0.99, na.rm=TRUE),
                       by=list(aeid)])
      list(e1)
      },
    
  	pval.neg.100 = function(aeids) {
      e1 <- bquote(dat[J(.(aeids)), pval := -100,
                       by=list(aeid)])
      list(e1)
      },
    
    resp.censormed.neg25 = function(aeids) {
      
      e1 <- bquote(dat[J(.(aeids)), med := median(resp), by=list(aeid,spid,conc)])
      e2 <- bquote(dat <- dat[med >= -25])
      e3 <- bquote(dat[ , med := NULL])
      list(e1,e2,e3)
    }
    
  
    
  )
  
  
  
}

#-------------------------------------------------------------------------------
