#-------------------------------------------------------------------------------
# sc1_mthds: Load list of sc1 method functions
#-------------------------------------------------------------------------------

#' @name SC1_Methods
#' @title List of level 1 single-concentration normalization functions
#' 
#' @description 
#' \code{sc1_mthds} returns a list of functions to be used during level 1 
#' single-concentration processing.
#' 
#' @return A list functions
#' 
#' @seealso \code{\link{sc1}}, \code{\link{Method functions}} to query what
#' methods get applied to each acid
#' 
#' @details 
#' The functions contained in the list returned by \code{sc1_mthds} return
#' a list of expressions to be executed in the \code{sc2} (not exported) 
#' function environment. The functions are described here for reference 
#' purposes, The \code{sc1_mthds} function is not exported, nor is it 
#' intended for use.
#' 
#' All available methods are described in the Available Methods section, listed
#' by the function/method name. 
#' 
#' @section Available Methods:
#' 
#' The methods are broken into three types, based on what fields they define. 
#' Different methods are used to define "bval" (the baseline value), "pval"
#' (the positive control value), and "resp" (the final response value). 
#' 
#' Although it does not say so specifically in each description, all methods 
#' are applied by acid.
#' 
#' More information about the level 3 single-concentration processing is 
#' available in the package vignette, "Data_processing."
#' 
#' \subsection{bval Methods}{
#'   \describe{
#'     \item{bval.apid.nwlls.med}{Calculate the baseline value (bval) as the plate-wise median,
#'      by assay plate ID (apid), of the raw values (rval) for neutral control wells (wllt = n).}
#'     \item{bval.apid.twlls.med}{Calculate the baseline value (bval) as the plate-wise median,
#'      by assay plate ID (apid), of the raw values (rval) for test compound wells (wllt = t).}
#'     \item{bval.apid.tn.med}{Calculate the baseline value (bval) as the plate-wise median,
#'      by assay plate ID (apid), of the raw values (rval) for test compound wells (wllt = t)
#'       and neutral control wells (wllt = n).}
#'   }
#' } 
#' 
#' \subsection{pval Methods}{
#'   \describe{
#'     \item{pval.apid.pwlls.med}{Calculate the positive control value (pval) as the plate-wise 
#'     median, by assay plate ID (apid), of the raw values (rval) for single-concentration 
#'     gain-of-signal positive control wells (wllt = p).}
#'     \item{pval.apid.mwlls.med}{Calculate the positive control value (pval) as the plate-wise 
#'     median, by assay plate ID (apid), of the raw values (rval) for multiple-concentration 
#'     loss-of-signal negative control wells (wllt = m).}
#'     \item{pval.apid.medpcbyconc.max}{Calculate the positive control value (pval) as the 
#'     plate-wise maximum, by assay plate ID (apid), of the medians of the raw values (rval) for 
#'     gain-of-signal single- or multiple-concentration positive control wells (wllt = p or c) by 
#'     apid, well type, and concentration.}
#'     \item{pval.apid.medpcbyconc.min}{Calculate the positive control value (pval) as the 
#'     plate-wise minimum, by assay plate ID (apid), of the medians of the raw values (rval) for 
#'     gain-of-signal single- or multiple-concentration positive control wells (wllt = p or c) by 
#'     apid, well type, and concentration.}
#'     \item{pval.apid.medncbyconc.min}{Calculate the positive control value (pval) as the 
#'     plate-wise minimum, by assay plate ID (apid), of the medians of the raw values (rval) for 
#'     gain-of-signal single- or multiple-concentration negative control wells (wllt = m or o) by 
#'     apid, well type, and concentration.}
#'     \item{pval.zero}{Set the positive control value (pval) to 0; pval = 0.}
#'     \item{pval.apid.or.aeid.pwlls.med}{Calculate the positive control value (pval) as the 
#'     plate-wise median, by assay plate ID (apid), of the raw values (rval) for 
#'     single-concentration gain-of-signal positive control wells (wllt = p). For plates without p 
#'     wells, set the pval as the median pval calculated from all plates.}
#'   }
#' } 
#' 
#' \subsection{resp Methods}{
#'   \describe{
#'     \item{resp.pc}{Calculate the normalized response (resp) as a percent of control, i.e. the 
#'     ratio of the difference between the raw (rval) and baseline (bval) values divided by the 
#'     difference between positive control (pval) and baseline (bval) values multiplied by 100; 
#'     \eqn{resp=(rval-bval)/(pval-bval)*100}{resp=(rval-bval)/(pval-bval)*100}.}
#'     \item{resp.fc}{Calculate the normalized response (resp) as fold change, i.e. the ratio of 
#'     the raw (rval) and baseline (bval) values; \eqn{resp = rval/bval}{resp = rval/bval}.}
#'     \item{resp.logfc}{Calculate the normalized response (resp) as the fold change of logged,
#'      i.e. the difference between raw (rval) and baseline (bval) log-scale values.}
#'     \item{resp.log2}{Transform the response values to log-scale (base 2).}
#'     \item{resp.multneg1}{Multiply the normalized response value (resp) by -1; 
#'     \eqn{-1*resp}{-1*resp}.}
#'     \item{none}{Use raw value (rval) as is. This may be necessary for additional 
#'     endpoint-specific adjustments, or where no additional sc1 methods are needed.}
#'     \item{resp.incr.zerocenter.fc}{Calculate the normalized response (resp) as a zero center 
#'     fold change, i.e. the ratio of the raw (rval) and baseline (bval) values minus 1; 
#'     \eqn{resp=rval/bval-1}{resp=rval/bval-1}. Typically used for increasing responses.}
#'   }
#' }
#' 
#' @note
#' This function is not exported and is not intended to be used by the user.


sc1_mthds <- function() {
  
  list(
    
    bval.apid.nwlls.med = function(aeids) {
      
      e1 <- bquote(dat[J(.(aeids)), 
                       bval := median(rval[wllt == "n"], na.rm = TRUE), 
                       by = list(aeid, apid)])
      list(e1)
      
    },    
    
    bval.apid.twlls.med = function(aeids) {
      
      e1 <- bquote(dat[J(.(aeids)), 
                       bval := median(rval[wllt == "t"], na.rm = TRUE),
                       by = list(aeid, apid)])
      list(e1)
      
    },
    
    bval.apid.tn.med = function(aeids) {
      
      e1 <- bquote(dat[J(.(aeids)), 
                       bval := median(rval[wllt %in% c("t", "n")], 
                                      na.rm = TRUE),
                       by = list(aeid, apid)])
      list(e1)
      
    },
    bval.nwlls.med = function(aeids) {

      e1 <- bquote(dat[J(.(aeids)),
                       bval := median(rval[wllt == "n"], na.rm = TRUE),
                       by = list(aeid)])
      list(e1)

    },
    
    pval.apid.pwlls.med = function(aeids) {
      
      e1 <- bquote(dat[J(.(aeids)), 
                       pval := median(rval[wllt == "p"], na.rm = TRUE), 
                       by = list(aeid, apid)])
      list(e1)
      
    },
    
    pval.apid.or.aeid.pwlls.med = function(aeids) {
      
      e1 <- bquote(dat[J(.(aeids)), 
                       pval := median(rval[wllt == "p"], na.rm = TRUE), 
                       by = list(aeid, apid)])
      e2 <- bquote(dat[J(.(aeids)),
                       temp := median(pval, 
                                      na.rm = TRUE),
                       by = list(aeid)])
      e3 <- bquote(dat[aeid %in% .(aeids) & (is.na(pval) | is.infinite(pval)), 
                       pval := temp,
                       by = list(aeid)])
      e4 <- bquote(dat[ , temp := NULL])
      list(e1,e2,e3,e4)
      
    },
    
    pval.apid.mwlls.med = function(aeids) {
      
      e1 <- bquote(dat[J(.(aeids)), 
                       pval := median(rval[wllt == "m"], na.rm = TRUE), 
                       by = list(aeid, apid)])
      list(e1)
      
    },
    
    pval.apid.medpcbyconc.max = function(aeids) {
      
      e1 <- bquote(dat[J(.(aeids)),
                       temp := median(rval[wllt %in% c("c", "p")], 
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
                       temp := median(rval[wllt %in% c("c", "p")], 
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
                       temp := median(rval[wllt %in% c("m","o")], 
                                      na.rm = TRUE),
                       by = list(aeid, apid, wllt, conc)])
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
      
      e1 <- bquote(dat[J(.(aeids)), resp := (rval - bval)/(pval - bval)*100])
      list(e1)
      
    },
    
    resp.fc = function(aeids) {
      
      e1 <- bquote(dat[J(.(aeids)), resp := rval/bval])
      list(e1)
      
    },
    
    resp.logfc = function(aeids) {
      
      e1 <- bquote(dat[J(.(aeids)), resp := rval - bval])
      list(e1)
      
    },
    
    resp.log2 = function(aeids) {
      
      e1 <- bquote(dat[J(.(aeids)), resp := log2(resp)])
      list(e1)
      
    },
    
    none = function(aeids) {
      
      e1 <- bquote(dat[J(.(aeids)), resp := rval])
      list(e1)
      
    },
    
    resp.multneg1 = function(aeids) {
      
      e1 <- bquote(dat[J(.(aeids)), resp := resp * -1])
      list(e1)
      
    },
    
    resp.incr.zerocenter.fc = function(aeids) {
      
      e1 <- bquote(dat[J(.(aeids)),
                       resp := (rval/bval)-1])
      list(e1)
    }
      
  )
}

#-------------------------------------------------------------------------------
