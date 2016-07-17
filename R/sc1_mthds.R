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
#' available in the package vignette, "Pipeline_Overview."
#' 
#' \subsection{bval Methods}{
#'   \describe{
#'     \item{bval.apid.nwlls.med}{Calculate bval as the median of rval for 
#'     wells with wllt equal to "n," by apid.}
#'     \item{bval.apid.twlls.med}{Calculate bval as the median of rval for 
#'     wells with wllt equal to "t," by apid.}
#'     \item{bval.apid.tn.med}{Calculate bval as the median of rval for wells 
#'     with wllt equal to "t" or "n," by apid.}
#'   }
#' } 
#' 
#' \subsection{pval Methods}{
#'   \describe{
#'     \item{pval.apid.pwlls.med}{Calculate pval as the median of rval for 
#'     wells with wllt equal to "p," by apid.}
#'     \item{pval.apid.mwlls.med}{Calculate pval as the median of rval for 
#'     wells with wllt equal to "m," by apid.}
#'     \item{pval.apid.medpcbyconc.max}{First calculate the median of rval for
#'     wells with wllt equal to "p" or "c," by wllt, conc, and apid. Then 
#'     calculate pval as the maximum of the calculated medians, by apid.}
#'     \item{pval.apid.medpcbyconc.min}{First calculate the median of rval for
#'     wells with wllt equal to "p" or "c," by wllt, conc, and apid. Then 
#'     calculate pval as the minimum of the calculated medians, by apid.}
#'     \item{pval.apid.medncbyconc.min}{First calculate the median of rval for
#'     wells with wllt equal to "m" or "o," by wllt, conc, and apid. Then 
#'     calculate pval as the minimum of the calculated medians, by apid.}
#'     \item{pval.zero}{Define pval as 0.}
#'   }
#' } 
#' 
#' \subsection{resp Methods}{
#'   \describe{
#'     \item{resp.pc}{Calculate resp as \eqn{\frac{\mathit{rval} - 
#'     \mathit{bval}}{\mathit{pval} - \mathit{bval}}100}{(rval - bval)/(pval 
#'     - bval)*100}.}
#'     \item{resp.fc}{Calculate resp as \eqn{\mathit{rval}/\mathit{bval}}{
#'     rval/bval}.}
#'     \item{resp.logfc}{Calculate resp as \eqn{\mathit{rval} - \mathit{bval}}{
#'     rval - bval}.}
#'     \item{resp.log2}{Take the logarithm of resp with base 2.}
#'     \item{resp.multneg1}{Multiply resp by -1.}
#'     \item{none}{Do no normalization; make resp equal to rval.}
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
      
    }
    
  )
}

#-------------------------------------------------------------------------------
