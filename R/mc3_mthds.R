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
#' 'aeids' (a numeric vector of aeid values) and returns a list of expressions 
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
#' available in the package vignette, "Pipeline_Overview."
#' 
#' \subsection{bval Methods}{
#'   \describe{
#'     \item{bval.apid.nwlls.med}{Calculate bval as the median of cval for 
#'     wells with wllt equal to "n," by apid.}
#'     \item{bval.apid.lowconc.med}{Calculate bval as the median of cval for 
#'     wells with wllt equal to "t" and cndx equal to 1 or 2, by apid.}
#'     \item{bval.apid.twlls.med}{Calculate bval as the median of cval for 
#'     wells with wllt equal to "t," by apid.}
#'     \item{bval.apid.tn.med}{Calculate bval as the median of cval for wells 
#'     with wllt equal to "t" or "n," by apid.}
#'     \item{bval.apid.nwllslowconc.med}{Calculate bval as the median of cval 
#'     for wells with wllt equal to "n" or wells with wllt equal to "t" and 
#'     cndx equal to 1 or 2, by apid.}
#'     \item{bval.spid.lowconc.med}{Calculate bval as the median of cval for 
#'     wells with wllt equal to "t" and cndx equal to 1, 2, or 3, by spid.}
#'     \item{bval.apid.nwllstcwllslowconc.med}{Calculate bval as the median of 
#'     cval for wells with wllt equal to "n" or cndx equal to 1 or 2 and
#'     wllt equal to "t" or "c" by apid.}
#'      
#'   }
#' } 
#' 
#' \subsection{pval Methods}{
#'   \describe{
#'     \item{pval.apid.pwlls.med}{Calculate pval as the median of cval for 
#'     wells with wllt equal to "p," by apid.}
#'     \item{pval.apid.mwlls.med}{Calculate pval as the median of cval for 
#'     wells with wllt equal to "m," by apid.}
#'     \item{pval.apid.medpcbyconc.max}{First calculate the median of cval for
#'     wells with wllt equal to "p" or "c," by wllt, conc, and apid. Then 
#'     calculate pval as the maximum of the calculated medians, by apid.}
#'     \item{pval.apid.medpcbyconc.min}{First calculate the median of cval for
#'     wells with wllt equal to "p" or "c," by wllt, conc, and apid. Then 
#'     calculate pval as the minimum of the calculated medians, by apid.}
#'     \item{pval.apid.medncbyconc.min}{First calculate the median of cval for
#'     wells with wllt equal to "m" or "o," by wllt, conc, and apid. Then 
#'     calculate pval as the minimum of the calculated medians, by apid.}
#'     \item{pval.apid.pmv.min}{First calculate the median of cval for
#'     wells with wllt equal to "p," "m," or "v," by wllt, conc, and apid. Then 
#'     calculate pval as the minimum of the calculated medians, by apid.}
#'     \item{pval.apid.pmv.max}{First calculate the median of cval for
#'     wells with wllt equal to "p," "m," or "v," by wllt, conc, and apid. Then 
#'     calculate pval as the maximum of the calculated medians, by apid.}
#'     \item{pval.apid.f.max}{First calculate the median of cval for
#'     wells with wllt equal to "f," by wllt, conc, and apid. Then calculate 
#'     pval as the maximum of the calculated medians, by apid.}
#'     \item{pval.apid.f.min}{First calculate the median of cval for
#'     wells with wllt equal to "f," by wllt, conc, and apid. Then calculate 
#'     pval as the minimum of the calculated medians, by apid.}
#'     \item{pval.apid.p.max}{First calculate the median of cval for
#'     wells with wllt equal to "p," by wllt, conc, and apid. Then calculate 
#'     pval as the maximum of the calculated medians, by apid.}
#'     \item{pval.apid.p.min}{First calculate the median of cval for
#'     wells with wllt equal to "p," by wllt, conc, and apid. Then calculate 
#'     pval as the minimum of the calculated medians, by apid.}
#'     \item{pval.apid.v.min}{First calculate the median of cval for
#'     wells with wllt equal to "v," by wllt, conc, and apid. Then calculate 
#'     pval as the minimum of the calculated medians, by apid.}
#'     \item{pval.zero}{Define pval as 0.}
#'   }
#' } 
#' 
#' \subsection{resp Methods}{
#'   \describe{
#'     \item{resp.pc}{Calculate resp as \eqn{\frac{\mathit{cval} - 
#'     \mathit{bval}}{\mathit{pval} - \mathit{bval}}100}{(cval - bval)/(pval 
#'     - bval)*100}.}
#'     \item{resp.fc}{Calculate resp as \eqn{\mathit{cval}/\mathit{bval}}{
#'     cval/bval}.}
#'     \item{resp.logfc}{Calculate resp as \eqn{\mathit{cval} - \mathit{bval}}{
#'     cval - bval}.}
#'     \item{resp.log2}{Take the logarithm of resp with base 2.}
#'     \item{resp.mult25}{Multiply resp by 25.}
#'     \item{resp.scale.mad.log2fc}{Multiply resp by the scale factor 
#'     \eqn{\frac{\log_2 \left( 1.2 \right)}{3\mathit{bmad}}}{
#'     log2(1.2)/(3*bmad)}.}
#'     \item{resp.scale.quant.log2fc}{Determine the maximum response 
#'     \eqn{\mathit{md}}{md} where \eqn{\mathit{md}}{md} = abs(1st centile - 
#'     50th centile) or abs(99th centile - 50th centile), whichever is greater. 
#'     Scale the response such that 20 percent of md equals
#'     \eqn{\log_2 \left( 1.2 \right)}{log2(1.2)}.}
#'     \item{resp.multneg1}{Multiply resp by -1.}
#'     \item{resp.shiftneg.3bmad}{Shift all resp values less than -3*bmad to 0.}
#'     \item{resp.shiftneg.6bmad}{Shift all resp values less than -6*bmad to 0.}
#'     \item{resp.shiftneg.10bmad}{Shift all resp values less than -10*bmad to 
#'     0.}
#'     \item{resp.blineshift.3bmad.repi}{Shift resp values with the blineShift
#'     function by repi, where the window (wndw) is 3*bmad.}
#'     \item{resp.blineshift.50.repi}{Shift resp values with the blineShift
#'     function by repi, where the window (wndw) is 50.}
#'     \item{resp.blineshift.3bmad.spid}{Shift resp values with the blineShift
#'     function by spid, where the window (wndw) is 3*bmad.}
#'     \item{resp.blineshift.50.spid}{Shift resp values with the blineShift
#'     function by spid, where the window (wndw) is 50.}
#'     \item{none}{Do no normalization; make resp equal to cval.}
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
                       wndw := mad(resp[cndx %in% 1:2 & wllt == "t"], 
                                   na.rm = TRUE) * 3,
                       by = aeid])
      e2 <- bquote(dat[aeid %in% .(aeids) & wllt %in% c("t", "c", "o"), 
                       resp := blineShift(resp, logc, wndw), 
                       by = list(aeid, spid, repi)])
      e3 <- bquote(dat[ , wndw := NULL])
      list(e1, e2, e3)
      
    },
    
    resp.blineshift.50.repi = function(aeids) {
      
      e1 <- bquote(dat[aeid %in% .(aeids) & wllt %in% c("t", "c", "o"), 
                       resp := blineShift(resp, logc, wndw = 50), 
                       by = list(aeid, spid, repi)])
      list(e1)
      
    },
    
    resp.blineshift.3bmad.spid = function(aeids) {
      
      e1 <- bquote(dat[J(.(aeids)), 
                       wndw := mad(resp[cndx %in% 1:2 & wllt == "t"], 
                                   na.rm = TRUE) * 3,
                       by = aeid])
      e2 <- bquote(dat[aeid %in% .(aeids) & wllt %in% c("t", "c", "o"), 
                       resp := blineShift(resp, logc, wndw), 
                       by = list(aeid, spid)])
      e3 <- bquote(dat[ , wndw := NULL])
      list(e1, e2, e3)
      
    },
    
    resp.blineshift.50.spid = function(aeids) {
      
      e1 <- bquote(dat[aeid %in% .(aeids) & wllt %in% c("t", "c", "o"), 
                       resp := blineShift(resp, logc, wndw = 50), 
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
    }
    
  )
  
}

#-------------------------------------------------------------------------------
