#-------------------------------------------------------------------------------
# mc4_mthds: List of bmad calculation methods (to be used at level 4)
#-------------------------------------------------------------------------------

#' @name MC4_Methods
#' @title List of level 4 multiple-concentration methods for calculating bmad
#' 
#' @description 
#' \code{mc4_mthds} returns a list of methods to be used 
#' during level 4 multiple-concentration processing for calculating bmad
#' 
#' @return A list of functions
#' 
#' @seealso \code{\link{mc4}}, \code{\link{Method functions}} to query what 
#' methods get applied to each aeid
#' 
#' @details
#' The functions contained in the list returned by \code{mc4_mthds} take 
#' 'aeids' (a numeric vector of aeid values) and returns a list of expressions 
#' to be executed in the \code{mc4} (not exported) function environment. The 
#' functions are described here for reference purposes, The 
#' \code{mc4_mthds} function is not exported, nor is it intended for use.
#' 
#' All available methods are described in the Available Methods section, listed
#' by the type of function and the function/method name. 
#' 
#' @section Available Methods:
#' 
#' 
#' Although it does not say so specifically in each description, all methods 
#' are applied by aeid.
#' 
#' More information about the level 4 multiple-concentration processing is 
#' available in the package vignette, "Pipeline_Overview."
#' 
#'   \describe{
#'     \item{bmad.aeid.lowconc.twells}{bmad based on two lowest concentration of treatment wells}
#'     \item{bmad.aeid.lowconc.nwells}{bmad based on two lowest concentration of nwells}
#'   }
#' 
#' @note
#' This function is not exported and is not intended to be used by the user.

mc4_mthds <- function() {
  
  list(
    
    bmad.aeid.lowconc.twells = function() {
      
      e1 <- bquote(dat[ , bmad := mad(resp[cndx %in% 1:2 & wllt == "t"], na.rm = TRUE)])
      list(e1)
      
    },
    
    bmad.aeid.lowconc.nwells = function() {
      
      e1 <- bquote(dat[ , bmad := mad(resp[wllt == "n"], na.rm = TRUE)])
      list(e1)
      
    },
    
    onesd.aeid.lowconc.twells = function() {
      
      e1 <- bquote(dat[ , osd := sd(resp[cndx %in% 1:2 & wllt == "t"], na.rm = TRUE)])
      list(e1)
      
    },
    bmed.aeid.lowconc.twells = function() {
      
      e1 <- bquote(dat[ , bmed := median(resp[cndx %in% 1:2 & wllt == "t"], na.rm = TRUE)])
      list(e1)
      
    },
    no.gnls.fit = function() {
      
      e1 <- bquote(dat[ ,fitmodels := list(c("cnst", "hill",  "poly1", "poly2", "pow", "exp2", "exp3","exp4", "exp5"))])
      list(e1)
      
    },
    nmad.apid.null.zwells = function() {
      
      e1 <- bquote(dat[, 
                       bmad := mad(resp[wllt == "z"], constant=1.4826,
                                      na.rm = TRUE),
                       by = list(aeid, apid)])
      list(e1)
      
    }
    
  )
}

# mc4_mthds <- function() {
#   
#   
#   
#   list(
#     
#     bval.apid.nwlls.med = function(aeids) {
#       e1 <- bquote(dat[J(.(aeids)), 
#                        bval := median(cval[wllt == "n"], na.rm = TRUE), 
#                        by = list(aeid, apid)])
#       list(e1)
#       
#     },    
#     
#     bval.apid.lowconc.med = function(aeids) {
#             
#       e1 <- bquote(dat[J(.(aeids)), 
#                        bval := median(cval[cndx %in% 1:2 & wllt == "t"],
#                                       na.rm = TRUE),
#                        by = list(aeid, apid)])
#       list(e1)
#       
#     },
#     
#     bval.apid.twlls.med = function(aeids) {
#             
#       e1 <- bquote(dat[J(.(aeids)), 
#                        bval := median(cval[wllt == "t"], na.rm = TRUE),
#                        by = list(aeid, apid)])
#       list(e1)
#       
#     },
#     
#     bval.apid.tn.med = function(aeids) {
#             
#       e1 <- bquote(dat[J(.(aeids)), 
#                        bval := median(cval[wllt %in% c("t", "n")], 
#                                       na.rm = TRUE),
#                        by = list(aeid, apid)])
#       list(e1)
#       
#     },
#     
#     bval.apid.nwllslowconc.med = function(aeids) {
#       
#       e1 <- bquote(dat[J(.(aeids)), 
#                        bval := median(cval[(cndx %in% 1:2 & wllt == "t") | 
#                                              wllt == "n"],
#                                       na.rm = TRUE),
#                        by = list(aeid, apid)])
#       list(e1)
#       
#     },
#     
#     bval.spid.lowconc.med = function(aeids) {
#             
#       e1 <- bquote(dat[J(.(aeids)), 
#                        bval := median(cval[cndx %in% 1:3 & wllt == "t"],
#                                       na.rm = TRUE),
#                        by = list(aeid, spid, repi)])
#       list(e1)
#       
#     },
#     
#     pval.apid.pwlls.med = function(aeids) {
#       
#       e1 <- bquote(dat[J(.(aeids)), 
#                        pval := median(cval[wllt == "p"], na.rm = TRUE), 
#                        by = list(aeid, apid)])
#       list(e1)
#       
#     },
#     
#     pval.apid.mwlls.med = function(aeids) {
#       
#       e1 <- bquote(dat[J(.(aeids)), 
#                        pval := median(cval[wllt == "m"], na.rm = TRUE), 
#                        by = list(aeid, apid)])
#       list(e1)
#       
#     },
#     
#     pval.apid.medpcbyconc.max = function(aeids) {
#             
#       e1 <- bquote(dat[J(.(aeids)),
#                        temp := median(cval[wllt %in% c("c", "p")], 
#                                       na.rm = TRUE),
#                        by = list(aeid, apid, wllt, conc)])
#       e2 <- bquote(dat[J(.(aeids)),
#                        pval := max(temp, na.rm = TRUE),
#                        by = list(aeid, apid)])
#       e3 <- bquote(dat[ , temp := NULL])
#       list(e1, e2, e3)
#       
#     },
#     
#     pval.apid.medpcbyconc.min = function(aeids) {
#       
#       e1 <- bquote(dat[J(.(aeids)),
#                        temp := median(cval[wllt %in% c("c", "p")], 
#                                       na.rm = TRUE),
#                        by = list(aeid, apid, wllt, conc)])
#       e2 <- bquote(dat[J(.(aeids)),
#                        pval := min(temp, na.rm = TRUE),
#                        by = list(aeid, apid)])
#       e3 <- bquote(dat[ , temp := NULL])
#       list(e1, e2, e3)
#       
#     },
#     
#     pval.apid.medncbyconc.min = function(aeids) {
#       
#       e1 <- bquote(dat[J(.(aeids)),
#                        temp := median(cval[wllt %in% c("m","o")], 
#                                       na.rm = TRUE),
#                        by = list(aeid, apid, wllt, conc)])
#       e2 <- bquote(dat[J(.(aeids)),
#                        pval := min(temp, na.rm = TRUE),
#                        by = list(aeid, apid)])
#       e3 <- bquote(dat[ , temp := NULL])
#       list(e1, e2, e3)
#       
#     },
#     
#     pval.apid.pmv.min = function(aeids) {
#       
#       e1 <- bquote(dat[J(.(aeids)),
#                        temp := median(cval[wllt %in% c("p", "m", "v")], 
#                                       na.rm = TRUE),
#                        by = list(aeid, apid, wllt, conc)])
#       e2 <- bquote(dat[J(.(aeids)),
#                        pval := min(temp, na.rm = TRUE),
#                        by = list(aeid, apid)])
#       e3 <- bquote(dat[ , temp := NULL])
#       list(e1, e2, e3)
#       
#     },
#     
#     pval.apid.pmv.max = function(aeids) {
#       
#       e1 <- bquote(dat[J(.(aeids)),
#                        temp := median(cval[wllt %in% c("p", "m", "v")], 
#                                       na.rm = TRUE),
#                        by = list(aeid, apid, wllt, conc)])
#       e2 <- bquote(dat[J(.(aeids)),
#                        pval := max(temp, na.rm = TRUE),
#                        by = list(aeid, apid)])
#       e3 <- bquote(dat[ , temp := NULL])
#       list(e1, e2, e3)
#       
#     },
#     
#     pval.apid.f.max = function(aeids) {
#       
#       e1 <- bquote(dat[J(.(aeids)),
#                        temp := median(cval[wllt == "f"], na.rm = TRUE),
#                        by = list(aeid, apid, conc)])
#       e2 <- bquote(dat[J(.(aeids)),
#                        pval := max(temp, na.rm = TRUE),
#                        by = list(aeid, apid)])
#       e3 <- bquote(dat[ , temp := NULL])
#       list(e1, e2, e3)
#       
#     },
#     
#     pval.apid.f.min = function(aeids) {
#       
#       e1 <- bquote(dat[J(.(aeids)),
#                        temp := median(cval[wllt == "f"], na.rm = TRUE),
#                        by = list(aeid, apid, conc)])
#       e2 <- bquote(dat[J(.(aeids)),
#                        pval := min(temp, na.rm = TRUE),
#                        by = list(aeid, apid)])
#       e3 <- bquote(dat[ , temp := NULL])
#       list(e1, e2, e3)
#       
#     },
#     
#     pval.apid.p.max = function(aeids) {
#       
#       e1 <- bquote(dat[J(.(aeids)),
#                        temp := median(cval[wllt == "p"], na.rm = TRUE),
#                        by = list(aeid, apid, conc)])
#       e2 <- bquote(dat[J(.(aeids)),
#                        pval := max(temp, na.rm = TRUE),
#                        by = list(aeid, apid)])
#       e3 <- bquote(dat[ , temp := NULL])
#       list(e1, e2, e3)
#       
#     },
#     
#     pval.apid.p.min = function(aeids) {
#       
#       e1 <- bquote(dat[J(.(aeids)),
#                        temp := median(cval[wllt == "p"], na.rm = TRUE),
#                        by = list(aeid, apid, conc)])
#       e2 <- bquote(dat[J(.(aeids)),
#                        pval := min(temp, na.rm = TRUE),
#                        by = list(aeid, apid)])
#       e3 <- bquote(dat[ , temp := NULL])
#       list(e1, e2, e3)
#       
#     },
#     
#     pval.apid.v.min = function(aeids) {
#       
#       e1 <- bquote(dat[J(.(aeids)),
#                        temp := median(cval[wllt == "v"], na.rm = TRUE),
#                        by = list(aeid, apid, conc)])
#       e2 <- bquote(dat[J(.(aeids)),
#                        pval := min(temp, na.rm = TRUE),
#                        by = list(aeid, apid)])
#       e3 <- bquote(dat[ , temp := NULL])
#       list(e1, e2, e3)
#       
#     },
#     
#     pval.zero = function(aeids) {
#       
#       e1 <- bquote(dat[J(.(aeids)), pval := 0])
#       list(e1)
#       
#     },
#     
#     resp.pc = function(aeids) {
#       
#       e1 <- bquote(dat[J(.(aeids)),
#                        resp := (cval - bval)/(pval - bval)*100])
#       list(e1)
#       
#     },
#     
#     resp.fc = function(aeids) {
#       
#       e1 <- bquote(dat[J(.(aeids)), resp := cval/bval])
#       list(e1)
#       
#     },
#     
#     resp.logfc = function(aeids) {
#       
#       e1 <- bquote(dat[J(.(aeids)), resp := cval - bval])
#       list(e1)
#       
#     },
#     
#     resp.log2 = function(aeids) {
#       
#       e1 <- bquote(dat[J(.(aeids)), resp := log2(resp)])
#       list(e1)
#       
#     },
#     
#     resp.mult25 = function(aeids) {
#       
#       e1 <- bquote(dat[J(.(aeids)), resp := resp * 25])
#       list(e1)
#       
#     },
#     
#     resp.scale.mad.log2fc = function(aeids) {
#       
#       e1 <- bquote(dat[J(.(aeids)), 
#                        bmad := mad(resp[cndx %in% 1:2 & wllt == "t"],
#                                    na.rm = TRUE),
#                        by = aeid])
#       e2 <- bquote(dat[J(.(aeids)), resp := log2(1.2)/(3*bmad)*resp])
#       e3 <- bquote(dat[ , bmad := NULL])
#     
#       list(e1, e2, e3)
#       
#     },
#     
#     resp.scale.quant.log2fc = function(aeids) {
#       
#       qv <- c(0.01, 0.5, 0.99)
#       e1 <- bquote(dat[J(.(aeids)), 
#                        c("q1", "q2", "q3") := as.list(quantile(resp, .(qv))), 
#                        by = aeid])
#       e2 <- bquote(dat[J(.(aeids)),
#                        md := max(abs(c(diff(c(q1, q2)), diff(c(q2, q3))))),
#                        by = aeid])
#       e3 <- bquote(dat[J(.(aeids)), resp := log2(1.2)/(0.2*md)*resp])
#       e4 <- bquote(dat[ , .(c("q1", "q2", "q3", "md")) := NULL, with = FALSE])
#       list(e1, e2, e3, e4)
#       
#     },
#     
#     resp.multneg1 = function(aeids) {
#       
#       e1 <- bquote(dat[J(.(aeids)), resp := resp * -1])
#       list(e1)
#       
#     },
#     
#     resp.shiftneg.3bmad = function(aeids) {
#       
#       e1 <- bquote(dat[J(.(aeids)), 
#                        bmad := mad(resp[cndx %in% 1:2 & wllt == "t"], 
#                                    na.rm = TRUE),
#                        by = aeid])
#       e2 <- bquote(dat[aeid %in% .(aeids) & resp < -3 * bmad, resp := 0])
#       e3 <- bquote(dat[ , bmad := NULL])
#       list(e1, e2, e3)
#       
#     },
#     
#     resp.shiftneg.6bmad = function(aeids) {
#       
#       e1 <- bquote(dat[J(.(aeids)), 
#                        bmad := mad(resp[cndx %in% 1:2 & wllt == "t"], 
#                                    na.rm = TRUE),
#                        by = aeid])
#       e2 <- bquote(dat[aeid %in% .(aeids) & resp < -6 * bmad, resp := 0])
#       e3 <- bquote(dat[ , bmad := NULL])
#       list(e1, e2, e3)
#       
#     },
#     
#     resp.shiftneg.10bmad = function(aeids) {
#       
#       e1 <- bquote(dat[J(.(aeids)), 
#                        bmad := mad(resp[cndx %in% 1:2 & wllt == "t"], 
#                                    na.rm = TRUE),
#                        by = aeid])
#       e2 <- bquote(dat[aeid %in% .(aeids) & resp < -10 * bmad, resp := 0])
#       e3 <- bquote(dat[ , bmad := NULL])
#       list(e1, e2, e3)
#       
#     },
#     
#     resp.blineshift.3bmad.repi = function(aeids) {
#       
#       e1 <- bquote(dat[J(.(aeids)), 
#                        wndw := mad(resp[cndx %in% 1:2 & wllt == "t"], 
#                                    na.rm = TRUE) * 3,
#                        by = aeid])
#       e2 <- bquote(dat[aeid %in% .(aeids) & wllt %in% c("t", "c", "o"), 
#                        resp := blineShift(resp, logc, wndw), 
#                        by = list(aeid, spid, repi)])
#       e3 <- bquote(dat[ , wndw := NULL])
#       list(e1, e2, e3)
#       
#     },
#     
#     resp.blineshift.50.repi = function(aeids) {
#       
#       e1 <- bquote(dat[aeid %in% .(aeids) & wllt %in% c("t", "c", "o"), 
#                        resp := blineShift(resp, logc, wndw = 50), 
#                        by = list(aeid, spid, repi)])
#       list(e1)
#       
#     },
#     
#     resp.blineshift.3bmad.spid = function(aeids) {
#       
#       e1 <- bquote(dat[J(.(aeids)), 
#                        wndw := mad(resp[cndx %in% 1:2 & wllt == "t"], 
#                                    na.rm = TRUE) * 3,
#                        by = aeid])
#       e2 <- bquote(dat[aeid %in% .(aeids) & wllt %in% c("t", "c", "o"), 
#                        resp := blineShift(resp, logc, wndw), 
#                        by = list(aeid, spid)])
#       e3 <- bquote(dat[ , wndw := NULL])
#       list(e1, e2, e3)
#       
#     },
#     
#     resp.blineshift.50.spid = function(aeids) {
#       
#       e1 <- bquote(dat[aeid %in% .(aeids) & wllt %in% c("t", "c", "o"), 
#                        resp := blineShift(resp, logc, wndw = 50), 
#                        by = list(aeid, spid)])
#       list(e1)
#       
#     },
#     
#     none = function(aeids) {
#       
#       e1 <- bquote(dat[J(.(aeids)), resp := cval])
#       list(e1)
#       
#     },
#     
#     bval.apid.nwllstcwllslowconc.med = function(aeids) {
#       
#       e1 <- bquote(dat[J(.(aeids)), 
#                        bval := median(cval[(cndx %in% 1:2 & wllt %in% c("t","c")) | 
#                                              wllt == "n"],
#                                       na.rm = TRUE),
#                        by = list(aeid, apid)])
#       list(e1)
#     }
#     
#   )
#   
# }
# 
# #-------------------------------------------------------------------------------
