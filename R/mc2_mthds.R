#-------------------------------------------------------------------------------
# mc2_mthds: Load list of correction functions (to be used at level 2)
#-------------------------------------------------------------------------------

#' @name MC2_Methods
#' @title List of level 2 multiple-concentration correction functions
#' 
#' @description 
#' \code{mc2_mthds} returns a list of correction/transformation functions 
#' to be used during level 2 multiple-concentration processing.
#' 
#' @return A list functions
#' 
#' @seealso \code{\link{mc2}}, \code{\link{Method functions}} to query what
#' methods get applied to each acid
#' 
#' @details 
#' The functions contained in the list returned by \code{mc2_mthds} return
#' a list of expressions to be executed in the \code{mc2} (not exported) 
#' function environment. The functions are described here for reference 
#' purposes, The \code{mc2_mthds} function is not exported, nor is it 
#' intended for use.
#' 
#' All available methods are described in the Available Methods section, listed
#' by the function/method name. 
#' 
#' @section Available Methods:
#' 
#' More information about the level 2 multiple-concentration processing is 
#' available in the package vignette, "Data_processing."
#' 
#' \subsection{Correction Methods}{
#' \describe{
#'   \item{log2}{Transform the corrected response value (cval) to log-scale (base 2).}
#'   \item{log10}{Transform the corrected response value (cval) to log-scale (base 10).}
#'   \item{rmneg}{Exclude wells with negative corrected response values (cval) and downgrading 
#'   their well quality (wllq); if \eqn{cval<0, wllq=0}{cval<0, wllq=0}.}
#'   \item{rmzero}{Exclude wells with corrected response values (cval) equal to zero and 
#'   downgrading their well quality (wllq); if  \eqn{cval=0, wllq=0}{cval=0, wllq=0}.}
#'   \item{mult25}{Multiply corrected response value (cval) by 25; \eqn{25*cval}{25*cval}.}
#'   \item{mult100}{Multiply corrected response value (cval) by 100; \eqn{100*cval}{100*cval}.}
#'   \item{negshift}{Shift corrected response values (cval) by subtracting the minimum cval and 
#'   adding 1, such that the new minimum is 1; \eqn{cval-min+1}{cval-min+1}.}
#'   \item{mult2.5}{Multiply corrected response value (cval) by 2.5; \eqn{2.5*cval}{2.5*cval}.}
#'   \item{mult3}{Multiply corrected response value (cval) by 3; \eqn{3*cval}{3*cval}.}
#'   \item{mult6}{Multiply corrected response value (cval) by 6; \eqn{6*cval}{6*cval}.}
#'   \item{sub100}{Center data around zero by subtracting the corrected response value (cval) from 
#'   100; \eqn{100-cval}{100-cval}. Typically used if data was pre-normalized around 100 with responses 
#'   decreasing to 0.}
#'   \item{zscore.npwlls}{Convert the corrected response value (cval) to an absolute Z-Score based 
#'   on the neutral and positive control wells (wllts = n and p), by assay plate ID (apid); eqn{cval=|(cval-mean(cval~for~wllt~=~n~and~p)/sd(cval~for~wllt~=~n~and~p)|}{cval=
#'    |(cval-mean(cval for wllt = n and p)/sd(cval for wllt = n and p)|}.}
#'   \item{sub1}{Center data around zero by subtracting the corrected response value (cval) from 1; 
#'    \eqn{1-cval}{1-cval}. Typically used if data was pre-normalized around 1 with responses decreasing to 0.}
#'   }
#' }
#' 
#' \subsection{Aggregation Methods}{
#' \describe{
#'  \item{agg.mean.rep.apid}{Aggregate technical test replicates (wllt=t) by taking the plate-wise mean per sample id (spid), assay plate (apid), and concentration index (cndx).}
#'  \item{agg.median.rep.apid}{Aggregate technical test replicates (wllt=t) by taking the plate-wise median per sample id (spid), assay plate (apid), and concentration index (cndx).}
#'  \item{agg.percent.rep.spid}{Use for binary data. Aggregate technical replicates as percentage by taking the sum of rval (raw values) relative to total replicates per sample id (spid) and concentration index (cndx); \eqn{cval = (sum(rval)/.N)*100}{cval = (sum(rval)/.N)*100}.}
#'  \item{agg.percent.rep.spid.min1}{Use for binary data with variable number of replicates. Aggregate technical replicates as percentage by taking the sum of rval (raw values) relative to total replicates per per sample id (spid) and concentration index (cndx), where there is more than one replicate; \eqn{cval=(sum(rval)/.N)*100,~where~.N>1}{cval = (sum(rval)/.N)*100, where .N>1}.} Rvals are collapsed to one value per cndx.}
#'   }
#' 
#' @note
#' This function is not exported and is not intended to be used by the user.


mc2_mthds <- function() {
  
  list(
    
    log2 = function() {
      
      e1 <- bquote(dat[ , cval := log2(cval)])
      list(e1)
      
    },
    
    log10 = function() {
      
      e1 <- bquote(dat[ , cval := log10(cval)])
      list(e1)
      
    },
    
    rmneg = function() {
      
      e1 <- bquote(dat[cval < 0, c('cval', 'wllq') := list(NA_real_, 0)])
      list(e1)
      
    },
    
    rmzero = function() {
      
      e1 <- bquote(dat[cval == 0, c('cval', 'wllq') := list(NA_real_, 0)])
      list(e1)
      
    },
    
    mult25 = function() {
      
      e1 <- bquote(dat[ , cval := cval * 25])
      list(e1)
      
    },
    
    mult100 = function() {
      
      e1 <- bquote(dat[ , cval := cval * 100])
      list(e1)
      
    },
    
    negshift = function() {
      
      e1 <- bquote(dat[ , cval := cval - min(cval, na.rm = TRUE) + 1])
      list(e1)
      
    },
    
    mult2.5 = function() {
      
      e1 <- bquote(dat[ , cval := cval * 2.5])
      list(e1)
      
    },
    
    mult3 = function() {
      
      e1 <- bquote(dat[ , cval := cval * 3])
      list(e1)
      
    },
    
    mult6 = function() {
      
      e1 <- bquote(dat[ , cval := cval * 6])
      list(e1)
      
    },
    
    sub100 = function() {
      
      e1 <- bquote(dat[ , cval := 100 - cval])
      list(e1)
      
    },
    
    zscore.npwlls = function() {
      
      e1 <- bquote(dat[ , cval := abs((cval - mean(cval[wllt %in% c("n","p")])) / sd(cval[wllt %in% c("n","p")],na.rm = TRUE)),
                        by = list(acid, apid)])
      list(e1)
      
    },
    sub1 = function() {
      
      e1 <- bquote(dat[ , cval := 1 - cval])
      list(e1)
      
    }, 
    
    agg.mean.rep.apid = function() {
      
      e1 <- bquote(dat[wllt=="t", cval := mean(cval),
                        by = list(acid, spid, apid, cndx)])
      list(e1)
      
    },
    
    agg.median.rep.apid = function() {
      
      e1 <- bquote(dat[wllt=="t", cval := median(cval),
                        by = list(acid, spid, apid, cndx)])
      list(e1)

    },
    
    agg.percent.rep.spid = function() {
      
      e1 <- bquote(dat[ , cval := sum(cval)/.N * 100,
                        by = list(acid, spid, cndx)])
      list(e1)

    },
    
    agg.percent.rep.spid.min1 = function() {
      
      e1 <- bquote( dat[.N>1,cval := sum(cval)/.N * 100, 
                       by = list(acid, spid, cndx)][,repi:=rowid(conc), 
                            by= list(acid, spid, wllt, srcf, apid)][repi>1, wllq := 0,  
                                   by = list(acid, spid, cndx)])
      
    list(e1)
}
  )
}

#-------------------------------------------------------------------------------
