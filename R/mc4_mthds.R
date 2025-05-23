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
#' \code{aeids} (a numeric vector of aeid values) and returns a list of expressions 
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
#' available in the package vignette, "Data_processing."
#' 
#'   \describe{
#'     \item{bmad.aeid.lowconc.twells}{Calculate the baseline median absolute value (bmad) as the 
#'     median absolute deviation of normalized response values (rep) for test compound wells (wllt = t) 
#'     with concentration index (cndx) equal to 1 or 2. Calculate one standard deviation of the normalized 
#'     response for test compound wells (wllt = t) with a concentration index (cndx) of 1 or 2; 
#'     onesd = sqrt(sum((resp - mean resp)^2)/sample size - 1). Onesd is used to establish BMR and therefore required for tcplfit2 processing.}
#'     \item{bmad.aeid.lowconc.nwells}{Calculate the baseline median absolute value (bmad) as the 
#'     median absolute deviation of normalized response values (resp) for neutral control wells (wllt = n).
#'     Calculate one standard deviation of the normalized response for neutral control wells (wllt = n);
#'     onesd = sqrt(sum((resp - mean resp)^2)/sample size - 1). Onesd is used to establish BMR and therefore required for tcplfit2 processing.}
#'     \item{bidirectional.false}{Limits bidirectional fitting and processes data in positive 
#'     analysis direction only. Use for gain-of-signal or inverted data.}
#'     \item{bmad5.onesd16.static}{Replace baseline median absolute deviation
#'     (bmad) with 5 and one standard deviation (osd) of the normalized response
#'     for test compound wells (wllt = t) with a concentration index (cndx) of 1
#'     or 2 with 16. Typically used for binary data where values would otherwise
#'     be 0; non-zero values are required for tcplfit2 processing.}
#'     \item{no.unbounded.models}{Exclude unbounded models and only fit data to bounded models (hill, gnls, exp4 and exp5).}
#'   }
#' 
#' @note
#' This function is not exported and is not intended to be used by the user.

mc4_mthds <- function() {
  
  list(
    
    bmad.aeid.lowconc.twells = function() {
      
      e1 <- bquote(dat[ , bmad := mad(resp[cndx %in% 1:2 & wllt == "t"], na.rm = TRUE)])
      e2 <- bquote(dat[ , osd := sd(resp[cndx %in% 1:2 & wllt == "t"], na.rm = TRUE)])
      list(e1, e2)
      
    },
    
    bmad.aeid.lowconc.nwells = function() {
      
      e1 <- bquote(dat[ , bmad := mad(resp[wllt == "n"], na.rm = TRUE)])
      e2 <- bquote(dat[ , osd := sd(resp[wllt == "n"], na.rm = TRUE)])
      list(e1, e2)
      
    },
    
    bidirectional.false = function() {
      
      e1 <- bquote(dat[ ,bidirectional := FALSE])
      list(e1)
      
    },
    
    bmad5.onesd16.static = function() {
      
      e1 <- bquote(dat[ , bmad := 5])
      e2 <- bquote(dat[ , osd := 16])
      list(e1, e2)
      
    },
    no.unbounded.models = function() {
      
      e1 <- bquote(dat[ ,fitmodels := list(c("cnst", "hill","gnls" ,"exp4", "exp5"))])
      list(e1)
      
    }

  )
}
