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
#' available in the package vignette, "Pipeline_Overview."
#' 
#' \describe{
#'   \item{log2}{Take the logarithm of cval with the base 2.}
#'   \item{log10}{Take the logarithm of cval with the base 10.}
#'   \item{rmneg}{Remove entries where cval is less than 0.}
#'   \item{rmzero}{Remove entries where cval is 0.}
#'   \item{mult25}{Multiply cval by 25.}
#'   \item{mult100}{Multiply cval by 100.}
#'   \item{negshift}{Shift cval by subtracting out the minimum of cval and 
#'   adding 1, such that the new minimum of cval is 1.}
#'   \item{mult25}{Multiply cval by 2.5.}
#'   \item{mult3}{Multiply cval by 3.}
#'   \item{mult6}{Multiply cval by 6.}
#' }
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
      
    }
    
  )
}

#-------------------------------------------------------------------------------
