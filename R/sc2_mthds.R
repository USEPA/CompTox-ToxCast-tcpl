#-------------------------------------------------------------------------------
# sc2_mthds: Load list of sc2 method functions
#-------------------------------------------------------------------------------

#' @name SC2_Methods
#' @title List of level 2 single-concentration hit-call functions
#' 
#' @description 
#' \code{sc2_mthds} returns a list of functions to be used during level 2 
#' single-concentration processing.
#' 
#' @return A list functions
#' 
#' @seealso \code{\link{sc2}}, \code{\link{Method functions}} to query what
#' methods get applied to each acid
#' 
#' @details 
#' The functions contained in the list returned by \code{sc2_mthds} return
#' a list of expressions to be executed in the \code{sc2} (not exported) 
#' function environment. The functions are described here for reference 
#' purposes, The \code{sc2_mthds} function is not exported, nor is it 
#' intended for use.
#' 
#' All available methods are described in the Available Methods section, listed
#' by the function/method name. 
#' 
#' @section Available Methods:
#' 
#' More information about the level 2 single-concentration processing is 
#' available in the package vignette, "Pipeline_Overview."
#' 
#' \describe{
#'   \item{bmad3}{Add a cutoff value of 3 multiplied by the baseline median absolute deviation 
#'   (bmad). By default, bmad is calculated using test compound wells (wllt = t) for the endpoint.}
#'   \item{pc20}{Add a cutoff value of 20. Typically for percent of control data.}
#'   \item{log2_1.2}{Add a cutoff value of log2(1.2). Typically for fold change data.}
#'   \item{log10_1.2}{Add a cutoff value of log10(1.2). Typically for fold change data.}
#'   \item{bmad5}{Add a cutoff value of 5 multiplied the baseline median absolute deviation (bmad). 
#'   By default, bmad is calculated using test compound wells (wllt = t) for the endpoint.}
#'   \item{bmad6}{Add a cutoff value of 6 multiplied by the baseline median absolute deviation 
#'   (bmad). By default, bmad is calculated using test compound wells (wllt = t) for the endpoint.}
#'   \item{bmad10}{Add a cutoff value of 10 multiplied by the baseline median absolute deviation 
#'   (bmad). By default, bmad is calculated using test compound wells (wllt = t) for the endpoint.}
#'   \item{pc30orbmad3}{Add a cutoff value of either 30 or 3 multiplied by the baseline median 
#'   absolute deviation (bmad), whichever is less. By default, bmad is calculated using test 
#'   compound wells (wllt = t) for the endpoint.}
#'   \item{pc0.88}{Add a cutoff value of 0.88. Typically for percent of control data.}
#'   \item{log2_1.5}{Add a cutoff value of log2(1.5). Typically for fold change data.}
#'   \item{pc25}{Add a cutoff value of 25. Typically for percent of control data.}
#'   \item{ow_bmad_nwells}{Overwrite the default baseline median absolute value (bmad) with a bmad 
#'   calculated using neutral control wells (wllt = n).}
#'   \item{bmad2}{Add a cutoff value of 2 multiplied by the baseline median absolute deviation 
#'   (bmad). By default, bmad is calculated using test compound wells (wllt = t) for the endpoint.}
#'   \item{bmad1}{Add a cutoff value of 1 multiplied by baseline median absolute deviation (bmad). 
#'   By default, bmad is calculated using test compound wells (wllt = t) for the endpoint.}
#'   \item{log2_0.76}{Add a cutoff value of 0.76 for log2-transformed data. This was a custom 
#'   threshold value set for endpoint id 1690 (formerly aeid 1691).}
#'   \item{pc30}{Add a cutoff value of 30. Typically for percent of control data.}
#'   \item{bmad1.5}{Add a cutoff value of 1.5 multiplied by the baseline median absolute deviation 
#'   (bmad). By default, bmad is calculated using test compound wells (wllt = t) for the endpoint.}
#'   \item{ow_bidirectional_false}{Overwrite the max_med and max_tmp values, which were calculated 
#'   using absolute value, to a calculation not using absolute value for non-bidirectional data.}
#' }
#' 
#' @note
#' This function is not exported and is not intended to be used by the user.


sc2_mthds <- function() {
  
  list(
    
    bmad3 = function() {
      
      e1 <- bquote(coff <- c(coff, dat[ , unique(bmad)*3]))
      list(e1)
      
    },
    
    pc20 = function() {
      
      e1 <- bquote(coff <- c(coff, 20))
      list(e1)
      
    },
    
    log2_1.2 = function() {
      
      e1 <- bquote(coff <- c(coff, log2(1.2)))
      list(e1)
      
    },
    
    log10_1.2 = function() {
      
      e1 <- bquote(coff <- c(coff, log10(1.2)))
      list(e1)
      
    },
    
    bmad5 = function() {
      
      e1 <- bquote(coff <- c(coff, dat[ , unique(bmad)*5]))
      list(e1)
      
    },
    
    bmad6 = function() {
      
      e1 <- bquote(coff <- c(coff, dat[ , unique(bmad)*6]))
      list(e1)
      
    },
    
    bmad10 = function() {
      
      e1 <- bquote(coff <- c(coff, dat[ , unique(bmad)*10]))
      list(e1)
      
    },
    
    pc30orbmad3 = function() {
      
      e1 <- bquote(coff <- c(coff, dat[ , min(30, unique(bmad)*3)]))
      list(e1)
      
    },
    
    pc0.88 = function() {
      
      e1 <- bquote(coff <- c(coff, 0.88))
      list(e1)
      
    },
    
    log2_1.5 = function() {
      
      e1 <- bquote(coff <- c(coff, log2(1.5)))
      list(e1)
      
    },
  	
    pc25 = function() {
      
      e1 <- bquote(coff <- c(coff, 25))
      list(e1)

    },    
    
    ow_bmad_nwells = function() {
      
      e1 <- bquote(dat[ , bmad := mad(resp[wllt == "n"], na.rm = TRUE)])
      list(e1)
      
    },
    
    ow_bidirectional_false = function() {
      
      e1 <- bquote(dat[ , c("max_med","max_tmp") := list(max(tmp), tmp[which.max(tmp)]), by = spid])
      list(e1)
      
    },
    
    bmad2 = function() {
      
      e1 <- bquote(coff <- c(coff, dat[ , unique(bmad)*2]))
      list(e1)
      
    },
    
    log2_0.76 = function() {
      
      e1 <- bquote(coff <- c(coff, 0.76))
      list(e1)
    },
    
    bmad1 = function() {
      
      e1 <- bquote(coff <- c(coff, dat[ , unique(bmad)]))
      list(e1)
      
    },
    
    pc30 = function() {
      
      e1 <- bquote(coff <- c(coff, 30))
      list(e1)
      
    },
    
     bmad1.5 = function() {
      
      e1 <- bquote(coff <- c(coff, dat[ , unique(bmad)*1.5]))
      list(e1)
      
    }

  )
}

#-------------------------------------------------------------------------------
