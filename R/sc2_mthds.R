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
#'   \item{bmad3}{Add a cutoff value of 3*bmad.}
#'   \item{pc20}{Add a cutoff value of 20.}
#'   \item{log2_1.2}{Add a cutoff value of log2(1.2).}
#'   \item{log10_1.2}{Add a cutoff value of log10(1.2).}
#'   \item{bmad5}{Add a cutoff value of 5*bmad.}
#'   \item{bmad6}{Add a cutoff value of 6*bmad.}
#'   \item{bmad10}{Add a cutoff value of 10*bmad.}
#'   \item{pc30orbmad3}{Add a cutoff value of either 30 or 3*bmad, whichever
#'   is less.}
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
      
      e1 <- bquote(coff <- c(coff, 20))
      list(e1)
      
    },
    
    log2_1.5 = function() {
      
      e1 <- bquote(coff <- c(coff, log2(1.5)))
      list(e1)
      
    }
    
  )
}

#-------------------------------------------------------------------------------
