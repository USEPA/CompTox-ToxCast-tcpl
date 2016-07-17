#-------------------------------------------------------------------------------
# mc5_mthds: Load list of cutoff methods (to be used at level 5)
#-------------------------------------------------------------------------------

#' @name MC5_Methods
#' @title Load list of level 5 multiple-concentration cutoff methods
#'
#' @description
#' \code{mc5_mthds} returns a list of additional activity cutoff methods
#' to be used during level 5 multiple-concentration processing.
#'
#' @return A list of functions
#'
#' @seealso \code{\link{mc5}}, \code{\link{Method functions}} to query what
#' methods get applied to each aeid
#'
#' @section Available Methods:
#'
#' More information about the level 5 multiple-concentration processing is
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
#'   \item{log2_2}{Add a cutoff value of log2(2).}
#'   \item{log10_2}{Add a cutoff value of log10(2).}
#'   \item{neglog2_0.88}{Add a cutoff value of -1*log2(0.88).}
#' }


mc5_mthds <- function() {

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

    maxmed20pct = function() {

      e1 <- bquote(coff <- c(coff, dat[ , max(max_med)*.20]))
      list(e1)

    },

    pc70 = function() {

      e1 <- bquote(coff <- c(coff, 70))
      list(e1)

    },

    pc50 = function() {

      e1 <- bquote(coff <- c(coff, 50))
      list(e1)

    },

    log2_2 = function() {

      e1 <- bquote(coff <- c(coff, log2(2)))
      list(e1)

    },

    log10_2 = function() {

      e1 <- bquote(coff <- c(coff, log10(2)))
      list(e1)

    },
    
    neglog2_0.88 = function() {
      
      e1 <- bquote(coff <- c(coff, -1*log2(0.88)))
      list(e1)
      
    }
  )
}

#-------------------------------------------------------------------------------
