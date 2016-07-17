#-------------------------------------------------------------------------------
# is.odd: tests whether a value is odd or even, returning TRUE for odd
#-------------------------------------------------------------------------------

#' @title Check for odd numbers
#' 
#' @description 
#' \code{is.odd} takes an integer vector, \code{x}, and returns TRUE for odd
#' integers.
#' 
#' @param x An integer
#' 
#' @return \code{TRUE} for odd integers and \code{FALSE} for even integers.
#' 
#' @family tcpl abbreviations

is.odd <- function(x) {
  
  if (!is.integer(x)) x <- as.integer(x)
  x %% 2 != 0
  
}

#-------------------------------------------------------------------------------
