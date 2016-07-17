#-------------------------------------------------------------------------------
# lw: Length of which is true
#-------------------------------------------------------------------------------

#' @title Abbreviation for \code{length(which(x))}
#' 
#' @description 
#' \code{lw} takes a logical vector, \code{x}, and returns 
#' \code{length(which(x))}.
#' 
#' @param x A logical
#' 
#' @return The length of the \code{TRUE} values in \code{x}
#' 
#' @family tcpl abbreviations
#' @seealso \code{\link{length}}, \code{\link{which}}
#' @export

lw <- function(x) length(which(x))

#-------------------------------------------------------------------------------
