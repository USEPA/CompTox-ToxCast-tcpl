#-------------------------------------------------------------------------------
# lu: Length of unique
#-------------------------------------------------------------------------------

#' @title Abbreviation for \code{length(unique(x))}
#' 
#' @description 
#' \code{lu} takes a logical vector, \code{x}, and returns 
#' \code{length(unique(x))}.
#' 
#' @param x A logical
#' 
#' @return The unique of the \code{TRUE} values in \code{x}
#' 
#' @family tcpl abbreviations
#' @seealso \code{\link{unique}}, \code{\link{which}}
#' @export

lu <- function(x) length(unique(x))

#-------------------------------------------------------------------------------
