#-------------------------------------------------------------------------------
# tcplHillACXX: Calculate the concentration for a given activity level
#-------------------------------------------------------------------------------

#' @rdname hill_utils
#' @export

tcplHillACXX <- function(XX, tp, ga, gw, bt = 0) {
  
  y <- tp * XX/100
  ga - log10((tp - bt)/(y - bt) - 1)/gw
  
}

#-------------------------------------------------------------------------------
