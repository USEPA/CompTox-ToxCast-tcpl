#-------------------------------------------------------------------------------
# tcplAICProb: Calculate the AIC probabilities
#-------------------------------------------------------------------------------

#' @title Calculate the AIC probabilities
#' 
#' @description
#' \code{tcplAICProb} Calculates the probability that the model best represents 
#' the data based on the AIC value for each model.
#' 
#' @param \dots Numeric vectors of AIC values
#' 
#' @details
#' The function takes vectors of AIC values. Each vector represents the model
#' AIC values for multiple observation sets. Each vector must contain the same 
#' number and order of observation sets. The calculation assumes every possible
#' model is accounted for, and the results should be interpreted accordingly. 
#'             
#' @examples 
#' ## Returns the probability for each model, given models with AIC values 
#' ## ranging from 80 to 100
#' tcplAICProb(80, 85, 90, 95, 100)
#' 
#' ## Also works for vectors
#' m1 <- c(95, 195, 300) ## model 1 for three different observations
#' m2 <- c(100, 200, 295) ## model 2 for three different observations
#' tcplAICProb(m1, m2)
#' 
#' @return A vector of probability values for each model given, as a list.
#'             
#' @seealso \code{\link{tcplFit}}, \code{\link{AIC}} for more information 
#' about AIC values.
#' 
#' @export


tcplAICProb <- function(...) {
  
  ### Calculate the AIC probabilities for the given models
  
  aics <- list(...)
  
  lens <- sapply(aics, length)
  if (abs(max(lens) - min(lens)) > 0) stop("All inputs must be same length.")
  
  maic <- pmin(..., na.rm = TRUE)
  
  di <- lapply(aics, function(x) x - maic)
  l  <- lapply(di,   function(x) exp(-x/2))
  
  lsum <- apply(do.call(cbind, l), 1, sum, na.rm = TRUE)
  prob <- lapply(l, function(x) x/lsum)
  
  prob
  
}

#-------------------------------------------------------------------------------
