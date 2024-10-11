#-------------------------------------------------------------------------------
# blineShift: Shift baseline to 0
#-------------------------------------------------------------------------------

#' @name blineShift
#' @title Shift the baseline to 0
#' 
#' @description 
#' \code{blineShift} Takes in dose-response data and shifts the baseline
#' to 0 based on the window. 
#' 
#' @param resp Numeric, the response values
#' @param conc Numeric, the concentration values
#' @param wndw Numeric, the threshold window 
#' 
#' @note
#' This function is not exported and is not intended to be used by the user.
#' 
#' @return A numeric vector containing the shifted response values
#' 
#' @seealso \code{\link{mc3_mthds}}, \code{\link{mc3}}
#' 
#' @importFrom stats median lm 

blineShift <- function(resp, conc, wndw) {
  
  if (any(is.na(resp))) return(resp)
  if (length(unique(conc)) < 4) return(resp)
  
  wndw <- unique(wndw)[1]
  ordr <- order(conc)
  resp <- resp[ordr]
  conc <- conc[ordr]
  
  uconc <- unique(conc)
  nconc <- length(uconc)

  low <- 1:max(ceiling(nconc/4), 2)
  rsub <- resp[which(conc %in% uconc[low])]
  csub <- log10(conc[which(conc %in% uconc[low])])
  low_med <- median(rsub)
  m <- lm(rsub ~ csub)$coefficients["csub"]
  if (is.na(m)) m <- 0
  test1 <- abs(low_med) <= wndw
  test2 <- abs(m) <= wndw/low[length(low)]
  if (test1 & test2) resp <- resp - low_med
  
  resp[order(ordr)]
  
}

#-------------------------------------------------------------------------------
