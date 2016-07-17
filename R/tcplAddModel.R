#' @title Draw a tcpl Model onto an exisiting plot
#' 
#' @description
#' \code{tcplAddModel} draws a a line for one of the tcpl Models (see 
#' \code{\link{Models}} for more information) onto an existing plot. 
#' 
#' @param pars List of parameters from level 4 or 5 output
#' @param modl Character of length 1, the model to plot: 'cnst,' 'hill,' or 
#' 'gnls' 
#' @param adj Numeric of length 1, an adjustment factor, see details for more
#' information
#' @param \dots Additional arguments passed to \code{curve}
#' 
#' @details
#' \code{tcplAddModel} draws the model line assuming the x-axis represents log
#' base 10 concentration.
#' 
#' If \code{modl} is NULL, the function checks \code{pars$modl} and will return 
#' an error if \code{pars$modl} is also NULL.
#' 
#' \code{adj} is intended to scale the models, so that models with different 
#' response units can be visualized on a single plot. The recommended value for 
#' \code{adl} is \code{1/(3*bmad)} for level 4 data and \code{1/coff} for level 
#' 5 data. If \code{adj} is NULL the function will check \code{pars$adj} and 
#' set \code{adj} to 1 if \code{pars$adj} is also NULL.
#' 
#' @examples
#' ## Create some dummy data to plot
#' logc <- 1:10
#' r1 <- sapply(logc, tcplHillVal, ga = 5, tp = 50, gw = 0.5)
#' r2 <- log2(sapply(logc, tcplHillVal, ga = 4, tp = 30, gw = 0.5))
#' p1 <- tcplFit(logc = logc, resp = r1, bmad = 10)
#' p2 <- tcplFit(logc = logc, resp = r2, bmad = log2(1.5))
#' 
#' ## In the dummy data above, the two plots are on very different scales
#' plot(r1 ~ logc, pch = 16, ylab = "raw response")
#' tcplAddModel(pars = p1, modl = "hill")
#' points(r2 ~ logc)
#' tcplAddModel(pars = p2, modl = "hill", lty = "dashed")
#' 
#' ## To visualize the two curves on the same plot for comparison, we can 
#' ## scale the values to the bmad, such that a scaled response of 1 will equal
#' ## the bmad for each curve.
#' plot(r1/10 ~ logc, pch = 16, ylab = "scaled response")
#' tcplAddModel(pars = p1, modl = "hill", adj = 1/10)
#' points(r2/log2(5) ~ logc)
#' tcplAddModel(pars = p2, modl = "hill", adj = 1/log2(5), lty = "dashed")
#' 
#' @seealso \code{\link{Models}}, \code{\link{tcplPlotFits}}
#' 
#' @importFrom graphics curve
#' @export

tcplAddModel <- function(pars, modl = NULL, adj = NULL, ...) {
  
  if (is.null(modl)) modl <- pars$modl
  if (is.null(modl)) stop("'modl' is not defined.")
  if (!is.null(adj)) pars$adj <- adj
  if (is.null(pars$adj)) pars$adj <- 1
  
  cnst <- function(x) 0*x
  hill <- function(x) with(pars, hill_tp*adj/(1 + 10^((hill_ga - x)*hill_gw)))
  gnls <- function(x) {
    with(pars, {
      h1 <- (1/(1 + 10^((gnls_ga - x)*gnls_gw)))
      h2 <- (1/(1 + 10^((x - gnls_la)*gnls_lw)))
      gnls_tp*adj*h1*h2
    })
  }
  
  do.call(curve,
          list(as.name(modl),
               add = TRUE,
               from = pars$logc_min,
               to = pars$logc_max,
               n = 1e4,
               ...))
  
}