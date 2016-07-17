#-------------------------------------------------------------------------------
# tcplObjGnls: Generate a gain-loss model objective function to optimize
#-------------------------------------------------------------------------------

#' @rdname Models
#' 
#' @section Gain-Loss Model (gnls): 
#' \code{tcplObjGnls} calculates the likelyhood for a 5 parameter model as the 
#' product of two Hill models with the same top and both bottoms equal to 0. 
#' The parameters passed to \code{tcplObjGnls} by \code{p} are (in order) top 
#' (\eqn{\mathit{tp}}), gain log AC50 (\eqn{\mathit{ga}}), gain hill coefficient (\eqn{gw}), 
#' loss log AC50 \eqn{\mathit{la}}, loss hill coefficient \eqn{\mathit{lw}}, and the scale 
#' term (\eqn{\sigma}). The gain-loss model value \eqn{\mu_{i}}{\mu[i]} for the 
#' \eqn{i^{th}}{ith} observation is given by:
#' \deqn{
#' g_{i} = \frac{1}{1 + 10^{(\mathit{ga} - x_{i})\mathit{gw}}}
#' }{
#' g[i] = 1/(1 + 10^(ga - x[i])*gw)}
#' \deqn{
#' l_{i} = \frac{1}{1 + 10^{(x_{i} - \mathit{la})\mathit{lw}}}
#' }{
#' l[i] = 1/(1 + 10^(x[i] - la)*lw)}
#' \deqn{\mu_{i} = \mathit{tp}(g_{i})(l_{i})}{\mu[i] = tp*g[i]*l[i]}
#' where \eqn{x_{i}}{x[i]} is the log concentration for the \eqn{i^{th}}{ith} 
#' observation.
#' 
#' @importFrom stats dt
#' @export

tcplObjGnls <- function(p, lconc, resp) {
  
  ### This function takes creates an objective function to be optimized using
  ### the starting gain-loss parameters, log concentration, and response. 
  ###
  ### Arguments: 
  ###   p:     a numeric vector of length 5 containg the starting values for 
  ###          the gain-loss model, in order: top, gain log AC50, gain hill 
  ###          coefficient, loss log AC50, loss hill coefficient and log error 
  ###          term
  ###   lconc: a numeric vector containing the log concentration values to  
  ###          produce the objective function
  ###   lresp: a numeric vector containing the response values to produce the 
  ###          objective function
  ### 
  ### Value:
  ###   An objective function for the gain-loss model and the given conc-resp 
  ###   data
  
  gn <- 1/(1 + 10^((p[2] - lconc)*p[3]))
  ls <- 1/(1 + 10^((lconc - p[4])*p[5]))
  mu <- p[1]*gn*ls
  sum(dt((resp - mu)/exp(p[6]), df = 4, log = TRUE) - p[6])
  
}

#-------------------------------------------------------------------------------
