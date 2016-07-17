#-------------------------------------------------------------------------------
# tcplObjHill: Generate a hill model objective function to optimize
#-------------------------------------------------------------------------------

#' @rdname Models
#' 
#' @section Hill Model (hill): 
#' \code{tcplObjHill} calculates the likelyhood for a 3 parameter Hill model
#' with the bottom equal to 0. The parameters passed to \code{tcplObjHill} by 
#' \code{p} are (in order) top (\eqn{\mathit{tp}}), log AC50 (\eqn{\mathit{ga}}), hill 
#' coefficient (\eqn{\mathit{gw}}), and the scale term (\eqn{\sigma}). The hill model 
#' value \eqn{\mu_{i}}{\mu[i]} for the \eqn{i^{th}}{ith} observation is given 
#' by:
#' \deqn{
#' \mu_{i} = \frac{tp}{1 + 10^{(\mathit{ga} - x_{i})\mathit{gw}}}
#' }{
#' \mu[i] = tp/(1 + 10^(ga - x[i])*gw)}
#' where \eqn{x_{i}}{x[i]} is the log concentration for the \eqn{i^{th}}{ith} 
#' observation.
#' 
#' @importFrom stats dt
#' @export

tcplObjHill <- function(p, lconc, resp) {
  
  ### This function takes creates an objective function to be optimized using
  ### the starting hill parameters, log concentration, and response. 
  ###
  ### Arguments: 
  ###   p:     a numeric vector of length 4 containg the starting values for 
  ###          the hill model, in order: top, log AC50, hill 
  ###          coefficient, and log error term
  ###   lconc: a numeric vector containing the log concentration values to  
  ###          produce the objective function
  ###   lresp: a numeric vector containing the response values to produce the 
  ###          objective function
  ### 
  ### Value:
  ###   An objective function for the hill model and the given conc-resp data
  
  mu <- p[1]/(1 + 10^((p[2] - lconc)*p[3]))
  sum(dt((resp - mu)/exp(p[4]), df = 4, log = TRUE) - p[4])
  
}

#-------------------------------------------------------------------------------
