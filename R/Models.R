#' @name Models
#' @aliases Models
#' @title Model objective functions
#'
#' @description 
#' These functions take in the dose-response data and the model parameters, and
#' return a likelyhood value. They are intended to be optimized using 
#' \code{\link{constrOptim}} in the \code{\link{tcplFit}} function.
#'
#' @param p Numeric, the parameter values. See details for more information.
#' @param lconc Numeric, the log10 concentration values
#' @param resp Numeric, the response values
#' 
#' @details 
#' These functions produce an estimated value based on the model and given 
#' parameters for each observation. Those estimated values are then used with 
#' the observed values and a scale term to calculate the log-likelyhood.
#' 
#' Let \eqn{t(z,\nu)} be the Student's t-ditribution with \eqn{\nu} degrees of
#' freedom, \eqn{y_{i}}{y[i]} be the observed response at the \eqn{i^{th}}{ith} 
#' observation, and \eqn{\mu_{i}}{\mu[i]} be the estimated response at the \eqn{i^{th}}{ith} 
#' observation. We calculate \eqn{z_{i}}{z[i]} as:
#' \deqn{
#' z_{i} = \frac{y_{i} - \mu_{i}}{e^\sigma}
#' }{
#' z[i] = (y[i] - \mu[i])/e^\sigma
#' }
#' where \eqn{\sigma} is the scale term. Then the log-likelyhood is:
#' \deqn{
#' \sum_{i=1}^{n} [ln(t(z_{i}, 4)) - \sigma]
#' }{
#' sum_{i=1}^{n} [ln(t(z[i], 4)) - \sigma]}
#' Where \eqn{n} is the number of observations.
#' 
#' @return The log-likelyhood.
NULL
