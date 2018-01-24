#-------------------------------------------------------------------------------
# tcplFit: Fit the data
#-------------------------------------------------------------------------------
library(numDeriv)
#' @title Fit the data with the constant, hill, and gain-loss models
#'
#' @description
#' \code{tcplFit} fits the constant, hill, and gain-loss models to the given data
#' and returns some summary statistics and the fit parameters in a list.
#'
#' @param logc Numeric, log concentration values
#' @param resp Numeric, normalized response values
#' @param bmad Numeric, the baseline median absolute deviation for the entire
#'             assay
#' @param force.fit Logical, TRUE indicates to attempt fitting every
#'                  concentration series
#' @param \dots Any other data to be included in list output.
#' @param bidirectional Boolean If TRUE, bidirectional negative data before fitting (default=FALSE)
#'            The original version of the code required the data to start at small
#'            values and rise, so that negative curves had to be bidirectionalped outside
#'            the function, and TOP was always positive. Setting bidirectional to TRUE
#'            allows both rising and falling curves
#' @param verbose Boolean If TRUE print warning messages
#' @details
#' when at least one median value is greater than 3*bmad.
#'
#' @examples
#' logc <- 1:10
#' resp <- sapply(1:10, tcplHillVal, ga = 5, tp = 50, gw = 0.5)
#' params <- tcplFit(logc = logc, resp = resp, bmad = 10)
#' plot(resp ~ logc)
#' tcplAddModel(pars = params, modl = "hill")
#'
#' @return List of summary values and fit parameters for the given data.
#'
#' @seealso \code{\link{tcplObjCnst}}, \code{\link{tcplObjHill}},
#' \code{\link{tcplObjGnls}}, \code{\link{constrOptim}}
#'
#' @importFrom numDeriv hessian
#' @importFrom stats optim constrOptim median mad
#' @importFrom methods is
#' @export

tcplFit <- function(logc, resp, bmad, force.fit = FALSE, bidirectional = TRUE, verbose = FALSE,...) {

  ## Variable-binding to pass R CMD Check
  hill_tp <- hill_ga <- hill_gw <- gnls_ga <- gnls_gw <- gnls_la <- NULL
  gnls_lw <- gnls_tp <- hill_tp_sd <- hill_ga_sd <- hill_gw_sd <- NULL
  hill_er <- hill_er_sd <- gnls_tp_sd <- gnls_ga_sd <- gnls_gw_sd <- NULL
  gnls_la_sd <- gnls_lw_sd <- gnls_er <- gnls_er_sd <- NULL

  fenv <- environment()

  bmad <- min(bmad)
  rmns <- tapply(resp, logc, mean)
  rmds <- tapply(resp, logc, median)
  if(!bidirectional) mmed <- max(rmds)
  else {
    val1 <- max(rmds)
    val2 <- min(rmds)
    if(abs(val1)>abs(val2)) mmed <- val1
    else mmed <- val2
  }
  mmed_conc <- as.numeric(names(which.max(abs(rmds))))

  hprs <- paste0("hill_", c("tp", "ga", "gw", "er"))
  hsds <- paste0("hill_", c("tp", "ga", "gw", "er"), "_sd")
  gprs <- paste0("gnls_", c("tp", "ga", "gw", "la", "lw", "er"))
  gsds <- paste0("gnls_", c("tp", "ga", "gw", "la", "lw", "er"), "_sd")

  resp_max <- max(resp)
  resp_min <- min(resp)
  logc_med <- median(logc)
  logc_min <- min(logc)
  logc_max <- max(logc)

  ncnc <- lu(logc)
  npts <- length(resp)
  nrep <- as.numeric(median(tapply(resp, logc, lu))) # Meidan number replicates
  nmed_gtbl <- lw(rmds >= 3 * bmad) # Number of medians above 3 * bmad
  hill_modl <- NULL
  gnls_modl <- NULL

  ## Do not fit anything with less than four concentrations of data.
  if (length(rmds) >= 4) {
    er_est <- if ((rmad <- mad(resp)) > 0) log(rmad) else log(1e-32)

    ###----------------------- Fit the Constant Model -----------------------###
    cfit <- optim(er_est,
                  tcplObjCnst,
                  method = "Brent",
                  lower = er_est - 2,
                  upper = er_est + 2,
                  control = list(fnscale = -1,
                                 reltol = 1e-4,
                                 maxit = 500),
                  resp = resp)
    if (!is(cfit, "try-error")) {
      cnst <- 1L
      cnst_er <- cfit$par
      caic <- 2 - 2*cfit$value # 2*length(cfit$par) - 2*cfit$value

      ## Calculate the rmse for constant
      crme <- sqrt(mean((0 - resp)^2, na.rm = TRUE))
    } else {
      cnst <- 0L
      cnst_er <- NA_real_
      caic <- NA_integer_
      crme <- NA_real_
    }

    if (lw(abs(rmds) >= 3*bmad) > 0 | force.fit) {
      ###------------------------ Fit the Hill Model ------------------------###
      ## Starting parameters for the Hill Model
      # cind <- (ceiling(length(meds)/2) + 1):length(meds)
      h <- c(mmed, # top (tp)
             mmed_conc - 1, # logAC50 (ga)
             1.2, # hill coefficient (gw)
             er_est) # logSigma (er)
       if (h[1] == 0) h[1] <- 0.1

      ## Generate the bound matrices to constrain the model.
      #                tp   ga   gw   er
      hUi <- matrix(c( 1,   0,   0,   0,
                      -1,   0,   0,   0,
                       0,   1,   0,   0,
                       0,  -1,   0,   0,
                       0,   0,   1,   0,
                       0,   0,  -1,   0),
                    byrow = TRUE, nrow = 6, ncol = 4)
      if(!bidirectional) {
        hbnds <- c(0, -1.2*resp_max, # tp bounds
                   logc_min - 1, -(logc_max + 0.5), # ga bounds
                   0.3, -8) # gw bounds
      }
      else {
        val <- 1.2*max(abs(resp_min),abs(resp_max))
        hbnds <- c(-val,-val, # tp bounds
                   logc_min - 1, -(logc_max + 0.5), # ga bounds
                   0.3, -8) # gw bounds
      }
      hCi <- matrix(hbnds, nrow = 6, ncol = 1)
      ## Optimize the hill model
      hfit <- try(constrOptim(h,
                              tcplObjHill,
                              ui = hUi,
                              ci = hCi,
                              mu = 1e-6,
                              method = "Nelder-Mead",
                              control = list(fnscale = -1,
                                             reltol = 1e-10,
                                             maxit = 6000),
                              lconc = logc,
                              resp = resp),
                  silent = !verbose)
      if(verbose) cat("hill >>>",hfit$counts[1],hfit$convergence,"\n")
      ## Generate some summary statistics
      if (!is(hfit, "try-error")) { # Hill model fit the data

        hill <- 1L
        haic <- 8 - 2*hfit$value # 2*length(hfit$par) - 2*hfit$value
        mapply(assign,
               c(hprs),
               hfit$par,
               MoreArgs = list(envir = fenv))

        ## Calculate rmse for hill
        hill_modl <- hill_tp/(1 + 10^((hill_ga - logc)*hill_gw))
        hrme <- sqrt(mean((hill_modl - resp)^2, na.rm = TRUE))

        ## Calculate the sd for the hill parameters
        hfit$cov <- try(solve(-hessian(tcplObjHill,
                                       hfit$par,
                                       lconc = logc,
                                       resp = resp)),
                        silent = !verbose)

        if (!is(hfit$cov, "try-error")) { # Could invert hill Hessian

          hcov <- 1L
          hdiag_sqrt <- suppressWarnings(sqrt(diag(hfit$cov)))
          if (any(is.nan(hdiag_sqrt))) {
            mapply(assign,
                   hsds,
                   NaN,
                   MoreArgs = list(envir = fenv))
          } else {
            mapply(assign,
                   hsds,
                   hdiag_sqrt,
                   MoreArgs = list(envir = fenv))
          }

        } else { # Could not invert hill Hessian

          hcov <- 0L
          mapply(assign,
                 c(hsds),
                 NA_real_,
                 MoreArgs = list(envir = fenv))

        }

      } else { # Hill model did not fit the data

        hill <- 0L
        haic <- NA_real_
        hcov <- NA_integer_
        hrme <- NA_real_

        mapply(assign,
               c(hprs, hsds),
               NA_real_,
               MoreArgs = list(envir = fenv))

      }

      ###--------------------- Fit the Gain-Loss Model ----------------------###
      ## Starting parameters for the Gain-Loss Model
      # cind <- (ceiling(length(meds)/2) + 1):length(meds)
      g <- c(mmed, # top (tp)
             mmed_conc - 1, # gain logAC50 (ga)
             1.2, # gain hill coefficient (gw)
             mmed_conc + 0.1, # loss logAC50 (la)
             5, # loss hill coefficient (lw)
             er_est) # logSigma (er)
      if (g[1] == 0) g[1] <- 0.1
      ## Generate the bound matrices to constrain the model.
      #                tp   ga   gw   la   lw   er
      gUi <- matrix(c( 1,   0,   0,   0,   0,   0,
                      -1,   0,   0,   0,   0,   0,
                       0,   1,   0,   0,   0,   0,
                       0,  -1,   0,   0,   0,   0,
                       0,   0,   1,   0,   0,   0,
                       0,   0,  -1 ,  0,   0,   0,
                       0,   0,   0,   1,   0,   0,
                       0,   0,   0,  -1,   0,   0,
                       0,   0,   0,   0,   1,   0,
                       0,   0,   0,   0,  -1,   0,
                       0,  -1,   0,   1,   0,   0),
                    byrow = TRUE, nrow = 11, ncol = 6)
      if(!bidirectional) {
        gbnds <- c(0, -1.2*resp_max, # tp bounds
                   logc_min - 1, -(logc_max), # ga bounds
                   0.3, -8, # gw bounds
                   logc_min - 1, -(logc_max + 2), # la bounds
                   0.3, -18, # lw bounds
                   0.25) # ga < la
      }
      else {
        val <- 1.2*max(abs(resp_min),abs(resp_max))
        gbnds <- c(-val,-val, # tp bounds
                  logc_min - 1, -(logc_max + 0.5), # ga bounds
                   0.3, -8, # gw bounds
                   logc_min - 1, -(logc_max + 2), # la bounds
                   0.3, -18, # lw bounds
                   0.25) # ga < la
      }
#cat("g:",gbnds[1],":",g[1],":",gbnds[2],"\n")
#cat("g:",gbnds[3],":",g[2],":",gbnds[4],"\n")
#cat("g:",gbnds[5],":",g[3],":",gbnds[6],"\n")
# if (mmed_conc > logc_min) g[7] <- mmed_conc - 0.25

      gCi <- matrix(gbnds, nrow = 11, ncol = 1)

      ## Optimize the gnls model
      gfit <- try(constrOptim(g,
                              tcplObjGnls,
                              ui = gUi,
                              ci = gCi,
                              mu = 1e-6,
                              method = "Nelder-Mead",
                              control = list(fnscale = -1,
                                             reltol = 1e-10,
                                             maxit = 6000),
                              lconc = logc,
                              resp = resp),
                  silent = !verbose)
      if(verbose) cat("gnls >>>",gfit$counts[1],gfit$convergence,"\n")

      ## Generate some summary statistics
      if (!is(gfit, "try-error")) { # Gain-loss fit the data

        gnls <- 1L
        gaic <- 12 - 2*gfit$value # 2*length(gfit$par) - 2*gfit$value
        #print(gfit)
        mapply(assign,
               c(gprs),
               gfit$par,
               MoreArgs = list(envir = fenv))

        ## Calculate rmse for gnls
        gnls_gn <- (1/(1 + 10^((gnls_ga - logc)*gnls_gw)))
        gnls_ls <- (1/(1 + 10^((logc - gnls_la)*gnls_lw)))
        gnls_modl <- gnls_tp * gnls_gn * gnls_ls
        grme <- sqrt(mean((gnls_modl - resp)^2, na.rm = TRUE))

        ## Calculate the sd for the gnls parameters
        gfit$cov <- try(solve(-hessian(tcplObjGnls,
                                       gfit$par,
                                       lconc = logc,
                                       resp = resp)),
                        silent = !verbose)

        if (!is(gfit$cov, "try-error")) { # Could invert gnls Hessian

          gcov <- 1L
          gdiag_sqrt <- suppressWarnings(sqrt(diag(gfit$cov)))
          if (any(is.nan(gdiag_sqrt))) {
            mapply(assign,
                   gsds,
                   NaN,
                   MoreArgs = list(envir = fenv))
          } else {
            mapply(assign,
                   gsds,
                   gdiag_sqrt,
                   MoreArgs = list(envir = fenv))
          }

        } else { # Could not invert gnls Hessian

          gcov <- 0L
          mapply(assign,
                 c(gsds),
                 NA_real_,
                 MoreArgs = list(envir = fenv))

        }

      } else { # Gain-loss did not fit the data

        gnls <- 0L
        gaic <- NA_real_
        gcov <- NA_integer_
        grme <- NA_real_

        mapply(assign,
               c(gprs, gsds),
               NA_real_,
               MoreArgs = list(envir = fenv))

      }

    } else { # None of the response values fell outside 3*bmad

      hill <- NA_integer_
      haic <- NA_real_
      hcov <- NA_integer_
      hrme <- NA_real_
      gnls <- NA_integer_
      gaic <- NA_real_
      gcov <- NA_integer_
      grme <- NA_real_

      mapply(assign,
             c(hprs, hsds, gprs, gsds),
             NA_real_,
             MoreArgs = list(envir = fenv))

    }

  } else { # Data has response data for less than four concentrations.

    cnst <- NA_integer_
    cnst_er <- NA_real_
    caic <- NA_real_
    crme <- NA_real_

    hill <- NA_integer_
    haic <- NA_real_
    hcov <- NA_integer_
    hrme <- NA_real_
    gnls <- NA_integer_
    gaic <- NA_real_
    gcov <- NA_integer_
    grme <- NA_real_

    mapply(assign,
           c(hprs, hsds, gprs, gsds),
           NA_real_,
           MoreArgs = list(envir = fenv))

  }
  if(verbose) cat(" aic values: ",caic,haic,gaic,"\n")
  out <- list(resp_max      = resp_max,
              resp_min      = resp_min,
              max_mean      = max(rmns),
              max_mean_conc = as.numeric(names(which.max(rmns))),
              max_med       = mmed,
              max_med_conc  = mmed_conc,
              logc_max      = logc_max,
              logc_min      = logc_min,
              cnst          = cnst,
              hill          = hill,
              hcov          = hcov,
              gnls          = gnls,
              gcov          = gcov,
              cnst_er       = cnst_er,
              cnst_aic      = caic,
              cnst_rmse     = crme,
              hill_tp       = hill_tp,
              hill_tp_sd    = hill_tp_sd,
              hill_ga       = hill_ga,
              hill_ga_sd    = hill_ga_sd,
              hill_gw       = hill_gw,
              hill_gw_sd    = hill_gw_sd,
              hill_er       = hill_er,
              hill_er_sd    = hill_er_sd,
              hill_aic      = haic,
              hill_rmse     = hrme,
              gnls_tp       = gnls_tp,
              gnls_tp_sd    = gnls_tp_sd,
              gnls_ga       = gnls_ga,
              gnls_ga_sd    = gnls_ga_sd,
              gnls_gw       = gnls_gw,
              gnls_gw_sd    = gnls_gw_sd,
              gnls_la       = gnls_la,
              gnls_la_sd    = gnls_la_sd,
              gnls_lw       = gnls_lw,
              gnls_lw_sd    = gnls_lw_sd,
              gnls_er       = gnls_er,
              gnls_er_sd    = gnls_er_sd,
              gnls_aic      = gaic,
              gnls_rmse     = grme,
              nconc         = ncnc,
              npts          = npts,
              nrep          = nrep,
              nmed_gtbl     = nmed_gtbl,
              logc          = logc,
              hill_modl     = hill_modl,
              gnls_modl     = gnls_modl,
              ...)

  out

}

#-------------------------------------------------------------------------------
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
#-------------------------------------------------------------------------------
# tcplObjCnst: Generate a constant model objective function to optimize
#-------------------------------------------------------------------------------

#' @rdname Models
#'
#' @section Constant Model (cnst):
#' \code{tcplObjCnst} calculates the likelyhood for a constant model at 0. The
#' only parameter passed to \code{tcplObjCnst} by \code{p} is the scale term
#' \eqn{\sigma}. The constant model value \eqn{\mu_{i}}{\mu[i]} for the
#' \eqn{i^{th}}{ith} observation is given by:
#' \deqn{\mu_{i} = 0}{\mu[i] = 0}
#'
#' @importFrom stats dt
#' @export

tcplObjCnst <- function(p, resp) {

  ### This function takes creates an objective function to be optimized using
  ### the starting constant model parameter, and response.
  ###
  ### Arguments:
  ###   p:     a numeric vector of length 1 containg the starting values for
  ###          the constant model, in order: log error term
  ###   lresp: a numeric vector containing the response values to produce the
  ###          objective function
  ###
  ### Value:
  ###   An objective function for the constant model and the given resp data

  mu <- 0
  sum(dt((resp - mu)/exp(p[1]), df = 4, log = TRUE) - p[1])

}

#-------------------------------------------------------------------------------
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
  #points(p[2],abs(p[1]))
  gn <- 1/(1 + 10^((p[2] - lconc)*p[3]))
  ls <- 1/(1 + 10^((lconc - p[4])*p[5]))
  mu <- p[1]*gn*ls
  sum(dt((resp - mu)/exp(p[6]), df = 4, log = TRUE) - p[6])

}

#-------------------------------------------------------------------------------
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

