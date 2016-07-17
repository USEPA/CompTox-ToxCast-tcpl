#-------------------------------------------------------------------------------
# tcplFit: Fit the data 
#-------------------------------------------------------------------------------

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
#' 
#' @details 
#' By default, \code{tcplFit} will only attempt to fit concentration series
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


tcplFit <- function(logc, resp, bmad, force.fit = FALSE, ...) {
  
  ## Variable-binding to pass R CMD Check
  hill_tp <- hill_ga <- hill_gw <- gnls_ga <- gnls_gw <- gnls_la <- NULL
  gnls_lw <- gnls_tp <- hill_tp_sd <- hill_ga_sd <- hill_gw_sd <- NULL
  hill_er <- hill_er_sd <- gnls_tp_sd <- gnls_ga_sd <- gnls_gw_sd <- NULL
  gnls_la_sd <- gnls_lw_sd <- gnls_er <- gnls_er_sd <- NULL
  
  fenv <- environment()
  
  bmad <- min(bmad)
  rmns <- tapply(resp, logc, mean)
  rmds <- tapply(resp, logc, median)
  mmed <- max(rmds)
  mmed_conc <- as.numeric(names(which.max(rmds)))
  
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
    
    if (lw(rmds >= 3*bmad) > 0 | force.fit) {
      
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
      
      hbnds <- c(0, -1.2*resp_max, # tp bounds
                 logc_min - 2, -(logc_max + 0.5), # ga bounds 
                 0.3, -8) # gw bounds
      
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
                  silent = TRUE)
      
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
                        silent = TRUE)
        
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
      
      gbnds <- c(0, -1.2*resp_max, # tp bounds
                 logc_min - 2, -(logc_max), # ga bounds
                 0.3, -8, # gw bounds
                 logc_min - 2, -(logc_max + 2), # la bounds
                 0.3, -18, # lw bounds
                 0.25) # ga < la
      
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
                  silent = TRUE)
      
      ## Generate some summary statistics
      if (!is(gfit, "try-error")) { # Gain-loss fit the data
        
        gnls <- 1L
        gaic <- 12 - 2*gfit$value # 2*length(gfit$par) - 2*gfit$value
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
                        silent = TRUE)
        
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
              ...)
  
  out
  
}

#-------------------------------------------------------------------------------
