#-------------------------------------------------------------------------------
# plotFit: Create plot of the dose-response with associated models
#-------------------------------------------------------------------------------

#' @importFrom graphics par layout plot rect abline curve axis axTicks points
#' @importFrom graphics plot.window text

.plotFit <- function(resp, logc, pars) {
  
  ###--------------------------- Draw Left Panel ----------------------------###
  
  layout(mat = matrix(1:2, nrow = 1), widths = c(4, 5.5), heights = 3.5)
  on.exit(layout(1))
  
  opar <- par()[c("pty", "mar", "family")]
  on.exit(par(opar), add = TRUE)
  par(pty = "s",
      mar = c(4, 4.5, 2, 3) + 0.1,
      family = "mono")
  
  ## Round all numeric values in 'pars' to 99 digits
  nind <- which(sapply(pars, is.numeric))
  pars[nind] <- lapply(pars[nind], round, digits = 99)
  
  ylab <- NULL
  if (pars$resp_unit == "percent_activity") {
    #y0 <- c(-50, 150)
    ylab <- "Percent Activity"
  }
  if (pars$resp_unit == "log2_fold_induction") {
    #y0 <- c(-1, 4)
    ylab <- "Log2(Fold Induction)"
  }
  if (pars$resp_unit == "log10_fold_induction") {
    #y0 <- c(-0.1, 2)
    ylab <- "Log10(Fold Induction)"
  }
  if (is.null(ylab)) {
    ylab <- pars$resp_unit
    #y0 <- c(-50, 150)
  }
  if(pars$bmad != 0){
    y0 <- c(signif(-10*pars$bmad,2), signif(20*pars$bmad,2))
  }else{
    if ("coff" %in% names(pars)) {
      y0 <- c(signif(-5*pars$coff,2), signif(10*pars$coff,2))
    }else{
      y0 <- c(-0.1, 2)
    }
  }
  
  fmax <- suppressWarnings(with(pars, 1.05*max(hill_tp, gnls_tp, na.rm = TRUE)))
  if (is.infinite(fmax)) fmax <- NA_real_
  view <- fmax/diff(range(resp))
  hbrk <- pars$resp_max > y0[2]
  lbrk <- pars$resp_min < y0[1]
  brk <- with(pars, view < 0.5 & (hbrk | lbrk))
  pad <- if (hbrk & lbrk) 0.1 else 0.2
  
  if (!is.na(brk) & brk) {
    yrng <- (fmax - y0[1])/(1 - hbrk*pad - lbrk*pad)
    ylim <- c(y0[1] - pad*yrng*lbrk, fmax + pad*yrng*hbrk)
    md <- resp < fmax & resp > ylim[1]
    if (all(md)) {
      brk <- FALSE
    } else {
      hi <- resp > fmax
      if (!any(hi)) hbrk <- FALSE
      lo <- resp < ylim[1]
      if (!any(lo)) lbrk <- FALSE
    }
    if (!any(lbrk, hbrk)) brk <- FALSE
  } else {
    if (is.na(brk)) {
      brk <- if(hbrk | lbrk) TRUE else FALSE      
      if (brk) {
        yrng <- diff(y0)/(1 - hbrk*pad - lbrk*pad)
        ylim <- c(y0[1] - pad*yrng*lbrk, y0[2] + pad*yrng*hbrk)
        md <- resp < ylim[2] & resp > ylim[1]
        if (all(md)) {
          brk <- FALSE
        } else {
          hi <- resp > ylim[2]
          if (!any(hi)) hbrk <- FALSE
          lo <- resp < ylim[1]
          if (!any(lo)) lbrk <- FALSE
        }
      } else {
        ylim <- y0
        md <- rep(TRUE, length(resp))
      }
    } else {
      ylim <- with(pars, c(min(y0[1], 1.2*resp_min), max(y0[2], 1.2*resp_max)))
      md <- rep(TRUE, length(resp))
    }
  }
  
  p <- list(ylim = ylim,
            xlim = range(logc),
            cex.lab = 1.2,
            cex.axis = 1.2,
            font.lab = 2,
            col = "black",
            cex = 2,
            xlab = expression(bold(paste("Concentration (",mu,"M)"))),
            ylab = ylab,
            main = "",
            bty = "n",
            xaxt = "n",
            yaxt = "n",
            type = "n")
  
  do.call(what = plot, args = c(resp[md] ~ logc[md], p), quote = TRUE)
  
  rect(xleft = par()$usr[1],
       xright = par()$usr[2], 
       ybottom = -3 * pars$bmad, 
       ytop = 3 * pars$bmad,
       border = NA, 
       col = "gray70",
       density = 15, 
       angle = 45)
  
  if ("coff" %in% names(pars)) abline(h = pars$coff, lwd = 1.5, col = "gray70")
  
  if (is.null(pars$modl)) pars$modl <- "none"
  if (is.na(pars$modl)) pars$modl <- "none"
  
  if (!is.na(pars$cnst) & pars$cnst) {
    
    abline(h = 0,
           lwd = 4,
           col = "darkorange",
           lty = ifelse(pars$modl == "cnst", "solid", "dashed"))
    
  }
  
  if (!is.na(pars$hill) & pars$hill) {
    
    hill.eq <- function(x) with(pars, hill_tp/(1 + 10^((hill_ga - x)*hill_gw)))
    curve(hill.eq, 
          from = pars$logc_min, 
          to = pars$logc_max,
          add = T, 
          n = 1e4, 
          lwd = 4, 
          col = "tomato3",
          lty = ifelse(pars$modl == "hill", "solid", "dashed"))  
    abline(v = pars$hill_ga,
           lwd = 2.5,
           lty = ifelse(pars$modl == "hill", "solid", "dashed"),
           col = "tomato3")
    
  }
  
  if (!is.na(pars$gnls) & pars$gnls) {
    
    gnls.eq <- function(x) {
      with(pars, {
        h1 <- (1/(1 + 10^((gnls_ga - x)*gnls_gw)))
        h2 <- (1/(1 + 10^((x - gnls_la)*gnls_lw)))
        gnls_tp*h1*h2
      })
    } 
    curve(gnls.eq, 
          from = pars$logc_min, 
          to = pars$logc_max,
          add = T, 
          n = 1e4, 
          lwd = 4,
          col = "dodgerblue2",
          lty = ifelse(pars$modl == "gnls", "solid", "dashed"))  
    abline(v = pars$gnls_ga,
           lwd = 2.5,
           lty = ifelse(pars$modl == "gnls", "solid", "dashed"),
           col = "dodgerblue2")
    
  }
  
  axis(side = 1, 
       at = axTicks(side = 1),
       labels = signif(10^axTicks(side = 1), digits = 1),
       font = 1, 
       lwd = 2, 
       cex.axis = 1.2, 
       col = "gray35")
  axis(side = 2, 
       at = axTicks(side = 2),
       labels = axTicks(side = 2),
       font = 1, 
       lwd = 2, 
       cex.axis = 1.2, 
       col = "gray35")
    
  # points(x = pars$emax_conc,
  #        y = pars$emax,
  #        pch = 22,
  #        cex = 2,
  #        col = "gray35",
  #        lwd = 1,
  #        bg = "yellow2")
  
  points(resp[md] ~ logc[md], cex = 1.5, lwd = 2.5, col = "gray30")
  
  if (brk) {
    
    if (hbrk) {
      
      hrng <- unique(range(resp[hi]))
      if (length(hrng) != 1) {
        hlim <- with(pars, c(resp_max - diff(hrng)/pad, resp_max))
      } else {
        hlim <- with(pars, c(resp_max - (hrng - y0[2])/pad, resp_max))
      }
      
      par(new = TRUE)
      plot.window(xlim = par()$usr[1:2], ylim = hlim)
      points(resp[hi] ~ logc[hi], cex = 0.5, lwd = 2.5, col = "gray60")
      
      axis(side = 4, 
           at = hrng,
           labels = signif(hrng, 2),
           font = 1, 
           lwd = 2, 
           cex.axis = 0.5, 
           col = "gray60")
      
    }
    
    if (lbrk) {
      
      lrng <- unique(range(resp[lo]))
      if (length(lrng) != 1) {
        llim <- with(pars, c(resp_min, resp_min + diff(lrng)/pad))
      } else {
        llim <- with(pars, c(resp_min, resp_min + (y0[1] - lrng)/pad))
      }
      
      par(new = TRUE)
      plot.window(xlim = par()$usr[1:2], ylim = llim)
      points(resp[lo] ~ logc[lo], cex = 0.5, lwd = 2.5, col = "gray60")
      
      axis(side = 4, 
           at = lrng,
           labels = signif(lrng, 2),
           font = 1, 
           lwd = 2, 
           cex.axis = 0.5, 
           col = "gray60")
      
    }
        
  }
  
  ###--------------------- Prepare Text for Right Panel ---------------------###
  
  spaces <- function(x) paste(rep(" ", x), collapse = "")
  
  itxt <- with(pars, {
    paste0("ASSAY:   ", aenm, "\n\n",
           "NAME:    ", chnm, "\n",
           "CHID:    ", chid, spaces(8 - ifelse(is.na(chid), 2, nchar(chid))),
           "CASRN: ", casn, "\n",
           "SPID(S): ", spid, "\n",
           "M4ID:    ", m4id, "  ", ifelse(brk, "BRK", ""), "\n\n"
    )
  })
  
  if (!is.na(pars$hill) & pars$hill) {
    
    if (pars$hcov) {
      hsds <- with(pars, signif(c(hill_tp_sd, hill_ga_sd, hill_gw_sd), 3))
      hsds[is.na(hsds)] <- "NaN"
    } else {
      hsds <- rep("NA", 3)
    }
    
    hprs <- with(pars, signif(c(hill_tp, hill_ga, hill_gw), 3))
    
    htxt1 <- paste("HILL MODEL (in red):\n      tp", 
                   "ga",
                   "gw\n",
                   sep = spaces(7))
    
    htxt2 <- paste0(c("val:  ", "sd:   "),
                    c(paste(sapply(hprs, 
                                   function(x) {
                                     paste0(x, spaces(9 - nchar(x)))
                                   }),
                            collapse = ""),
                      paste(sapply(hsds, 
                                   function(x) {
                                     paste0(x, spaces(9 - nchar(x)))
                                   }),
                            collapse = "")),
                    collapse = "\n")
    
    htxt <- paste0(htxt1, htxt2, "\n\n")
    
  } else {
    
    if (is.na(pars$hill)) {
      htxt <- "HILL MODEL: Not applicable.\n\n"
    } else {
      htxt <- "HILL MODEL: Failed to converge.\n\n"
    } 
    
  }
  
  if (!is.na(pars$gnls) & pars$gnls) {
    
    if (pars$gcov) {
      gsds <- with(pars, 
                   signif(c(gnls_tp_sd, 
                            gnls_ga_sd, 
                            gnls_gw_sd, 
                            gnls_la_sd, 
                            gnls_lw_sd),
                          3)
      )
      gsds[is.na(gsds)] <- "NaN"
    } else {
      gsds <- rep("NA", 5)
    }
    
    gprs <- with(pars, 
                 signif(c(gnls_tp, gnls_ga, gnls_gw, gnls_la, gnls_lw), 3))
    
    gtxt1 <- paste("GAIN-LOSS MODEL (in blue):\n      tp",
                   "ga", 
                   "gw",
                   "la",
                   "lw\n",
                   sep = spaces(7))
    
    gtxt2 <- paste0(c("val:  ", "sd:   "),
                    c(paste(sapply(gprs, 
                                   function(x) {
                                     paste0(x, spaces(9 - nchar(x)))
                                   }),
                            collapse = ""),
                      paste(sapply(gsds, 
                                   function(x) {
                                     paste0(x, spaces(9 - nchar(x)))
                                   }),
                            collapse = "")),
                    collapse = "\n")
    
    gtxt <- paste0(gtxt1, gtxt2, "\n\n")
    
  } else {
    
    if (is.na(pars$hill)) {
      gtxt <- "GAIN-LOSS MODEL: Not applicable.\n\n"
    } else {
      gtxt <- "GAIN-LOSS MODEL: Failed to converge.\n\n"
    } 
    
  }
  
  cvals <- function(x) {
    x <- as.character(x)
    x[is.na(x)] <- "NA"
    x
  }
  
  aics <- cvals(with(pars, round(c(cnst_aic, hill_aic, gnls_aic), 2)))
  prob <- cvals(with(pars, round(c(cnst_prob, hill_prob, gnls_prob), 2)))
  rmse <- cvals(with(pars, round(c(cnst_rmse, hill_rmse, gnls_rmse), 2)))
  models <- c("CNST", "HILL", "GNLS")
  
  atxt <- paste0(spaces(6), 
                 models[1], 
                 spaces(8), 
                 models[2], 
                 spaces(8), 
                 models[3],
                 "\n",
                 paste0("AIC:  ", 
                        aics[1], 
                        spaces(12 - nchar(aics[1])),
                        aics[2],
                        spaces(12 - nchar(aics[2])),
                        aics[3]),
                 "\n",
                 paste0("PROB: ", 
                        prob[1], 
                        spaces(12 - nchar(prob[1])),
                        prob[2],
                        spaces(12 - nchar(prob[2])),
                        prob[3]),
                 "\n",
                 paste0("RMSE: ", 
                        rmse[1], 
                        spaces(12 - nchar(rmse[1])),
                        rmse[2],
                        spaces(12 - nchar(rmse[2])),
                        rmse[3]),
                 "\n\n")
  
  pars$max_mean <- signif(pars$max_mean, 3)
  pars$max_med  <- signif(pars$max_med,  3)
  
  ntxt <- paste0("MAX_MEAN: ", pars$max_mean,
                 spaces(10 - nchar(pars$max_mean)),
                 "MAX_MED: ", pars$max_med, 
                 spaces(10 - nchar(pars$max_med)),
                 "BMAD: ", signif(pars$bmad, 3),
                 "\n\n")
  
  if (!is.null(pars$hitc)) {
    
    pars$coff <- signif(pars$coff, 3)
    ctxt <- paste0("COFF: ", pars$coff, spaces(7 - nchar(pars$coff)),
                   "HIT-CALL: ", pars$hitc, spaces(5 - nchar(pars$hitc)), 
                   "FITC: ", pars$fitc, spaces(5 - nchar(pars$fitc)),
                   "ACTP: ", round(pars$actp, 2),
                   "\n\n")
    
  } else {
    
    ctxt <- NULL
    
  }
  
  if (!is.null(pars$flgo)) {
    
    ftxt <- paste0("FLAGS:\n", ifelse(is.na(pars$flgo), "", pars$flgo))
    
  } else {
    
    ftxt <- NULL
    
  }
  
  plot_txt1 <- paste0(itxt, htxt, gtxt, atxt, ntxt, ctxt, ftxt)
  
  if (pars$modl != "none") {
    nlines <- sum(7,
                  length(gregexpr("\n", htxt)[[1]]),
                  length(gregexpr("\n", gtxt)[[1]]))
    winner <- with(pars, which(c("cnst", "hill", "gnls") == pars$modl))
    if (length(winner) > 1) winner <- winner[1]
    
    plot_txt2 <- paste0(paste(rep("\n", nlines), collapse = ""),
                        spaces(6 + 12*(winner - 1)),
                        models[winner],
                        "\n",
                        spaces(6 + 12*(winner - 1)),
                        aics[winner],
                        "\n",
                        spaces(6 + 12*(winner - 1)),
                        prob[winner],
                        "\n",
                        spaces(6 + 12*(winner - 1)),
                        rmse[winner])
    
  } else {
    
    plot_txt2 <- NULL
    
  }
  
  
  ###--------------------------- Draw Right Panel ---------------------------###
  
  par(pty = "m", 
      family = "mono",
      mar = rep(2,4) + 0.1)
  
  plot(0, 
       type = "n", 
       bty = "n", 
       xaxt = "n", 
       yaxt = "n", 
       ylab = "", 
       xlab = "", 
       xlim = c(0, 16), 
       ylim = c(0, 16))
  
  suppressWarnings(
    text(y = 15, 
         x = 1,
         labels = plot_txt1, 
         adj = c(0, 1),
         font = 2,
         cex = 1)
  )
  
  suppressWarnings(
    text(y = 15, 
         x = 1,
         labels = plot_txt2, 
         adj = c(0, 1),
         font = 2,
         cex = 1,
         col = "red")
  )
  
}

#-------------------------------------------------------------------------------
