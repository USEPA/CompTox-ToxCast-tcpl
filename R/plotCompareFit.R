#-----------------------------------------------------------------------------------------
# plotCompareFit: Create plot of the dose-response with associated models across two aeids
#-----------------------------------------------------------------------------------------

#' @importFrom graphics par layout plot rect abline curve axis axTicks points
#' @importFrom graphics plot.window text arrows
#' @importFrom caTools trapz

.plotCompareFit <- function(all.resp, all.logc, aeids, all.pars,
                            scale.by = 'coff', sym=c(1, 4), col=c('tomato3', 'dodgerblue2'), desc_col=c('red', 'blue'),
                            cnst=NULL, orig.aeid=NULL) {
  
  ###--------------------------- Draw Left Panel ----------------------------###
  
  # Left panel ----
  layout(mat = matrix(1:2, nrow = 1), widths = c(4, 5.5), heights = 3.5)
  on.exit(layout(1))
  
  opar <- par()[c("pty", "mar", "family")]
  on.exit(par(opar), add = TRUE)
  par(pty = "s",
      mar = c(4, 4.5, 2, 3) + 0.1,
      family = "mono")
  
  # Set scaled parameters
  
  param.to.scale = c('bmad', 'coff', 'hill_tp', 'gnls_tp', 'resp_max', 'resp_min')
  if (!is.null(cnst)) {
    param.to.scale = c(param.to.scale, 'pos')
    all.pars[['pos']] = cnst
  }
  all.pars[['auc']] = c(NA_real_,NA_real_)
  #all.pars[['coff']] = c(100,100)
  for (scl in param.to.scale) {
    orig.var <- all.pars[[scl]] # params to be scaled
    orig.scale <- all.pars[[scale.by]] # param to scale by
    all.pars[[paste('scl',scl,sep='_')]] = orig.var/orig.scale
  }
  
  
  #aeid <- unique(aeids)
  aeid <- orig.aeid
  len.aeid1 <- length(aeids[aeids==aeid[1]])
  len.aeid2 <- length(aeids[aeids==aeid[2]])
  scale <- c(rep(all.pars$coff[1], len.aeid1), rep(all.pars$coff[2], len.aeid2))
  resp.scale = all.resp/scale
  
  # Set the dimensions of the plot based on both AEIDs
  ylab <- "Scaled Endpoint"
  
  if(any(all.pars$scl_bmad != 0)){
    y0 <- c(min(signif(-10*all.pars$scl_bmad,2)), max(signif(20*all.pars$scl_bmad,2)))
    #y0 <- c(min(signif(-10*all.pars$scl_bmad,2)), max(signif(20*all.pars$scl_bmad,2)))
  }else{
    if ("scl_coff" %in% names(all.pars)) {
      y0 <- c(min(signif(-5*all.pars$scl_coff,2)), max(signif(10*all.pars$scl_coff,2)))
    }else{
      y0 <- c(-0.1, 2)
    }
  }
  
  if ("scl_pos" %in% names(all.pars)) {
    fmax <- suppressWarnings(with(all.pars, 1.05*max(scl_hill_tp, scl_gnls_tp, scl_pos, na.rm = TRUE)))
  } else {
    fmax <- suppressWarnings(with(all.pars, 1.05*max(scl_hill_tp, scl_gnls_tp, na.rm = TRUE)))
  }
  if (is.infinite(fmax)) fmax <- NA_real_
  view <- fmax/diff(range(resp.scale))
  hbrk <- max(all.pars$scl_resp_max) > y0[2]
  lbrk <- min(all.pars$scl_resp_min) < y0[1]
  brk <- with(all.pars, view < 0.5 & (hbrk | lbrk))
  pad <- if (hbrk & lbrk) 0.1 else 0.2
  
  if (!is.na(brk) & brk) {
    yrng <- (fmax - y0[1])/(1 - hbrk*pad - lbrk*pad)
    ylim <- c(y0[1] - pad*yrng*lbrk, fmax + pad*yrng*hbrk)
    md <- resp.scale < fmax & resp.scale > ylim[1]
    if (all(md)) {
      brk <- FALSE
    } else {
      hi <- resp.scale > fmax
      if (!any(hi)) hbrk <- FALSE
      lo <- resp.scale < ylim[1]
      if (!any(lo)) lbrk <- FALSE
    }
    if (!any(lbrk, hbrk)) brk <- FALSE
  } else {
    if (is.na(brk)) {
      brk <- if(hbrk | lbrk) TRUE else FALSE    
      if (brk) {
        yrng <- diff(y0)/(1 - hbrk*pad - lbrk*pad)
        ylim <- c(y0[1] - pad*yrng*lbrk, y0[2] + pad*yrng*hbrk)
        md <- resp.scale < ylim[2] & resp.scale > ylim[1]
        if (all(md)) {
          brk <- FALSE
        } else {
          hi <- resp.scale > ylim[2]
          if (!any(hi)) hbrk <- FALSE
          lo <- resp.scale < ylim[1]
          if (!any(lo)) lbrk <- FALSE
        }
      } else {
        ylim <- y0
        md <- rep(TRUE, length(resp.scale))
      }
    } else {
      if ("scl_pos" %in% names(all.pars)) { 
        ylim <- with(all.pars, c(min(y0[1], 1.2*scl_resp_min), max(y0[2], 1.2*scl_resp_max, 1.2*scl_pos)))
      } else {
        ylim <- with(all.pars, c(min(y0[1], 1.2*scl_resp_min), max(y0[2], 1.2*scl_resp_max)))
      } 
     md <- rep(TRUE, length(resp.scale))
    }
  }
  
  p <- list(ylim = ylim,
            xlim = range(all.logc),
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
  
  

  #do.call(what = plot, args = c(resp[md] ~ logc[md], p), quote = TRUE)
  do.call(what = plot, args = c(resp.scale[md] ~ all.logc[md], p), quote = TRUE)
  
  for (ii in seq(length(aeid))) {
    resp <- resp.scale[aeids==aeid[ii]]
    logc <- all.logc[aeids==aeid[ii]]
    md.scl <- md[aeids==aeid[ii]]
    pars <- lapply(all.pars, `[[`, ii)
    pars[['scale.by']] <- pars[[scale.by]]
    
    # rect(xleft = par()$usr[1],
    #      xright = par()$usr[2],
    #      ybottom = min(-3 * all.pars$scl_bmad),
    #      ytop = max(3 * all.pars$scl_bmad),
    #      border = NA,
    #      col = "gray70",
    #      density = 15,
    #      angle = 45)
    
    ## Round all numeric values in 'pars' to 99 digits
    nind <- which(sapply(pars, is.numeric))
    pars[nind] <- lapply(pars[nind], round, digits = 99)
    
    
    if ("scl_coff" %in% names(pars)) abline(h = pars$scl_coff, lwd = 1.5, col = "gray70")
    if ("scl_pos" %in% names(pars)) abline(h = pars$scl_pos, lwd = 1.5, col = col[ii], lty=2)
    
    if (is.null(pars$modl)) pars$modl <- "none"
    if (is.na(pars$modl)) pars$modl <- "none"
    
    if (!is.na(pars$cnst) & pars$cnst) {
      
      abline(h = 0,
             lwd = 4,
             col = col[ii],
             lty = ifelse(pars$modl == "cnst", "solid", "blank"))
      
    }
    
    if (!is.na(pars$hill) & pars$hill) {
      
      hill.eq <- function(x) with(pars, (hill_tp/scale.by)/(1 + 10^((hill_ga - x)*hill_gw)))
      curve(hill.eq, 
            from = pars$logc_min, 
            to = pars$logc_max,
            add = T, 
            n = 1e4, 
            lwd = 4, 
            col = col[ii],
            lty = ifelse(pars$modl == "hill", "solid", "blank"))  
      abline(v = pars$hill_ga,
             lwd = 2.5,
             lty = ifelse(pars$modl == "hill", "solid", "blank"),
             col = col[ii])
      
      tmp.x <- seq(pars$logc_min,pars$logc_max,length=1000)
      #all.pars[['auc']][ii] = trapz(tmp.x, hill.eq(tmp.x))
      
    }
    
    if (!is.na(pars$gnls) & pars$gnls) {
      
      gnls.eq <- function(x) {
        with(pars, {
          h1 <- (1/(1 + 10^((gnls_ga - x)*gnls_gw)))
          h2 <- (1/(1 + 10^((x - gnls_la)*gnls_lw)))
          gnls_tp*h1*h2/scale.by
        })
      } 
      curve(gnls.eq, 
            from = pars$logc_min, 
            to = pars$logc_max,
            add = T, 
            n = 1e4, 
            lwd = 4,
            col = col[ii],
            lty = ifelse(pars$modl == "gnls", "solid", "blank"))  
      abline(v = pars$gnls_ga,
             lwd = 2.5,
             lty = ifelse(pars$modl == "gnls", "solid", "blank"),
             col = col[ii])
      
      tmp.x <- seq(pars$logc_min,pars$logc_max,length=1000)
      #all.pars[['auc']][ii] = trapz(tmp.x, gnls.eq(tmp.x))
      
    }
    
    if (is.null(pars$toxboot)) pars$toxboot <- NA
    #error bars plotting
    if (!is.na(pars$toxboot) & pars$toxboot) {
      y = pars$modl_tp/2 #50% resp
      if(!is.na(pars$modl_ga_min) & !is.na(pars$modl_ga_max))
        suppressWarnings(arrows(pars$modl_ga_min, y, pars$modl_ga_max, y, code=3, angle=90, length=0.1, lwd = 2))
      
    }
    
    
      
    # points(x = pars$emax_conc,
    #        y = pars$emax,
    #        pch = 22,
    #        cex = 2,
    #        col = "gray35",
    #        lwd = 1,
    #        bg = "yellow2")
    
    points(resp[md.scl] ~ logc[md.scl], pch=sym[ii], cex = 1.5, lwd = 2.5, col = col[ii])
  }
  
  if (brk) {

    if (hbrk) {

      hrng <- unique(range(resp.scale[hi]))
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
      lrng <- unique(range(resp.scale[lo]))
      if (length(lrng) != 1) {
        llim <- with(pars, c(resp_min, resp_min + diff(lrng)/pad))
      } else {
        llim <- with(pars, c(resp_min, resp_min + (y0[1] - lrng)/pad))
      }

      par(new = TRUE)
      plot.window(xlim = par()$usr[1:2], ylim = llim)
      points(resp.scale[lo] ~ all.logc[lo], cex = 0.5, lwd = 2.5, col = "gray60")

      axis(side = 4,
           at = lrng,
           labels = signif(lrng, 2),
           font = 1,
           lwd = 2,
           cex.axis = 0.5,
           col = "gray60")

    } 

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
  
  

  
# Right Panel ----  
  ###--------------------- Prepare Text for Right Panel ---------------------###
  
  spaces <- function(x) paste(rep(" ", x), collapse = "")

  itxt <- with(all.pars, {
    paste0("ASSAY:   ", paste(aenm, collapse='VS.\n         '), "\n\n",
           "NAME:    ", unique(chnm), "\n",
           "CHID:    ", unique(chid), spaces(8 - ifelse(is.na(unique(chid)), 2, nchar(unique(chid)))),
           "CASRN: ", unique(casn), "\n",
           "SPID(S): ", paste(spid, collapse=', '), "  ", ifelse(brk, "BRK", ""), "\n",
           "M4ID(s):    ", paste(m4id, collapse=', '), "  ", ifelse(brk, "BRK", ""), "\n\n"
    )
  })
  
  
  final.aeid.txt <- c()
  for (ii in seq(length(aeid))) {
    #resp <- resp.scale[aeids==aeid[ii]]
    #logc <- all.logc[aeids==aeid[ii]]
    #md.scl <- md[aeids==aeid[ii]]
    pars <- lapply(all.pars, `[[`, ii)
  
    if (!is.na(pars$hill) & pars$hill) {
  
      if (pars$hcov) {
        hsds <- with(pars, signif(c(hill_tp_sd, hill_ga_sd, hill_gw_sd), 3))
        hsds[is.na(hsds)] <- "NaN"
      } else {
        hsds <- rep("NA", 3)
      }
  
      hprs <- with(pars, signif(c(hill_tp, hill_ga, hill_gw), 3))

      #lbl <- with(pars, paste0(aenm, ' (', desc_col[ii], '): ',"HILL MODEL:\n"))
      lbl <- with(pars, paste(strwrap(paste0(aenm, ' (', desc_col[ii], '): ',"HILL MODEL:\n"), width=45), collapse = '\n'))
      
      htxt1 <- paste(lbl, "tp",
                     " ga",
                     " gw\n",
                     sep = spaces(6))
  
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
  
      
      #lbl <- with(pars, paste0(aenm, ' (', desc_col[ii], '): ',"GAIN-LOSS MODEL:\n"))
      lbl <- with(pars, paste(strwrap(paste0(aenm, ' (', desc_col[ii], '): ',"GAIN-LOSS MODEL:\n"), width=45), collapse = '\n'))
      
      
      gtxt1 <- paste(lbl, 
                     "tp",
                     " ga",
                     " gw",
                     " la",
                     " lw\n",
                     sep = spaces(6))
  
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
  
    
  
    # ntxt <- paste0("MAX_MEAN: ", pars$max_mean,
    #                spaces(10 - nchar(pars$max_mean)),
    #                "MAX_MED: ", pars$max_med,
    #                spaces(10 - nchar(pars$max_med)),
    #                "BMAD: ", signif(pars$bmad, 3),
    #                "\n\n")
    # 
    # if (!is.null(pars$hitc)) {
    # 
    #   pars$coff <- signif(pars$coff, 3)
    #   ctxt <- paste0("COFF: ", pars$coff, spaces(7 - nchar(pars$coff)),
    #                  "HIT-CALL: ", pars$hitc, spaces(5 - nchar(pars$hitc)),
    #                  "FITC: ", pars$fitc, spaces(5 - nchar(pars$fitc)),
    #                  "ACTP: ", round(pars$actp, 2),
    #                  "\n\n")
    # 
    # } else {
    # 
    #   ctxt <- NULL
    # 
    # }
    # 
    # if (!is.null(pars$flgo)) {
    # 
    #   ftxt <- paste0("FLAGS: ", ifelse(is.na(pars$flgo), "", pars$flgo),"\n\n")
    # 
    # } else {
    # 
    #   ftxt <- NULL
    # 
    # }
  
    # Not Doing toxboot for now
    # if (!is.na(pars$toxboot)) {
    # 
    #   btxt <- paste0("HIT-PCT: ", pars$hit_pct, spaces(2),
    #                  "MED-GA: ", round(pars$modl_ga_med,4), spaces(2),
    #                  "GA-CI: ", round(pars$modl_ga_delta,4), spaces(2),
    #                  "\n\n")
    # 
    # } else {
    # 
    #   btxt <- NULL
    # 
    # }
    
    # Select correct text based on winning model and store in vector
    with(pars, paste(strwrap(paste0(aenm, ' (', desc_col[ii], '): ',"HILL MODEL:\n"), width=45), collapse = '\n'))
    if (is.na(pars$modl)) {
      #aeid_txt = with(pars, paste0(aenm, ' (', desc_col[ii], '): No Model Fit'))
      aeid_txt = with(pars, paste(strwrap(paste0(aenm, ' (', desc_col[ii], '): No Model Fit'), width=45), collapse = '\n'))
      
    } else if (pars$modl == 'cnst') {
      #aeid_txt = with(pars, paste0(aenm, ' (', desc_col[ii], '): Constant Model'))
      aeid_txt = with(pars, paste(strwrap(paste0(aenm, ' (', desc_col[ii], '): Constant Model'), width=45), collapse = '\n'))
    } else if (pars$modl == 'hill') {
      aeid_txt = htxt
    } else if (pars$modl == 'gnls') {
      aeid_txt = gtxt
    } else {
      aeid_txt <- ""
    }
  
  final.aeid.txt <- c(final.aeid.txt, aeid_txt)
  } # End for loop for AEID text
  
  mtxt <- paste0(final.aeid.txt, collapse = '\n\n') # Combine aeid text
  
  
  # Summary table for comparison
  #aics <- cvals(with(pars, round(c(cnst_aic, hill_aic, gnls_aic), 2)))
  #prob <- cvals(with(pars, round(c(cnst_prob, hill_prob, gnls_prob), 2)))
  #rmse <- cvals(with(pars, round(c(cnst_rmse, hill_rmse, gnls_rmse), 2)))
  accs <- cvals(with(all.pars, round(c(10^modl_acc), 2)))
  ac50s <- cvals(with(all.pars, round(c(10^modl_ga), 2)))
  tps <- cvals(with(all.pars, round(c(modl_tp), 2)))
  #aucs <- cvals(with(all.pars, round(c(auc), 2)))
  hitcs <- cvals(with(all.pars, c(hitc), 2))
  
  models <- c(paste0('AEID',all.pars$aeid[1]), paste0('AEID',all.pars$aeid[2]))
  
  
  atxt <- paste0("\n\n",
                 spaces(6),
                 models[1],
                 spaces(10),
                 models[2],
                 #spaces(8),
                 #models[3],
                 "\n",
                 paste0("AC50: ",
                        ac50s[1],
                        spaces(15 - nchar(ac50s[1])),
                        ac50s[2]),
                 "\n",
                 paste0("mEFF: ",
                        tps[1],
                        spaces(15 - nchar(tps[1])),
                        tps[2]),
                 "\n",
                # paste0("AUC:  ",
                #        aucs[1],
                #        spaces(15 - nchar(aucs[1])),
                #        aucs[2]),
                # "\n",
                 paste0("ACC:  ",
                        accs[1],
                        spaces(15 - nchar(accs[1])),
                        accs[2]),
                 "\n",
                 paste0("HITC: ",
                        hitcs[1],
                        spaces(15 - nchar(hitcs[1])),
                        hitcs[2]),
                 "\n\n")
  
  pars$max_mean <- signif(pars$max_mean, 3)
  pars$max_med  <- signif(pars$max_med,  3)
  
  #plot_txt1 <- paste0(itxt, htxt, gtxt, atxt, ntxt, ctxt, ftxt, btxt)
  plot_txt1 <- paste0(itxt, mtxt, atxt)

  # if (pars$modl != "none") {
  #   nlines <- sum(7,
  #                 length(gregexpr("\n", htxt)[[1]]),
  #                 length(gregexpr("\n", gtxt)[[1]]))
  #   winner <- with(pars, which(c("cnst", "hill", "gnls") == pars$modl))
  #   if (length(winner) > 1) winner <- winner[1]
  # 
  #   plot_txt2 <- paste0(paste(rep("\n", nlines), collapse = ""),
  #                       spaces(6 + 12*(winner - 1)),
  #                       models[winner],
  #                       "\n",
  #                       spaces(6 + 12*(winner - 1)),
  #                       aics[winner],
  #                       "\n",
  #                       spaces(6 + 12*(winner - 1)),
  #                       prob[winner],
  #                       "\n",
  #                       spaces(6 + 12*(winner - 1)),
  #                       rmse[winner])
  # 
  # } else {
  # 
  #   plot_txt2 <- NULL
  # 
  # }


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

  # suppressWarnings(
  #   text(y = 15,
  #        x = 1,
  #        labels = plot_txt2,
  #        adj = c(0, 1),
  #        font = 2,
  #        cex = 1,
  #        col = "red")
  # )
  
}

#-------------------------------------------------------------------------------
