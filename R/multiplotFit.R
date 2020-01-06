#-------------------------------------------------------------------------------
# multiPlotfit: Create plot of the dose-response with associated models
#-------------------------------------------------------------------------------

#' @importFrom graphics par layout plot rect abline curve axis axTicks points
#' @importFrom graphics plot.window text arrows

multiPlotfit <- function(resp, logc, pars) {
  
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
            cex.main = .85,
            xlab = expression(bold(paste("Concentration (",mu,"M)"))),
            ylab = ylab,
            main = paste(pars$chnm,
                         paste0("CASRN: ", pars$casn),
                         pars$aenm,
                         paste0("M4ID: ",pars$m4id), 
                         sep = "\n"),
            bty = "n",
            xaxt = "n",
            yaxt = "n",
            type = "n")
  
  do.call(what = plot, args = c(resp[md] ~ logc[md], p), quote = TRUE)
  
  if (is.null(pars$modl)) pars$modl <- "none"
  if (is.na(pars$modl)) pars$modl <- "none"
  
  if(pars$modl == "hill") AC50 <- signif(10^pars$hill_ga,4) 
  if(pars$modl == "gnls") AC50 <- signif(10^pars$gnls_ga,4) 
  if(pars$modl == "cnst") AC50 <- NA
  if(pars$modl == "none") AC50 <- NA
  
  legend("topleft", bty="n", text.font=2,
         legend = paste(paste0("AC50: ",AC50), 
                        paste0("hitc: ", pars$hitc),
                        sep = "\n")
  )
  
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
  
  if (is.null(pars$toxboot)) pars$toxboot <- NA
  #error bars plotting
  if (!is.na(pars$toxboot) & pars$toxboot) {
    y = pars$modl_tp/2 #50% resp
    if(!is.na(pars$modl_ga_min) & !is.na(pars$modl_ga_max))
      suppressWarnings(arrows(pars$modl_ga_min, y, pars$modl_ga_max, y, code=3, angle=90, length=0.1, lwd = 2))
    
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
}