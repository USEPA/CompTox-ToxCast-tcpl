tcplPlotSetYRange <- function(dat,yuniform,yrange,type){
  #validate yrange
  if (length(yrange) != 2) {
    stop("'yrange' must be of length 2")
  }
  
  # set range
  if (yuniform == TRUE && identical(yrange, c(NA,NA))) {
    min <- min(dat$resp_min, unlist(dat$resp))
    max <- max(dat$resp_max, unlist(dat$resp))
    if (type == "mc") {
      # any bidirectional models contained in dat, cutoff both ways
      if (2 %in% dat$model_type) {
        cutoffs <- dat[model_type == 2]$coff
        min <- min(min, cutoffs, cutoffs * -1)
        max <- max(max, cutoffs, cutoffs * -1)
      }
      # any gain models contained in dat, cutoff only positive
      if (3 %in% dat$model_type) {
        cutoffs <- dat[model_type == 3]$coff
        min <- min(min, cutoffs)
        max <- max(max, cutoffs)
      }
      # any loss models contained in dat, cutoff only negative
      if (4 %in% dat$model_type) {
        cutoffs <- dat[model_type == 4]$coff
        min <- min(min, cutoffs * -1)
        max <- max(max, cutoffs * -1)
      }
    } else {
      min <- min(min, dat$coff, dat$coff * -1)
      max <- max(max, dat$coff, dat$coff * -1)
    }
    yrange = c(min, max)
  }
  
  yrange
}
  
  
  tcplPlotValidate <- function(type = "mc",flags = NULL,output = "none",multi = NULL,verbose = FALSE){
    
    # set lvl based on type
    lvl <- 5
    if (type == "sc") {
      lvl <- 2
      if (flags == TRUE) {
        warning("'flags' was set to TRUE - no flags exist for plotting single concentration")
        flags = FALSE
      }
    }
    
    # default assign multi=TRUE for output="pdf" 
    if (output == "pdf" && is.null(multi)) {
      multi <- TRUE
    }
    # forced assign multi=FALSE for output = c("console","png","jpg","svg","tiff"), verbose=FALSE for output="console"
    if (output !="pdf") {
      multi <- FALSE
      if(output =="console"){
        verbose <- FALSE
      }
    }
    
    list(lvl = lvl,type = type,flags = flags,output = output,multi = multi,verbose = verbose)

  }
  
  
  tcplLegacyPlot <- function(){
    if (length(output) > 1) output <- output[1]
    
    prs <- list(type = "mc", fld = fld, val = val)
    
    if (lvl == 4L) dat <- do.call(tcplLoadData, args = c(lvl = 4L, prs))
    if (lvl >= 5L) dat <- do.call(tcplLoadData, args = c(lvl = 5L, prs))
    if (lvl >= 6L) {
      flg <- do.call(tcplLoadData, args = c(lvl = 6L, prs))
    } else {
      flg <- NULL
    }
    if (lvl == 7L) {
      boot <- do.call(tcplLoadData, args = c(lvl = 7L, prs))
    } else {
      boot <- NULL
    }
    
    if (nrow(dat) == 0) stop("No data for fld/val provided")
    
    agg <- do.call(tcplLoadData, args = c(lvl = "agg", prs))
    
    if (nrow(dat) == 1 & output == "console") {
      tcplPlotFits(dat = dat, agg = agg, flg = flg, boot = boot)
    }
    if (nrow(dat) > 1 & output == "console") stop("More than 1 concentration series returned for given field/val combination.  Set output to pdf or reduce the number of curves to 1. Current number of curves: ", nrow(dat))
    
    
    if (is.null(by)) {
      if (output == "pdf" & !multi) {
        graphics.off()
        pdf(
          file = file.path(
            getwd(),
            paste0(fileprefix, ".", output)
          ),
          height = 6,
          width = 10,
          pointsize = 10
        )
        tcplPlotFits(dat = dat, agg = agg, flg = flg, boot = boot)
        graphics.off()
      }
      # plotting if using multiplot function
      hitc.all <- TRUE
      if (multi) {
        graphics.off()
        pdf(file = file.path(getwd(), paste0(fileprefix, ".", output)), height = 10, width = 6, pointsize = 10)
        par(mfrow = c(3, 2))
        tcplMultiplot(dat = dat, agg = agg, flg = flg, boot = boot, hitc.all = hitc.all)
        graphics.off()
      }
    } else {
      if (!by %in% names(dat)) stop("grouping variable unavailable.")
      subset <- unlist(unique(dat[, by, with = FALSE]))
      for (s in subset) {
        if (output == "pdf" & !multi) {
          graphics.off()
          pdf(
            file = file.path(
              getwd(),
              paste0(fileprefix, "_", by, "_", s, ".", output)
            ),
            height = 6,
            width = 10,
            pointsize = 10
          )
          tcplPlotFits(dat = dat[get(by) == s], agg = agg, flg = flg, boot = boot)
          graphics.off()
        }
        # plotting if using multiplot function
        hitc.all <- TRUE
        if (multi) {
          graphics.off()
          pdf(file = file.path(getwd(), paste0(fileprefix, "_", by, "_", s, ".", output)), height = 10, width = 6, pointsize = 10)
          par(mfrow = c(3, 2))
          tcplMultiplot(dat = dat[get(by) == s], agg = agg, flg = flg, boot = boot, hitc.all = hitc.all)
          graphics.off()
        }
      }
    }
  }
  
