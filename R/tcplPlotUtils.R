#' tcplPlotLoadWllt
#' Replaces NA dtxsid and chnm with a string description of the sample's well type(s)
#'
#' @param dat dataset
#' @param type mc or sc
#'
#' @return dat with updated dtxsid/chnm if they are NA
#' @import data.table
#' @importFrom dplyr group_by summarize select left_join case_when 
tcplPlotLoadWllt <- function(dat = NULL, type = "mc") {
  
  # variable binding for R CMD check
  dsstox_substance_id <- chnm <- aeid <- acid <- wllt <- wllt_desc <- NULL
  
  # determine missing chemical info
  missing <- dat[is.na(dsstox_substance_id) | is.na(chnm), .(spid, aeid)]
  if (nrow(missing) > 0) {
    acid_map <- tcplLoadAcid(fld = "aeid", val = missing$aeid)[,.(aeid,acid)]
    missing <- missing[acid_map, on="aeid"]
    l0_dat <- tcplLoadData(type = type, lvl = 0, fld = list("spid","acid"), 
                           list(missing$spid, missing$acid))
    
    # replace with string describing the well type(s)
    wllts <- l0_dat |> select(acid, spid, wllt) |> group_by(acid, spid) |> 
      summarize(wllt_desc = dplyr::case_when(
        all(unique(wllt) %in% c('c', 'p')) ~ "Gain-of-signal control",
        all(unique(wllt) %in% c('m', 'o')) ~ "Loss-of-signal control",
        all(unique(wllt) == 'n') ~ "Neutral/negative control",
        all(unique(wllt) == 'b') ~ "Blank",
        all(unique(wllt) == 'v') ~ "Viability control",
        all(!is.na(unique(wllt))) && all(!is.null(unique(wllt))) ~ paste0("Well type: ", paste(unique(wllt), collapse = ", ")),
        .default = NA
      ), .groups="drop") |> left_join(acid_map, by = "acid") |> as.data.table()
    no_wllt <- wllts[is.na(wllt_desc), spid]
    if (length(no_wllt)) warning(paste0("wllt for SPID(s): ", paste(no_wllt, collapse = ", "), " missing. Leaving dsstox_substance_id and chnm as NA."))
    dat <- left_join(dat, wllts, by = c("aeid", "spid"))
    dat[is.na(dsstox_substance_id) | is.na(chnm), c("dsstox_substance_id", "chnm") := wllt_desc]
  }
  dat
  
}


#' tcplPlotSetYRange
#'
#' @param dat dataset
#' @param yuniform should the yrange be uniform
#' @param yrange length 2 of the yrange
#' @param type mc or sc
#'
#' @return yrange of the data
tcplPlotSetYRange <- function(dat, yuniform, yrange, type) {
  # variable binding
  model_type <- NULL
  # validate yrange
  if (length(yrange) != 2) {
    stop("'yrange' must be of length 2")
  }

  # set range
  if (yuniform == TRUE && identical(yrange, c(NA, NA))) {
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
    yrange <- c(min, max)
  }

  yrange
}

#' tcplPlotCalcAspectRatio
#'
#' @param type string of mc or sc indicating if it is single or multi conc
#' @param verbose should the plot return a table with parameters
#' @param multi Boolean, by default TRUE for "pdf" if the number of plots exceeds
#' one. Prints variable number of plots per page depending on 'verbose' and 'type' 
#' settings.
#' @param nrows Integer, number of rows each compare plot uses
#' @param output How should the plot be presented. To work with the plot in 
#' environment, use "ggplot"; to interact with the plot in application, use 
#' "console"; or to save as a file type, use "pdf", "jpg", "png", "svg", or "tiff".
#' @param group.threshold Integer of length 1, minimum number of samples in a 
#' given plot where comparison plots, instead of coloring models by sample, should
#' delineate curve color by a given group.fld. By default 9.
#' @param nrow Integer, number of rows in multiplot. By default 2.
#' @param ncol Integer, number of columns in multiplot. By default 3, 2 if verbose, 
#' 1 for compare plots.
#' @param flags Boolean, by default FALSE. If TRUE, level 6 flags are displayed
#' within output.
#'
#' @return a list of validated parameters for plotting
tcplPlotCalcAspectRatio <- function(type = "mc", verbose = FALSE, multi = FALSE, nrows = 0, 
                                    output = c("ggplot", "console", "pdf", "png", "jpg", "svg", "tiff"), 
                                    group.threshold = 9, nrow = NULL, ncol = NULL, flags = FALSE) {
  
  # assign nrow = ncol = 1 for output="pdf" and multi=FALSE to plot one plot per page
  if(output == "pdf" && multi == FALSE)
    nrow <- ncol <- 1
  w <- 7
  h <- 5
  if (verbose && any(nrows > 1 & nrows < group.threshold)) 
    w <- 12
  if (type == "sc" && any(!verbose, all(nrows == 1 | nrows >= group.threshold))) 
    w <- 5
  if (type == "sc")
    h <- 6
  if (verbose && any(nrows > 1 & nrows < group.threshold)) 
    h <- max(2 + max(nrows[nrows > 1 & nrows < group.threshold]), 6)
  if (any(nrows >= group.threshold) && flags)
    h <- max(h, 8)
  if(is.null(nrow))
    nrow <- max(round(10/h), 1)
  if(is.null(ncol))
    ncol <- max(round(14/w), 1)
  list(w=w,h=h,nrow=nrow,ncol=ncol)
}

#' tcplPlotValidate
#'
#' @param dat data.table containing plot-prepared data
#' @param type string of mc or sc indicating if it is single or multi conc
#' @param compare Character vector, the field(s) to join samples on to create comparison
#' plots
#' @param by Parameter to divide files into e.g. "aeid".
#' @param flags bool - should we return flags
#' @param output how should the plot be formatted
#' @param multi are there multiple plots
#' @param verbose should the plot return a table with parameters
#'
#' @return a list of validated parameters for plotting
tcplPlotValidate <- function(dat = NULL, type = "mc", compare = "m4id", by = NULL, 
                             flags = NULL, output = c("ggplot", "console", "pdf", "png", "jpg", "svg", "tiff"), 
                             multi = NULL, verbose = FALSE) {
  # set lvl based on type
  lvl <- 5
  if (type == "sc") {
    lvl <- 2
    if (flags == TRUE) {
      warning("'flags' was set to TRUE - no flags exist for plotting single concentration")
      flags <- FALSE
    }
    if (compare == "m4id") compare <- "s2id"
  }
  
  if (!is.null(by) && length(by) > 1) stop("'by' must be of length 1.")
  if (length(output) > 1) output <- output[1] else if (length(output) == 0) stop("'output' cannot be NULL")
  
  if (!is.null(dat) && !is.data.table(dat)) {
    if (!is.list(dat) || !is.data.table(dat[[1]])) {
      stop("'dat' must be a data.table or a list of data.tables.")
    }
    if (!compare %in% c("m4id", "s2id")) {
      warning("'dat' provided as list of list of data tables, meaning compare plots are already subdivided. 'compare' field will be ignored and have no effect.")
    }
    if (!is.null(by)) {
      warning("Using 'by' can have unintended consequences when 'dat' is provided as a list of data.tables. Instead, consider adding a custom field to group comparison plots, and specify using the 'compare' parameter. Then, use 'by' to split plots into files.")
    }
  }
  
  # forced assign multi=FALSE for output = c("console","png","jpg","svg","tiff"), verbose=FALSE for output="console"
  if (output != "pdf") {
    multi <- FALSE
    if (output == "console") verbose <- FALSE
  }

  list(lvl = lvl, compare = compare, flags = flags, output = output, multi = multi, verbose = verbose)
}


#' tcplLegacyPlot
#'
#' @return a ggplot based on old plotting methodology
tcplLegacyPlot <- function() {
  stop("Legacy plotting is currently unsupported.")
  # VARIABLE BINDING
  fld <- val <- lvl <- multi <- fileprefix <- NULL
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
