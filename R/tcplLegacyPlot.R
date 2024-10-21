#' tcplLegacyPlot
#'
#' @return a ggplot based on old plotting methodology
tcplLegacyPlot <- function() {
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
