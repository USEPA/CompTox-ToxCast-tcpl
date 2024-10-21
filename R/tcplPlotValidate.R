#' tcplPlotValidate
#'
#' @param type string of mc or sc indicating if it is single or multi conc
#' @param flags bool - should we return flags
#' @param output how should the plot be formatted
#' @param multi are there multiple plots
#' @param verbose should the plot return a table with parameters
#'
#' @return a list of validated parameters for plotting
tcplPlotValidate <- function(type = "mc", flags = NULL, output = "none", multi = NULL, verbose = FALSE) {
  # set lvl based on type
  lvl <- 5
  if (type == "sc") {
    lvl <- 2
    if (flags == TRUE) {
      warning("'flags' was set to TRUE - no flags exist for plotting single concentration")
      flags <- FALSE
    }
  }

  # default assign multi=TRUE for output="pdf"
  if (output == "pdf" && is.null(multi)) {
    multi <- TRUE
  }
  # forced assign multi=FALSE for output = c("console","png","jpg","svg","tiff"), verbose=FALSE for output="console"
  if (output != "pdf") {
    multi <- FALSE
    if (output == "console") {
      verbose <- FALSE
    }
  }

  list(lvl = lvl, type = type, flags = flags, output = output, multi = multi, verbose = verbose)
}
