#-------------------------------------------------------------------------------
# tcplggplot: Create a ggplot for tcpl data
#-------------------------------------------------------------------------------

#' @title Create a ggplot for tcpl data
#'
#' @description
#' \code{tcplggplot} generates a ggplot for the given level and data type.
#'
#' @param dat data.table containing plot-prepared data
#' @param verbose Boolean, by default FALSE. If TRUE, a table with fitting parameters
#'  is included with the plot.
#' @param lvl Integer, the level of data to plot (2 for 'sc' or 5 for 'mc')
#' @param flags Boolean, by default FALSE. If TRUE, level 6 flags are displayed
#' below annotations on plot
#' @param yrange Integer of length 2, for directly setting the y-axis range, 
#' c(<min>,<max>). By default, c(NA,NA).
#'
#' @return A ggplot
#'
#' @import ggplot2
#' @export
#'
#' @examples
#' \dontrun{
#' tcplggplot(dat, verbose = TRUE, lvl = 5, flags = TRUE, yrange = c(NA, NA))
#' }
tcplggplot <- function(dat, verbose = FALSE, lvl = 5, flags = FALSE, yrange = c(NA, NA)) {
  # variable binding
  conc <- resp <- NULL

  # create ggplot
  p <- ggplot(data = dat, aes(x = conc, y = resp)) +
    geom_line() +
    geom_point() +
    labs(title = paste("Level", lvl, "Plot"), x = "Concentration", y = "Response")

  # add flags if required
  if (flags) {
    p <- p + geom_text(aes(label = flag), vjust = -1)
  }

  # set y-axis range if provided
  if (!all(is.na(yrange))) {
    p <- p + ylim(yrange)
  }

  # add verbose table if required
  if (verbose) {
    p <- p + theme(plot.margin = unit(c(1, 1, 4, 1), "lines")) +
      annotation_custom(
        grob = tableGrob(dat[, .(Parameter = names(dat), Value = unlist(dat))]),
        xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = -Inf
      )
  }

  return(p)
}
