#-------------------------------------------------------------------------------
# tcplggplotCompare: Create a ggplot for comparing tcpl data
#-------------------------------------------------------------------------------

#' @title Create a ggplot for comparing tcpl data
#'
#' @description
#' \code{tcplggplotCompare} generates a ggplot for comparing the given level and data type.
#'
#' @param dat1 data.table containing plot-prepared data for the first dataset
#' @param dat2 data.table containing plot-prepared data for the second dataset
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
#' tcplggplotCompare(dat1, dat2, verbose = TRUE, lvl = 5, flags = TRUE, yrange = c(NA, NA))
#' }
tcplggplotCompare <- function(dat1, dat2, verbose = FALSE, lvl = 5, flags = FALSE, yrange = c(NA, NA)) {
  # variable binding
  conc <- resp <- NULL

  # create ggplot
  p <- ggplot() +
    geom_line(data = dat1, aes(x = conc, y = resp), color = "blue") +
    geom_point(data = dat1, aes(x = conc, y = resp), color = "blue") +
    geom_line(data = dat2, aes(x = conc, y = resp), color = "red") +
    geom_point(data = dat2, aes(x = conc, y = resp), color = "red") +
    labs(title = paste("Level", lvl, "Comparison Plot"), x = "Concentration", y = "Response")

  # add flags if required
  if (flags) {
    p <- p + geom_text(data = dat1, aes(label = flag), vjust = -1, color = "blue") +
      geom_text(data = dat2, aes(label = flag), vjust = -1, color = "red")
  }

  # set y-axis range if provided
  if (!all(is.na(yrange))) {
    p <- p + ylim(yrange)
  }

  # add verbose table if required
  if (verbose) {
    p <- p + theme(plot.margin = unit(c(1, 1, 4, 1), "lines")) +
      annotation_custom(
        grob = tableGrob(dat1[, .(Parameter = names(dat1), Value = unlist(dat1))]),
        xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = -Inf
      ) +
      annotation_custom(
        grob = tableGrob(dat2[, .(Parameter = names(dat2), Value = unlist(dat2))]),
        xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = -Inf
      )
  }

  return(p)
}
