#-------------------------------------------------------------------------------
# tcplPlotlyPlot: Create a plotly plot for tcpl data
#-------------------------------------------------------------------------------

#' @title Create a plotly plot for tcpl data
#'
#' @description
#' \code{tcplPlotlyPlot} generates a plotly plot for the given level and data type.
#'
#' @param dat data.table containing plot-prepared data
#' @param lvl Integer, the level of data to plot (2 for 'sc' or 5 for 'mc')
#'
#' @return A plotly plot
#'
#' @import plotly
#' @export
#'
#' @examples
#' \dontrun{
#' tcplPlotlyPlot(dat, lvl = 5)
#' }
tcplPlotlyPlot <- function(dat, lvl = 5) {
  # variable binding
  conc <- resp <- NULL

  # create plotly plot
  p <- plot_ly(data = dat, x = ~conc, y = ~resp, type = 'scatter', mode = 'lines+markers')

  # add layout
  p <- p %>% layout(
    title = paste("Level", lvl, "Plot"),
    xaxis = list(title = "Concentration"),
    yaxis = list(title = "Response")
  )

  return(p)
}
