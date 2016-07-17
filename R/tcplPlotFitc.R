#-------------------------------------------------------------------------------
# tcplPlotFitc: Plot the fit category tree
#-------------------------------------------------------------------------------

#' @title Plot the fit category tree
#' 
#' @description
#' \code{tcplPlotFitc} makes a plot showing the level 5 fit categories.
#' 
#' @param fitc Integer, the fit categories
#' @param main Character of length 1, the title (optional)
#' @param fitc_sub, Integer, a subset of fit categories to plot
#' 
#' @note
#' Suggested device size (inches): width = 10, height = 7.5, pointsize = 9
#' 
#' @import data.table
#' @importFrom grDevices col2rgb colorRampPalette rgb
#' @importFrom stats quantile
#' @importFrom graphics par plot lines text legend 
#' @export

tcplPlotFitc <- function(fitc = NULL, main = NULL, fitc_sub = NULL) {
  
  ## Variable-binding to pass R CMD Check
  r <- g <- N <- edge <- plt <- leaf <- parent_fitc <- xloc <- yloc <- NULL
  name <- J <- NULL
  
  if (!is.null(fitc)) {
    
    vals <- data.table(fitc = fitc)[ , .N, by = fitc]
    
    mypal <- rev(c("#A50026", "#FF7F00", "#FFFF33", "#33A02C",
                   "#1F78B4", "#762A83"))
    
    clrs <- data.table(edge = 1:8,
                       r = col2rgb(colorRampPalette(mypal)(8))["red", ]/255,
                       g = col2rgb(colorRampPalette(mypal)(8))["green", ]/255,
                       b = col2rgb(colorRampPalette(mypal)(8))["blue", ]/255)
    clrs[ , col := rgb(r, g, b, 0.4)]
    
    vmax <- unname(quantile(vals[ , N], 0.9))
    vmin <- min(vals[ , N])
    
    b <- unlist(lapply(0:7, function(x) { vmax/((vmax/vmin)^(1/8))^x }))
    b <- round(rev(b[1:7]), 0)
    
    vals[N <= b[1],            col := clrs[edge == 1, col]]
    vals[N <= b[2] & N > b[1], col := clrs[edge == 2, col]]
    vals[N <= b[3] & N > b[2], col := clrs[edge == 3, col]]
    vals[N <= b[4] & N > b[3], col := clrs[edge == 4, col]]
    vals[N <= b[5] & N > b[4], col := clrs[edge == 5, col]]
    vals[N <= b[6] & N > b[5], col := clrs[edge == 6, col]]
    vals[N <= b[7] & N > b[6], col := clrs[edge == 7, col]]
    vals[N >  b[7],            col := clrs[edge == 8, col]]
        
    vals[ , plt := TRUE]
    if(!is.null(fitc_sub)) vals[!fitc %in% fitc_sub, plt := FALSE]
    
  }
    
  tree <- tcplQuery("SELECT * FROM mc5_fit_categories;", getOption("TCPL_DB"))
  tree[ , leaf := !fitc %in% parent_fitc]
  tree[ , plt := TRUE]
  if(!is.null(fitc_sub)) tree[!fitc %in% fitc_sub, plt := FALSE]
  tree[ , xloc := xloc - xloc[1]]
  tree[ , yloc := yloc - yloc[1]]
  
  # pdf("test.pdf", width = 10, height = 7.5, pointsize = 9)
  
  opar <- par()[c("pty", "mar", "family")]
  on.exit(par(opar))
  fmar <- c(0, 0, ifelse(is.null(main), 0, 4), 0)
  par(pty = "m", mar = fmar)
  
  p <- list(type = "n",
            bty = "n",
            xlim = c(-1150, 1150),
            ylim = c(-800, 800),
            ylab = "",
            xlab = "",
            xaxt = "n",
            yaxt = "n",
            main = main
  )
  
  do.call(plot, c(tree$yloc ~ tree$xloc, p), quote = TRUE)
  
  setkey(tree, fitc)
  for (i in tree[fitc != 1 & plt, fitc]) {
    lines(x = c(tree[J(i), xloc], tree[tree[J(i), parent_fitc], xloc]),
          y = c(tree[J(i), yloc], tree[tree[J(i), parent_fitc], yloc]))
  }
  
  with(tree[which(plt)],
       rect(xleft = xloc - 120, 
            xright = xloc + 120, 
            ybottom = yloc - 15, 
            ytop = yloc + 15,
            border = NA, 
            col = "white"))
  
  if (!is.null(fitc)) {
    .drawCircles(x = tree[J(vals[which(plt), fitc]), xloc], 
                 y = tree[J(vals[which(plt), fitc]), yloc], 
                 r = vals[which(plt), (log(N, 2) + 2)*15], 
                 border = NA, 
                 col = vals[which(plt), col])
  }
  
  if (lw(tree[ , plt & !leaf]) > 0) {
    with(tree[plt & !leaf],
         text(x = xloc, y = yloc, labels = name, cex = 0.45))
  }
  
  if (lw(tree[ , plt & leaf]) > 0) {
    text(x = tree[plt & leaf, xloc], 
         y = tree[plt & leaf, yloc], 
         labels = tree[plt & leaf, name], 
         cex = 0.45,
         font = ifelse(is.null(fitc), 2, 1), 
         col = ifelse(is.null(fitc), "darkgreen", "black"))
  }
  
  if (!is.null(fitc)) {
    legend(x = "bottom",
           ncol = 8, 
           bty = "n",
           box.lwd = 0,
           bg = "white",
           pch = 19,
           cex = 0.8,
           col = clrs[ , col],
           legend = c(paste0("1-", b[1]), 
                      paste0(b[1] + 1, "-", b[2]), 
                      paste0(b[2] + 1, "-", b[3]), 
                      paste0(b[3] + 1, "-", b[4]), 
                      paste0(b[4] + 1, "-", b[5]), 
                      paste0(b[5] + 1, "-", b[6]), 
                      paste0(b[6] + 1, "-", b[7]), 
                      paste0(b[7] + 1, "+")))
  }
    
}
