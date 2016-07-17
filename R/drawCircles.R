#-------------------------------------------------------------------------------
# drawCircles: draw circles on the current plot device
#-------------------------------------------------------------------------------

#' @importFrom graphics polygon

.drawCircles <- function(x, y, r, border = "black", col = NA, 
                        lwd = 1, lty = "solid", n = 100) {
  
  if (length(x) < length(y)) {
    x <- rep(x, length.out = length(y))
  }
  
  if (length(y) < length(x)) {
    y <- rep(y, length.out = length(x))
  }
  
  inc <- 2*pi/n
  angles <- angles <- seq(0, 2 * pi - inc, by = inc)
  
  if (length(col) < length(x)) {
    col <- rep(col, length.out = length(x))
  }
  
  if (length(r) < length(x)) {
    r <- rep(r, length.out = length(x))
  }
  
  if (length(lwd) < length(x)) {
    lwd <- rep(lwd, length.out = length(x))
  }
  
  if (length(lty) < length(x)) {
    lty <- rep(lty, length.out = length(x))
  }
  
  if (length(border) < length(x)) {
    border <- rep(border, length.out = length(x))
  }
  
  invisible(
    lapply(1:length(x),
           function(i) {
             xv <- cos(angles) * r[i] + x[i]
             yv <- sin(angles) * r[i] + y[i]
             polygon(xv, yv, 
                     border = border[i], 
                     col = col[i], 
                     lty = lty[i], 
                     lwd = lwd[i])
           })
  )
  
}

#-------------------------------------------------------------------------------
