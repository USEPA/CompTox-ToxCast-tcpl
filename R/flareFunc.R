#-------------------------------------------------------------------------------
# flareFunc: Calculate the weighted mean of a square to detect plate flares
#-------------------------------------------------------------------------------

#' @title Calculate the weighted mean of a square to detect plate flares
#' 
#' @description
#' \code{flareFunc} calculates the weighted mean of square regions to detect
#' plate flares.
#' 
#' @param val Numeric, the well values
#' @param coli Integer, the well column index
#' @param rowi Integer, the well row index
#' @param apid Character, the assay plate id
#' @param r Integer, the number of wells from the center well (in one 
#' direction) to make the square
#' 
#' @seealso \code{\link{MC6_Methods}}, \code{\link{Method functions}}, 
#' \code{\link{mc6}}
#' 
#' @import data.table
#' @importFrom stats dist

flareFunc <- function(val, coli, rowi, apid, r){
  
  ## Variable-binding to pass R CMD Check
  cold <- rowd <- flrv <- index <- NULL
  
  if (r > 4) r <- 4
  nrep <- (1 + 2*r)^2
  minc <- min(coli)
  maxc <- max(coli)
  minr <- min(rowi)
  maxr <- max(rowi)
  ordr <- order(apid, coli, rowi)
  val  <- val[ordr]
  val[val > 100] <- 100
  coli <- coli[ordr]
  rowi <- rowi[ordr]  
  apid <- apid[ordr]
  val_len <- length(val)
  adj     <- -r:r
  adj     <- c(0, adj[-which(adj == 0)])  
  adj_len <- length(adj)
  adjc <- rep(adj, each  =  adj_len)
  adjr <- rep(adj, times =  adj_len)
  adjd <- 1/as.matrix(dist(cbind(adjc, adjr)))[ , 1] 
  adjd[1] <- 1
  dat <- data.table(val   = rep(val, nrep),
                    coli  = rep(coli, nrep),
                    rowi  = rep(rowi, nrep), 
                    apid  = rep(apid, nrep),
                    ordr  = rep(ordr, nrep),
                    index = rep(1:nrep, each = val_len),
                    cold  = rep(adjc, each = val_len),
                    rowd  = rep(adjr, each = val_len),
                    adjd = rep(adjd, each = val_len))
  
  dat[, coli := coli + cold]
  dat[, rowi := rowi + rowd]
  dat[, flrv := sum(val*adjd, na.rm = TRUE)/nrep - 
        max(val*adjd, na.rm = TRUE)/nrep,
      by = list(apid, coli, rowi)]
  dat[index == 1, flrv][order(ordr)][]            
  
}

#-------------------------------------------------------------------------------
