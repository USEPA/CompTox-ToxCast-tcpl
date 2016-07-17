#-------------------------------------------------------------------------------
# interlaceFunc: Calculate the distance weighted mean of a square to detect 
#                interlaced (384 chemical plate) effects caused by potential
#                spillage, volatility or overly non-random sample plating
#-------------------------------------------------------------------------------

#' @title Calculate the weighted mean of a square to detect interlace effect
#' 
#' @description
#' \code{interlaceFunc} calculates the distance weighted mean of square regions
#' from a 384-well plate that is interlaced onto a 1536 well plate to detect
#' non-random signals coming from the source plate 
#' 
#' @param val Numeric, the well values
#' @param intq Numeric, interlace quadrant
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

interlaceFunc <- function(val, intq, coli, rowi, apid, r){
  
  ## Variable-binding to pass R CMD Check
  cold <- rowd <- intv <- index <- NULL
  
  if (r > 4) r <- 4
  nrep <- (1 + 2*r)^2
  minc <- min(coli)
  maxc <- max(coli)
  minr <- min(rowi)
  maxr <- max(rowi)
  ordr <- order(apid, intq, coli, rowi)
  val  <- val[ordr]
  val[val > 100] <- 100
  intq <- intq[ordr]
  coli <- coli[ordr]
  rowi <- rowi[ordr]  
  apid <- apid[ordr]
  val_len <- length(val)
  adj     <- -r:r
  adj     <- c(0,adj[-which(adj == 0)])  
  adj_len <- length(adj)
  adjc <- rep(adj, each  =  adj_len)
  adjr <- rep(adj, times =  adj_len)
  adjd <- 1/as.matrix(dist(cbind(2*adjc,adjr)))[,1] 
  adjd[1] <- 1
  dat <- data.table(val   = rep(val, nrep), 
                    coli  = rep(coli, nrep), 
                    rowi  = rep(rowi, nrep), 
                    apid  = rep(apid, nrep),
                    intq  = rep(intq, nrep),
                    ordr  = rep(ordr, nrep),
                    index = rep(1:nrep, each = val_len),
                    cold  = rep(adjc, each = val_len),
                    rowd  = rep(adjr, each = val_len),
                    adjd = rep(adjd, each = val_len))
  
  dat[, coli := coli + cold]
  dat[, rowi := rowi + rowd]
  dat[, intv := sum(val * adjd, na.rm = TRUE)/nrep -
        max(val * adjd, na.rm = TRUE)/nrep,
      by = list(apid, intq, coli, rowi)]
  dat[index == 1, intv][order(ordr)][]            
  
}

#-------------------------------------------------------------------------------
