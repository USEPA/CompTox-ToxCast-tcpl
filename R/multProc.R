#' @importFrom parallel mclapply

.multProc <- function(id, lvl, type, ncores) {
  
  proc_func <- get(paste0(type, lvl))
  
  tmp <- mclapply(id, proc_func, mc.cores = ncores, mc.preschedule = FALSE)
  cat("Writing level", lvl, "data for", lw(sapply(tmp, "[[", 1)), "ids...\n")
  stime <- Sys.time()
  dat <- rbindlist(lapply(tmp, "[[", 2))
  if (nrow(dat) == 0) {
    ttime <- round(difftime(Sys.time(), stime, units = "sec"), 2)
    ttime <- paste(unclass(ttime), units(ttime))
    cat("Writing level ", lvl, " complete. (", ttime, ")\n", sep = "")
    return(sapply(tmp, "[[", 1))
  }
  tcplWriteData(dat = dat, lvl = lvl, type = type)
  ttime <- round(difftime(Sys.time(), stime, units = "sec"), 2)
  ttime <- paste(unclass(ttime), units(ttime))
  cat("Writing level ", lvl, " complete. (", ttime, ")\n", sep = "")
  
  sapply(tmp, "[[", 1)
  
}