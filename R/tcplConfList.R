#-------------------------------------------------------------------------------
# tcplConfList: 
#-------------------------------------------------------------------------------

#' @rdname config_funcs
#' @export

tcplConfList <- function(show.pass = FALSE) {
  
  opts <- list("TCPL_DB", "TCPL_USER", "TCPL_HOST", "TCPL_DRVR")
  if (show.pass) opts <- c(opts, "TCPL_PASS")
  do.call(options, opts)
  
}

#-------------------------------------------------------------------------------
