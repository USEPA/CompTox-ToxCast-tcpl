#-------------------------------------------------------------------------------
# tcplConfDefault: Generate default config file
#-------------------------------------------------------------------------------

#' @rdname config_funcs
#' @export

tcplConfDefault <- function () {
  
  TCPLlite <- file.path(system.file(package = "tcpl"), "csv")
  tcplConf(db = TCPLlite, user = NA, host = NA, drvr = "tcplLite")
  
}

#-------------------------------------------------------------------------------
