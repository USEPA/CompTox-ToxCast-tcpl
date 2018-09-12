#-------------------------------------------------------------------------------
# tcplConfExample: Generate default example location
#-------------------------------------------------------------------------------

#' @rdname config_funcs
#' @export

tcplConfExample <- function () {
  
  TCPLlite <- file.path(system.file(package = "tcpl"), "example")
  tcplConf(db = TCPLlite, user = NA, host = NA, drvr = "tcplLite")
  
}

#-------------------------------------------------------------------------------
