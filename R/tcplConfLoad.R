#-------------------------------------------------------------------------------
# tcplConfLoad: Load the current configuration file
#-------------------------------------------------------------------------------

#' @rdname config_funcs
#' 
#' @param list.new Logical of length 1, should the new settings be printed?
#' 
#' @export

tcplConfLoad <- function (list.new = TRUE) {
  
  stopifnot(is.logical(list.new) && length(list.new) == 1)
  
  conf_file <- .getConfFile()
  
  ## Variable-binding to pass R CMD Check
  DRVR <- USER <- PASS <- HOST <- DB <- NULL
  
  source(conf_file, local = TRUE)
  
  tcplConf(DRVR, USER, PASS, HOST, DB)
  tcplConfList()
  
}

#-------------------------------------------------------------------------------
