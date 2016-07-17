#-------------------------------------------------------------------------------
# tcplConfLoad: Load the current configuration file
#-------------------------------------------------------------------------------

#' @rdname config_funcs
#' @export

tcplConfLoad <- function () {
  
  ## Variable-binding to pass R CMD Check
  DRVR <- USER <- PASS <- HOST <- DB <- NULL
  
  source(file.path(system.file(package = "tcpl"), "TCPL.config"), local = TRUE)
  
  tcplConf(DRVR, USER, PASS, HOST, DB)
  
}

#-------------------------------------------------------------------------------
