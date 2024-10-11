#-------------------------------------------------------------------------------
# tcplConfDefault: Generate default config file
#-------------------------------------------------------------------------------

#' @rdname config_funcs
#' @export

tcplConfDefault <- function () {
  
  tcpl_key <- "01cbaf22-904f-11ee-954e-325096b39f47"
  tcplConf(db = NA, user = NA, pass = tcpl_key, host = NULL, drvr = "API")
  
}

#-------------------------------------------------------------------------------
