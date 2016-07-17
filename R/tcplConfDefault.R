#-------------------------------------------------------------------------------
# tcplConfDefault: Generate default config file
#-------------------------------------------------------------------------------

#' @rdname config_funcs
#' @export

tcplConfDefault <- function () {
  
  sqlite <- file.path(system.file(package = "tcpl"), "sql", "tcpldb.sqlite")
  tcplConf(db = sqlite, user = NA, host = NA, drvr = "SQLite")
  
}

#-------------------------------------------------------------------------------
