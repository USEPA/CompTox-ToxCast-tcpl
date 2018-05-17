#-------------------------------------------------------------------------------
# tcplConfReset: Generate default config file
#-------------------------------------------------------------------------------

#' @rdname config_funcs
#' @export

tcplConfReset <- function () {
  
  conf_file <- .getConfFile()
  
  cat("###################################################################",
      "",
      "## Detailed information about this file available in the help file for",
      "## tcplConf (?tcplConf).",
      "",
      "DRVR <- \"SQLite\"",
      "HOST <- NA_character_",
      "USER <- NA_character_",
      "PASS <- NA_character_",
      "DB   <- file.path(system.file(package = \"tcpl\"),",
      "                  \"sql\",",
      "                  \"tcpldb.sqlite\")",
      "",
      "###################################################################",
      sep = "\n",
      file = file.path(conf_file),
      append = FALSE)
  
}

#-------------------------------------------------------------------------------
