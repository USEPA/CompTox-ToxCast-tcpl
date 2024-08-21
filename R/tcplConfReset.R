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
      "DRVR <- \"API\"",
      "HOST <- NA_character_",
      "USER <- NA_character_",
      "PASS <- \"01cbaf22-904f-11ee-954e-325096b39f47\"",
      "DB   <- NA_character_",
      "",
      "###################################################################",
      sep = "\n",
      file = file.path(conf_file),
      append = FALSE)
  
}

#-------------------------------------------------------------------------------
