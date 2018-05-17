#-------------------------------------------------------------------------------
# tcplConfSave: Save current tcpl settings to config file
#-------------------------------------------------------------------------------

#' @rdname config_funcs
#' @export

tcplConfSave <- function () {
  
  conf_file <- .getConfFile()
    
  if(any(sapply(tcplConfList(), is.null))) {
    stop("One of the tcpl settings is NULL. Saving the configuration file ",
         "with a NULL setting\nwill keep the package from loading in future ",
         "sessions.")
  }
  
  drvr <- getOption("TCPL_DRVR")
  if (!drvr %in% c("SQLite", "MySQL")) {
    stop(drvr, " is not a supported database driver. Must be 'SQLite' or ",
         "'MySQL'.")
  }
  drvr <- shQuote(drvr)
  
  host <- getOption("TCPL_HOST")
  host <- if(is.na(host)) host else shQuote(host)
  user <- getOption("TCPL_USER")
  user <- if(is.na(user)) user else shQuote(user)
  pass <- getOption("TCPL_PASS")
  pass <- if(is.na(pass)) pass else shQuote(pass)
  db   <- getOption("TCPL_DB")
  db   <- if(is.na(db))   db   else shQuote(db)
  
  cat("###################################################################\n",
      "\n",
      "## Detailed information about this file available in the help file for",
      "## tcplConf (?tcplConf).\n",
      "\n",
      "DRVR <-", drvr, "\n",
      "HOST <-", host, "\n",
      "USER <-", user, "\n",
      "PASS <-", pass, "\n",
      "DB   <-", db, "\n",
      "\n",
      "###################################################################\n",
      sep = " ",
      file = file.path(conf_file),
      append = FALSE)
  
}

#-------------------------------------------------------------------------------
