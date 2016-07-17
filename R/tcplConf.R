#-------------------------------------------------------------------------------
# tcplConf: Configure the tcpl options
#-------------------------------------------------------------------------------

#' @rdname config_funcs
#' @export

tcplConf <- function (drvr = NULL, user = NULL, pass = NULL, host = NULL, 
                      db = NULL) {
  
  check <- function(x) length(x) == 1 && is.character(x)
  setop <- function(x) {
    xn <- deparse(substitute(x))
    if (is.na(x)) x <- NA_character_
    if (check(x)) {
      p <- list(x)
      names(p) <- paste0("TCPL_", toupper(xn))
      do.call(what = options, p)
    } else {
      warning("Invalid '", xn, ",' no changes made to TCPL_", toupper(xn))
    }
  }
  
  if (!is.null(user)) setop(user)
  if (!is.null(pass)) setop(pass)
  if (!is.null(host)) setop(host)
  if (!is.null(db))   setop(db)
  
  if (!is.null(drvr)) {
    
    if (!drvr %in% c("SQLite", "MySQL")) {
      stop(drvr, " is not a supported database driver. Must be 'SQLite' or ",
           "'MySQL.'")
    }
    
    if (drvr == "SQLite") {
      options("TCPL_DRVR" = "SQLite")
    }
    
    if (drvr == "MySQL") {
      options("TCPL_DRVR" = "MySQL")
      mxp <- tcplQuery("SHOW VARIABLES LIKE 'max_allowed_packet'")$Value
      mxp <- as.numeric(mxp)
      if (mxp < 1073741824) {
        warning("The 'max_allowed_packet' MySQL server setting is set to ", 
                mxp, " bytes. It is recommended that you increase it to ",
                "1073741824 bytes to ensure larger queries run without error.")
      }
    }
    
  }
  
}

#-------------------------------------------------------------------------------
