#-------------------------------------------------------------------------------
# tcplConf: Configure the tcpl options
#-------------------------------------------------------------------------------

#' @rdname config_funcs
#' @export

tcplConf <- function (drvr = NULL, user = NULL, pass = NULL, host = NULL, 
                      db = NULL,...) {
  #tcplConf(user='_dataminer', pass='pass', host='au.epa.gov', drvr = 'MySQL',db = 'invitrodb')
  
  # Notes for tcplLite
  # ==================
  # Allow drvr='tcplLite' for writing flat files for each level of analysis
  # db=<local dir for writing files>
  
  # Notes for example
  # ==================
  # Allow drvr='example' for loading example data from data directory in tcpl package
  
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
  
  
  additional_pars <- list(...)
  if(length(additional_pars)>0){
  names(additional_pars) <- paste0("TCPL_", toupper(names(additional_pars)))
  do.call(what = options, additional_pars)
  }
  
  if (!is.null(drvr)) {
    
    if (!drvr %in% c( "MySQL", "tcplLite", "example", "API")) {
      stop(drvr, " is not a supported database driver. Must be ",
           "'MySQL', 'tcplLite', 'API' or 'example'.")
    }
    
    if (drvr == "example"){
      options("TCPL_DRVR" = "example")
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
    
    if (drvr == "tcplLite") {
      tcplLiteInit()
      options("TCPL_DRVR" = "tcplLite")
    } 
    
    if (drvr == "API") {
      options("TCPL_DRVR" = "API")
      if (is.null(pass)) stop("'API' driver requires an API-key, supply it to 
                              the 'pass' parameter. To request a key, send an
                              email to ccte_api@epa.gov.")
      if (is.null(host)) options("TCPL_HOST" = "https://api-ccte.epa.gov/bioactivity")
      register_ccdr(key = pass)
    }
    
  }
  
}

#-------------------------------------------------------------------------------
