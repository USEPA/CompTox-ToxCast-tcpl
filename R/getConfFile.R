#-------------------------------------------------------------------------------
# .getConfFile: Get the conf file locoation and check that it is writable
#-------------------------------------------------------------------------------

.getConfFile <- function() {
  
  conf_file <- Sys.getenv("TCPL_CONF")
  if (conf_file == "") {
    stop("Must add 'TCPL_CONF' environment variable to .Renviron file to ",
         "store configuration settings. See ?tcplConf for more information.")
  } else if (file.exists(conf_file)) {
    if (file.access(conf_file, mode = 2) != 0) {
      stop("The file given by 'TCPL_CONF' is not writeable. Please check the ",
           "permissions on the file or update 'TCPL_CONF' in your .Renviron ",
           "file.")
    }
  } else {
    testfile <- try(cat("", file = conf_file))
    if (is(testfile, "try-error")) {
      stop("Cannot write to the location given by 'TCPL_CONF'. ",
           "Please check the ",
           "permissions on the file or update 'TCPL_CONF' in your .Renviron ",
           "file.")
    }
  }
  
  conf_file
  
}

#-------------------------------------------------------------------------------
