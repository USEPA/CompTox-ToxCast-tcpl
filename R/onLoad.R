.onLoad <- function(libname, pkgname) {
  
  conf_file <- Sys.getenv("TCPL_CONF")
  
  if (file.exists(conf_file)) {
    
    loadConf <- try(tcplConfLoad(list.new = FALSE))
    
    if (is(loadConf, 'try-error')) {
      
      warning("The configuration file given by 'TCPL_CONF' could not be ",
              "loaded. Using default settings. Please see ?tcplConf for ",
              "more information.")
      
      tcplConfDefault()
    
    }
    
  } else {
    
    tcplConfDefault()
    
  }
  
}
