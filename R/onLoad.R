.onLoad <- function(libname, pkgname) {
  
  conf_file <- file.path(system.file(package = "tcpl"), "TCPL.config")
  
  if (file.exists(conf_file)) {
    
    tcplConfLoad()
    
  } else {
    
    tcplConfReset()
    tcplConfLoad()
    
  }
  
}
