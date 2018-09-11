.onAttach <- function(libname, pkgname) {
  
  v <- tcplConfList()
  p <- names(v)
  pn <- sapply(p, nchar)
  sep <- sapply(pn, function(x) paste(rep(" ", 11 - x), collapse = ""))
  sep <- paste0(":", sep)
  cs <- sapply(seq_along(v), function(x) paste(p[x], v[[x]], sep = sep[x]))
  
  packageStartupMessage("tcpl (v", as.character(utils::packageVersion("tcpl")), 
                        ") loaded with the following settings:\n  ",
                        paste(cs, collapse = "\n  "),
                        "\nDefault settings stored in tcpl config file. See ", 
                        "?tcplConf for more information.")
  
}
