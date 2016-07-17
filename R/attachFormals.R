.attachFormals <- function(fun, envir = NULL) {
  
  if (is.null(envir)) envir <- parent.frame()
  f <- formals(deparse(substitute(fun)))
  f <- lapply(f, function(x) if(class(x) != "name") eval(x) else x)
  mapply(assign, names(f), f, MoreArgs = list(envir = envir))
  NULL
  
}