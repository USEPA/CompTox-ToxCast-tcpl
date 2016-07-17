#-------------------------------------------------------------------------------
# tcplMthdList: 
#-------------------------------------------------------------------------------

#' @rdname mthd_funcs
#' @export

tcplMthdList <- function(lvl, type = "mc") {
  
  tbl <- paste0(type, lvl, "_methods")
  qstring <- paste0("SELECT * FROM ", tbl, ";")
  
  ## Suppress warnings because the data fields are not recognized by R and 
  ## imported as character. 
  dat <- suppressWarnings(tcplQuery(qstring, getOption("TCPL_DB")))
  
  if (nrow(dat) == 0) {
    warning("No ", type, lvl, " methods in the tcpl databases.")
    return(dat[])
  }
  
  drop_cols <- c("created_date", "modified_date", "modified_by")
  dat <- dat[ , .SD, .SDcols = setdiff(names(dat), drop_cols)]

  dat[]
  
}

#-------------------------------------------------------------------------------
