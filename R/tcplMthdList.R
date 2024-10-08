#-------------------------------------------------------------------------------
# tcplMthdList: 
#-------------------------------------------------------------------------------

#' @rdname mthd_funcs
#' @export

tcplMthdList <- function(lvl, type = "mc") {
  
  if (length(lvl) > 1) stop("'lvl' must be an integer of length 1.")
  if (!type %in% c("mc", "sc")) stop("Invalid 'type' value.")
  if (type == "mc" & !lvl %in% c(2, 3,4, 5, 6)) stop("Invalid 'lvl' value.")
  if (type == "sc" & !lvl %in% 1:2) stop("Invalid 'lvl' value.")
  
  if(getOption("TCPL_DRVR") == "API"){
    return(tcpl::mthd_list_defaults[grep(paste0(type,lvl), names(tcpl::mthd_list_defaults))][[1]])
  }
  
  tbl <- paste0(type, lvl, "_methods")
  qstring <- paste0("SELECT * FROM ", tbl, ";")
  
  ## Suppress warnings because the data fields are not recognized by R and 
  ## imported as character. 
  #dat <- suppressWarnings(tcplQuery(qstring, getOption("TCPL_DB"), tbl=c("mc2_methods")))
  dat <- suppressWarnings(tcplQuery(qstring, getOption("TCPL_DB"), tbl=tbl))
  
  if (nrow(dat) == 0) {
    warning("No ", type, lvl, " methods in the tcpl databases.")
    return(dat[])
  }
  
  drop_cols <- c("created_date", "modified_date", "modified_by")
  dat <- dat[ , .SD, .SDcols = setdiff(names(dat), drop_cols)]

  dat[]
  
}

#-------------------------------------------------------------------------------
