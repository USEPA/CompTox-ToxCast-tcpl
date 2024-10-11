#-------------------------------------------------------------------------------
# tcplLoadAid: Load assay id and name for the given fields
#-------------------------------------------------------------------------------

#' @rdname assay_funcs
#' @import data.table
#' @export

tcplLoadAid <- function(fld = NULL, val = NULL, add.fld = NULL) {
  
  if (getOption("TCPL_DRVR") == "API") {
    dat <- tcplQueryAPI(resource = "assay", fld = fld, val = val, return_flds = c("aid", "assay_name", add.fld))
    setnames(dat, "assay_name", "anm")
    setorder(dat, "aid")
    return(unique(dat, by = c(fld, "aid", "anm")))
  }
  
  tbl=c("assay")
  out <- c("assay.aid", 
           "assay.assay_name")
  
  qstring <- .buildAssayQ(out = out, 
                          tblo = c(6, 1, 4, 3, 2), 
                          fld = fld, 
                          val = val, 
                          add.fld = add.fld)
  
  dat <- tcplQuery(query = qstring, db = getOption("TCPL_DB"), tbl=tbl)
  
  dat[]
  
}

#-------------------------------------------------------------------------------