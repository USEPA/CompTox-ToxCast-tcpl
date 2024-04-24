#-------------------------------------------------------------------------------
# tcplLoadAsid: Load assay source id and name for the given fields
#-------------------------------------------------------------------------------

#' @rdname assay_funcs
#' @import data.table
#' @export

tcplLoadAsid <- function(fld = NULL, val = NULL, add.fld = NULL) {
  
  if (getOption("TCPL_DRVR") == "API") {
    dat <- tcplQueryAPI(resource = "assay", fld = fld, val = val, return_flds = c("asid", "assay_source_name", add.fld))
    setnames(dat, "assay_source_name", "asnm")
    setorder(dat, "asid")
    return(unique(dat, by = c(fld, "asid", "asnm")))
  }
  
  tbl = "assay_source"
  out <- c("assay_source.asid", 
           "assay_source.assay_source_name")
  
  qstring <- .buildAssayQ(out = out, 
                          tblo = c(6, 4:1), 
                          fld = fld, 
                          val = val, 
                          add.fld = add.fld)
  dat <- tcplQuery(query = qstring, db = getOption("TCPL_DB"), tbl=tbl)
  
  dat[]
  
}

#-------------------------------------------------------------------------------