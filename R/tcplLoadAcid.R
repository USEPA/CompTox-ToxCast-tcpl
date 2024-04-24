#-------------------------------------------------------------------------------
# tcplLoadAcid: Load assay component id and name for the given fields
#-------------------------------------------------------------------------------

#' @rdname assay_funcs
#' @import data.table
#' @export

tcplLoadAcid <- function(fld = NULL, val = NULL, add.fld = NULL) {
  
  if (getOption("TCPL_DRVR") == "API") {
    dat <- tcplQueryAPI(resource = "assay", fld = fld, val = val, return_flds = c("acid", "assay_component_name", add.fld))
    setnames(dat, "assay_component_name", "acnm")
    setorder(dat, "acid")
    return(unique(dat, by = c(fld, "acid", "acnm")))
  }
  
  tbl = c("assay_component", "assay_component_map", "assay")
  out <- c("assay_component.acid", 
           "assay_component.assay_component_name")
  
  qstring <- .buildAssayQ(out = out, 
                          tblo = c(1, 2, 4, 6, 3), 
                          fld = fld, 
                          val = val, 
                          add.fld = add.fld)
  
  dat <- tcplQuery(query = qstring, db = getOption("TCPL_DB"), tbl=tbl)
  
  dat[]
  
}

#-------------------------------------------------------------------------------