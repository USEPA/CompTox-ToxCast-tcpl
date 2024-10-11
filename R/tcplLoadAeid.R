#-------------------------------------------------------------------------------
# tcplLoadAeid: Load assay endpoint id and name for the given fields
#-------------------------------------------------------------------------------

#' @rdname assay_funcs
#' @import data.table
#' @export

tcplLoadAeid <- function(fld = NULL, val = NULL, add.fld = NULL) {
  
  if (getOption("TCPL_DRVR") == "API") {
    dat <- tcplQueryAPI(resource = "assay", fld = fld, val = val, return_flds = c("aeid", "assay_component_endpoint_name", add.fld))
    setnames(dat, "assay_component_endpoint_name", "aenm")
    setorder(dat, "aeid")
    return(unique(dat, by = c(fld, "aeid", "aenm")))
  }
  
  tbl = c("assay_component_endpoint", "assay", "assay_component")
  out <- c("assay_component_endpoint.aeid", 
           "assay_component_endpoint.assay_component_endpoint_name")
  
  qstring <- .buildAssayQ(out = out, 
                          tblo = c(1, 2, 4, 3, 6), 
                          fld = fld, 
                          val = val, 
                          add.fld = add.fld)
  
  dat <- tcplQuery(query = qstring, db = getOption("TCPL_DB"), tbl=tbl)
  
  dat[]

}

#-------------------------------------------------------------------------------
