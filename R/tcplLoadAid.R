#-------------------------------------------------------------------------------
# tcplLoadAid: Load assay id and name for the given fields
#-------------------------------------------------------------------------------

#' @rdname assay_funcs
#' @import data.table
#' @export

tcplLoadAid <- function(fld = NULL, val = NULL, add.fld = NULL) {
  
  out <- c("assay.aid", 
           "assay.assay_name")
  
  qstring <- .buildAssayQ(out = out, 
                          tblo = c(6, 1, 4, 3, 2), 
                          fld = fld, 
                          val = val, 
                          add.fld = add.fld)
  
  dat <- tcplQuery(query = qstring, db = getOption("TCPL_DB"))
  
  dat[]
  
}

#-------------------------------------------------------------------------------