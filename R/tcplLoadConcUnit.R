#-------------------------------------------------------------------------------
# tcplLoadConcUnit: Load concentration units for assay endpoints
#-------------------------------------------------------------------------------

#' @title Load concentration units for assay endpoints
#' 
#' @description 
#' \code{tcplLoadUnit} queries the tcpl databases and returns a data.table
#' with the concentration units for the given assay endpoint ids (spid).
#' 
#' @param spid Integer, assay endpoint ids 
#' 
#' @return A data.table containing level 3 correction methods for the given
#' spids.
#' 
#' @seealso \code{\link{tcplQuery}}, \code{\link[data.table]{data.table}}
#' 
#' @import data.table

tcplLoadConcUnit <- function(spid) {
  
  if (getOption("TCPL_DRVR") == "API") {
    dat <- tcplQueryAPI(resource = "data", fld = "spid", val = spid, return_flds = c("spid", "tested_conc_unit"))
    if (length(colnames(dat))) {
      setnames(dat, "tested_conc_unit", "conc_unit")
      setorder(dat, "spid")
    }
    return(unique(dat, by = c("spid", "conc_unit")))
  }
  
  qformat <- 
    '
    SELECT
      spid,
      tested_conc_unit AS conc_unit
    FROM
      sample
    WHERE
      spid IN ("%s");
    '
  
  qstring <- sprintf(qformat, paste(spid, collapse = '","'))
  
  dat <- tcplQuery(query = qstring, db = getOption("TCPL_DB"), tbl=c("sample"))
  
  if (nrow(dat) == 0) {
    #warning("The given spid(s) do not have concentration units.")
    return(dat)
  }
  
  len_miss <- lw(!spid %in% dat$spid)
  if (len_miss > 0) {
    #warning(len_miss, " of the given spid(s) do not have concentration units.")
  }
  
  dat[]
  
}

#-------------------------------------------------------------------------------
