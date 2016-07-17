#-------------------------------------------------------------------------------
# tcplLoadUnit: Load response units for assay endpoints
#-------------------------------------------------------------------------------

#' @title Load response units for assay endpoints
#' 
#' @description 
#' \code{tcplLoadUnit} queries the tcpl databases and returns a data.table
#' with the response units for the given assay endpoint ids (aeid).
#' 
#' @param aeid Integer, assay endpoint ids 
#' 
#' @return A data.table containing level 3 correction methods for the given
#' aeids.
#' 
#' @seealso \code{\link{tcplQuery}}, \code{\link{data.table}}
#' 
#' @import data.table

tcplLoadUnit <- function(aeid) {
  
  qformat <- 
    "
    SELECT
      aeid,
      normalized_data_type AS resp_unit
    FROM
      assay_component_endpoint
    WHERE
      aeid IN (%s);
    "
  
  qstring <- sprintf(qformat, paste(aeid, collapse = ","))
  
  dat <- tcplQuery(query = qstring, db = getOption("TCPL_DB"))
  
  if (nrow(dat) == 0) {
    warning("The given aeid(s) do not have response units.")
    return(dat)
  }
  
  len_miss <- lw(!aeid %in% dat$aeid)
  if (len_miss > 0) {
    warning(len_miss, "of the given aeid(s) do not have response units.")
  }
  
  dat[]
  
}

#-------------------------------------------------------------------------------
