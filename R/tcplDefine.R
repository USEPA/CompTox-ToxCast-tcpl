#-------------------------------------------------------------------------------
# tcplDefine: Load data dictionary descriptions
#-------------------------------------------------------------------------------

#' @title Load data dictionary descriptions
#' 
#' @description
#' \code{tcplDefine} queries the tcpl databases and returns field descriptions 
#' from the data dictionary.
#' 
#' @param field Character of length 1, \code{'invitrodb_table'} or 
#' \code{'invitrodb_field'}, whether to search by table name (invitrodb_table), 
#' or field name (invitrodb_field)
#' @param val The values to query on
#' 
#' @details
#' Short descriptions of fields for different tables are stored in a data
#' dictionary. Query by \code{'invitrodb_table'} to retrieve descriptions of 
#' each field in the given table, or query by \code{'invitrodb_field'} to
#' retrieve descriptions on every field with the given name, regardless of
#' which table they are in.
#' 
#' @examples 
#' ## Store the current config settings, so they can be reloaded at the end 
#' ## of the examples
#' conf_store <- tcplConfList()
#' tcplConfExample()
#' 
#' ## Passing no parameters returns all of the fields described in the data
#' ## dictionary
#' tcplDefine()
#' 
#' ## Specifying invitrodb tables of 'chemical' and 'sample' yields all of the
#' ## fields from the 'chemical' and 'sample' tables
#' tcplDefine(field="invitrodb_table", val=c("chemical", "sample"))
#' 
#' ## Specifying an invitrodb field of 'wllt' yields all of the fields from any
#' ## table that contains 'wllt' as a field
#' tcplDefine(field="invitrodb_field", val=c("wllt"))
#' 
#' ## Reset configuration
#' options(conf_store)
#' 
#' @return A data.table with the data dictionary information for the given
#' parameters.
#' 
#' @import data.table
#' @export

tcplDefine <- function(field = NULL, val = NULL) {
  
  tbl = c("invitrodb_dd")
  
  qformat <- "SELECT invitrodb_table, invitrodb_field, description FROM invitrodb_dd"
  
  if (!is.null(field)) {
    
    vfield <- c("invitrodb_table", "invitrodb_field")
    if (!field %in% vfield) stop("Invalid 'field' value.")
    
    qformat <- paste(qformat, "WHERE %s IN (%s);")
    qstring <- sprintf(qformat, field, paste0("\"", val, "\"", collapse = ","))
    
  } else {
    
    qstring <- paste0(qformat, ";")
    
  }
  
  dat <- tcplQuery(query = qstring, db = getOption("TCPL_DB"), tbl=tbl)
  
  if (!is.null(field)) {
    
    if (nrow(dat) == 0) {
      warning("The given ", field, "(s) do not exist.")
      return(dat[])
    }
    
  }
  
  dat[]
  
}

#-------------------------------------------------------------------------------
