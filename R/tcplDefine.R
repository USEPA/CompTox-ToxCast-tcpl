#-------------------------------------------------------------------------------
# tcplDefine: Load data dictionary descriptions
#-------------------------------------------------------------------------------

#' @title Load data dictionary descriptions
#' 
#' @description
#' \code{tcplDefine} queries the tcpl databases and returns field descriptions 
#' from the data dictionary.
#' 
#' @param val The values to query on. Can be any combination of table names
#' (to return all of its field descriptions) and field names
#' 
#' @details
#' Short descriptions of fields for different tables are stored in a data
#' dictionary. Query by table name to retrieve descriptions of each field in
#' the given table, and/or query by field name to retrieve descriptions on
#' every field with the given name, regardless of which table they belong to.
#' 
#' @examples 
#' ## Store the current config settings, so they can be reloaded at the end 
#' ## of the examples
#' conf_store <- tcplConfList()
#' tcplConf(drvr = "example")
#' 
#' ## Passing no parameters returns all of the fields described in the data
#' ## dictionary
#' tcplDefine()
#' 
#' ## Specifying table names of 'chemical' and 'sample' yields all of the
#' ## fields from the 'chemical' and 'sample' tables
#' tcplDefine(c("chemical", "sample"))
#' 
#' ## Specifying a field of 'wllt' yields all of the fields from any table that
#' ## contains 'wllt' as a field
#' tcplDefine("wllt")
#' 
#' ## Specifying a combination of table and field names results in all of the
#' ## fields which are contained in the given tables and all of the given fields
#' ## found in any table
#' tcplDefine(c("chemical", "spid", "wllt"))
#' 
#' ## Reset configuration
#' options(conf_store)
#' 
#' @return A data.table with the data dictionary information for the given
#' parameters.
#' 
#' @import data.table
#' @export

tcplDefine <- function(val = NULL) {
    
  tbl = c("invitrodb_dd")
  
  qformat <- "SELECT invitrodb_table, invitrodb_field, description FROM invitrodb_dd"
  
  if (!is.null(val)) {
    
    vstring <- paste0("\"", val, "\"", collapse = ",")
    qformat <- paste(qformat, "WHERE invitrodb_table IN (%s) OR invitrodb_field IN (%s);")
    qstring <- sprintf(qformat, vstring, vstring)
    
  } else {
    
    qstring <- paste0(qformat, ";")
    
  }
  
  dat <- tcplQuery(query = qstring, db = getOption("TCPL_DB"), tbl=tbl)
  
  if (!is.null(val)) {
    
    if (nrow(dat) == 0) {
      warning("The given table and/or field names were not found.")
      return(dat[])
    }
    
  }
  
  dat[]
    
}

#-------------------------------------------------------------------------------
