#-------------------------------------------------------------------------------
# tcplLoadClib: Load chemical library information 
#-------------------------------------------------------------------------------

#' @title Load chemical library information
#' 
#' @description
#' \code{tcplLoadClib} queries the tcpl databases and returns information 
#' about the chemial library.
#' 
#' @param field Character of length 1, \code{'chid'} or \code{'clib'}, whether 
#' to search by chemical id (chid), or chemical library (clib)
#' @param val The values to query on
#' 
#' @details
#' Chemicals are stored in different libraries by chemcial ID. Therefore, it 
#' is not possible to delineate samples with the same chemical ID into two 
#' distinct chemical libraries. However, it is possible for a chemcial ID to 
#' belong to more than one (or no) chemical libraries. 
#' 
#' When chemicals belong to more than one library, the chemical is listed 
#' multiple times (one for each distinct library).
#' 
#' @examples 
#' ## Store the current config settings, so they can be reloaded at the end 
#' ## of the examples
#' conf_store <- tcplConfList()
#' tcplConfDefault()
#' 
#' ## Passing no parameters gives all of the chemical ISs that have a chemical
#' ## library registered
#' clib <- tcplLoadClib()
#' 
#' ## Notice there are more rows in tcplLoadClib than in tcplLoadChem, 
#' ## indicating some chemicals must belong to more than library.
#' chem <- tcplLoadChem(include.spid = FALSE)
#' nrow(chem)
#' nrow(clib)
#' 
#' ## It is possible that some chemicals do not have a chemical library 
#' ## registered, although this is not the case in the example data.
#' all(chem$chid %in% clib$chid)
#' 
#' ## Show the unique chemical libraries
#' clib[ , unique(clib)]
#' 
#' ## Specifying a chemical library will not show what other libraries a 
#' ## chemical might belong to.
#' tcplLoadClib(field = "clib", val = "other")
#' tcplLoadClib(field = "chid", val = 1:2)
#' 
#' ## Reset configuration
#' options(conf_store)
#' 
#' @return A data.table with the chemical library information for the given
#' parameters.
#' 
#' @import data.table
#' @export

tcplLoadClib <- function(field = NULL, val = NULL) {
  
  if (!is.null(field)) {
    vfield <- c("chid", "clib")
    if (!field %in% vfield) stop("Invalid 'field' value.")
  }
    
  qstring <- .ClibQ(field = field, val = val)
  
  dat <- tcplQuery(query = qstring, db = getOption("TCPL_DB"))
  
  
  if (!is.null(field)) {
    
    if (nrow(dat) == 0) {
      warning("The given ", field,"(s) do not have chemical library assigned.")
      return(dat[])
    }
    
  }
  
  dat[]
  
}

#-------------------------------------------------------------------------------
