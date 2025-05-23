#-------------------------------------------------------------------------------
# tcplLoadChemList: Load chemical list information 
#-------------------------------------------------------------------------------

#' @title Load chemical list information
#' 
#' @description
#' \code{tcplLoadChemList} queries the tcpl databases and returns information 
#' about the chemical lists.
#' 
#' @param field Character of length 1, \code{'chid'}, \code{'dsstox_substance_id'} 
#' or \code{'list_acronym'}, whether to search by chemical id (chid), 
#' dsstox_substance_id, or list_acronym
#' @param val The values to query on
#' 
#' @details
#' Chemicals are stored in different lists by chemical ID. Therefore, it 
#' is not possible to delineate samples with the same chemical ID into two 
#' distinct chemical lists. However, it is possible for a chemical ID to 
#' belong to more than one (or no) chemical lists. 
#' 
#' When chemicals belong to more than one list, the chemical is listed 
#' multiple times (one for each distinct list).
#' 
#' @examples 
#' \dontrun{
#' ## Passing no parameters gives all of the chemical IDs that have a chemical
#' ## list registered
#' clist <- tcplLoadChemList()
#' 
#' ## Notice there are different number of rows in tcplLoadChemList than in tcplLoadChem, 
#' ## indicating some chemicals must belong to more than list (or no lists).
#' chem <- tcplLoadChem(include.spid = TRUE)
#' nrow(chem)
#' nrow(clist)
#' 
#' 
#' ## Show the unique chemical lists
#' clist[ , unique(list_acronym)]
#' 
#' ## Specifying a chemical list will not show what other libraries a 
#' ## chemical might belong to.
#' tcplLoadChemList(field = "list_acronym", val = "CPDBAS")
#' tcplLoadChemList(field = "chid", val = 20182)
#' tcplLoadChemList(field = "dsstox_substance_id", val = "DTXSID7020182")
#' }
#' 
#' @return A data.table with the chemical list information for the given
#' parameters.
#' 
#' @import data.table
#' @export

tcplLoadChemList <- function(field = NULL, val = NULL) {
  tbl = c("chemical_library")
  if (!is.null(field)) {
    vfield <- c("chid", "dsstox_substance_id","list_acronym")
    if (!field %in% vfield) stop("Invalid 'field' value.")
  }
    
  qstring <- .ChemListQ(field = field, val = val)
  
  dat <- tcplQuery(query = qstring, db = getOption("TCPL_DB"), tbl=tbl)
  
  
  if (!is.null(field)) {
    
    if (nrow(dat) == 0) {
      warning("The given ", field,"(s) do not have chemical list(s) assigned.")
      return(dat[])
    }
    
  }
  
  dat[]
  
}

#-------------------------------------------------------------------------------
