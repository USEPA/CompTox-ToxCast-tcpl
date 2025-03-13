#-------------------------------------------------------------------------------
# .ChemQ: Create tcplLoadChem query string 
#-------------------------------------------------------------------------------

#' @title Function to support queries by chemical
#' @note This function is not exported and not intended to be used by the user.
#' 
#' @param field Character, the field to query on
#' @param val Vector of values to subset on
#' @param exact Logical, should chemical names be considered exact?
#' 
#' @description
#' \code{.ChemQ} creates tcplLoadChem query string
#' 
#' @seealso \code{\link{tcplLoadData}}
#' @seealso \code{\link{tcplLoadChem}}
.ChemQ <- function(field, val, exact) {
  
  qformat <- 
    "
      SELECT spid,chemical.chid,casn,chnm,dsstox_substance_id
      FROM sample LEFT JOIN chemical ON chemical.chid=sample.chid
      "
      
  if (!is.null(field)) {
    
    nfld <- switch(field,
                   spid = "spid",
                   chid = "chemical.chid",
                   casn = "casn",
                   code = "casn",
                   chem.only = 'chem.only',
                   dsstox_substance_id = "dsstox_substance_id",
                   "chnm")
    
    if (field == "code") val <- suppressWarnings(sapply(val, tcplCode2CASN))
    
    qformat <- paste(qformat, "WHERE")
    
    if (nfld == "chnm") {
      if (exact) {
        qformat <- paste(qformat, "chnm IN (%s);")
        val <- paste0("\"", val, "\"", collapse = ",")
        qstring <- sprintf(qformat, val)
      } else {
        qformat <- paste(qformat, "chnm RLIKE %s;")
        val <- paste0("\"", paste(val, collapse = "|"), "\"")
        qstring <- sprintf(qformat, val)
      }
    } else if (nfld == 'chem.only') {
      qstring <- "
                SELECT *
                FROM chemical
                "
    }
      else {
      qformat <- paste(qformat, nfld, "IN (%s)")
      qstring <- sprintf(qformat, paste0("\"", val, "\"", collapse = ","))
    }
    
  } else {
    
    qstring <- qformat
    
  }
  
  qstring
  
}

#-------------------------------------------------------------------------------
# END .ChemQ
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# .ChemListQ: Create tcplLoadChemList query string 
#-------------------------------------------------------------------------------

#' @title Function to support queries by chemical
#' @note This function is not exported and not intended to be used by the user.
#' 
#' @param field Character, the field to query on
#' @param val Vector of values to subset on
#' 
#' @description
#' \code{.ChemListQ} creates tcplLoadChemList query string
#' 
#' @seealso \code{\link{tcplLoadChemList}}
.ChemListQ <- function(field, val) {
  
  qformat <- "SELECT * FROM chemical_lists"
  
  if (!is.null(field)) {
    
    nfld <- switch(field,
                   chid = "chid",
                   dsstox_substance_id = "dsstox_substance_id",
                   list_acronym = "list_acronym")
    
    qformat <- paste(qformat, "WHERE %s IN (%s);")
    qstring <- sprintf(qformat, nfld, paste0("\"", val, "\"", collapse = ","))
    
  } else {
    
    qstring <- paste0(qformat, ";")
    
  }
  
  qstring
  
}

#-------------------------------------------------------------------------------
# END .ChemListQ
#-------------------------------------------------------------------------------