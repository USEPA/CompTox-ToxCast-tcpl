#-------------------------------------------------------------------------------
# .ChemQ: Create tcplLoadChem query string 
#-------------------------------------------------------------------------------

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
        qstring <- sprintf(qformat, val, val)
      } else {
        qformat <- paste(qformat, "chnm RLIKE %s;")
        val <- paste0("\"", paste(val, collapse = "|"), "\"")
        qstring <- sprintf(qformat, val, val)
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

