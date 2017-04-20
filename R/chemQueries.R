#-------------------------------------------------------------------------------
# .ChemQ: Create tcplLoadChem query string 
#-------------------------------------------------------------------------------

.ChemQ <- function(field, val, exact) {
  
  qformat <- 
    "
      SELECT spid,chemical.chid,casn,chnm
      FROM sample LEFT JOIN chemical ON chemical.chid=sample.chid
      "
      
  if (getOption("TCPL_DRVR") == "SQLite") exact <- TRUE
  
  if (!is.null(field)) {
    
    nfld <- switch(field,
                   spid = "spid",
                   chid = "chemical.chid",
                   casn = "casn",
                   code = "casn",
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
    } else {
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
# .ClibQ: Create tcplLoadClib query string 
#-------------------------------------------------------------------------------

.ClibQ <- function(field, val) {
  
  qformat <- "SELECT chid, clib FROM chemical_library"
  
  if (!is.null(field)) {
    
    nfld <- switch(field,
                   chid = "chid",
                   clib = "clib")
    
    qformat <- paste(qformat, "WHERE %s IN (%s);")
    qstring <- sprintf(qformat, nfld, paste0("\"", val, "\"", collapse = ","))
    
  } else {
    
    qstring <- paste0(qformat, ";")
    
  }
  
  qstring
  
}

#-------------------------------------------------------------------------------
# END .ClibQ
#-------------------------------------------------------------------------------

