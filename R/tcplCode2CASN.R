#-------------------------------------------------------------------------------
# tcplCode2CASN: Convert chemical code to CAS Registry Number
#-------------------------------------------------------------------------------

#' @title Convert chemical code to CAS Registry Number
#' 
#' @description
#' \code{tcplCode2CASN} takes a code and converts it CAS Registry Number.
#' 
#' @param code Character of length 1, a chemical code
#' 
#' @details
#' The function checks for the validity of the CAS Registry Number. Also, 
#' the ToxCast data includes chemcials for which there is no CASRN. The 
#' convention for these chemicals is to give them a CASRN as NOCAS_chid; the
#' code for these compounds is CNOCASchid. The function handles the NOCAS 
#' compounds as they are stored in the database, as shown in the exmaple below.
#' 
#' @examples
#' tcplCode2CASN("C80057")
#' tcplCode2CASN("C09812420") ## Invalid CASRN will give a warning
#' tcplCode2CASN("CNOCAS0015") ## The underscore is reinserted for NOCAS codes
#' 
#' @return A CAS Registry Number.
#' 
#' @export

tcplCode2CASN <- function(code) {
  
  if (length(code) > 1) {
    code <- code[1]
    warning("length of 'code' greater than 1, only the first element used.")
  }
  
  code <- sub("C", "", code)
  
  if (grepl("NOCAS", code)) {
    
    code <- sub("NOCAS", "NOCAS_", code)
    return(code)
    
  }
  
  if (!grepl("[a-z]|[A-Z]", code)) {
    
    code <- as.numeric(sapply(code, strsplit, "")[[1]])
    clen <- length(code)
    test <- sum((clen - 1):1 * code[1:(clen - 1)]) %% 10 == code[clen]
    code <- paste(paste(code[1:(clen - 3)], collapse = ""),
                  paste(code[(clen - 2):(clen - 1)], collapse = ""),
                  code[clen],
                  sep = "-")
    if (test) return(code) 
  }
  
  warning("'code' does not appear to have a valid CAS Registry Number.")
  code
  
}

#-------------------------------------------------------------------------------
