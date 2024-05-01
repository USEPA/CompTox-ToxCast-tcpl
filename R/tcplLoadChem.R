#-------------------------------------------------------------------------------
# tcplLoadChem: Load sample/chemical information
#-------------------------------------------------------------------------------

#' @title Load sample/chemical information
#' 
#' @description
#' \code{tcplLoadChem} queries the tcpl database and returns the chemical 
#' information for the given field and values. 
#' 
#' @param field Character of length 1, the field to query on
#' @param val Vector of values to subset on
#' @param exact Logical, should chemical names be considered exact?
#' @param include.spid Logical, should spid be included?
#' 
#' @details 
#' The 'field' parameter is named differently from the 'fld' parameter seen
#' in other functions because it only takes one input.
#' 
#' In the MySQL environment the user should be able to give partial
#' chemical name strings, to find chemicals with similar names. For example,
#' setting 'val' to "phenol" when 'field' is "chnm" and 'exact' is
#' \code{FALSE} might pull up the chemicals "Bisphenol A" and
#' "4-Butylphenol". More technically, setting 'exact' to \code{FALSE} passes
#' the string in 'val' to an RLIKE statement within the MySQL query.  
#' 
#' @examples 
#' ## Store the current config settings, so they can be reloaded at the end 
#' ## of the examples
#' conf_store <- tcplConfList()
#' tcplConfExample()
#' 
#' ## Passing no parameters gives all of the registered chemicals with their
#' ## sample IDs
#' tcplLoadChem()
#' 
#' ## Or the user can exclude spid and get a unique list of chemicals
#' tcplLoadChem(include.spid = FALSE)
#' 
#' ## In addition, the user can retrieve only the registered chemicals from the chemical table
#' tcplLoadChem(field = 'chem.only')
#' 
#' ## Other examples:
#' tcplLoadChem(field = "chnm", val = "Bisphenol A")
#' tcplLoadChem(field = "chid", val = 20182)
#' 
#' ## Reset configuration
#' options(conf_store)
#' 
#' @return A data.table with the chemical information for the given parameters
#' 
#' @import data.table
#' @export

tcplLoadChem <- function(field = NULL, val = NULL, exact = TRUE,
                         include.spid = TRUE) {
  
  if (getOption("TCPL_DRVR") == "API") {
    if (tolower(field) != "spid") stop("When drvr option is set to 'API', only 'spid' is a valid 'field' value.")
    if (!exact) exact <- TRUE
    dat <- tcplQueryAPI(resource = "data", fld = "spid", val = val, return_flds = c("spid", "chid", "casn", "chnm", "dsstox_substance_id"))
    setorder(dat, "spid")
  } else {
    tbl <- c("chemical", "sample")
    ## Variable-binding to pass R CMD Check
    code <- casn <- chid <- chnm <- dsstox_substance_id <- NULL
    
    if (!is.null(field)) {
      vfield <- c("chid", "spid", "chnm", "casn", "code", "chem.only","dsstox_substance_id")
      if (!field %in% vfield) stop("Invalid 'field' value.")
    }
    
    
    qstring <- .ChemQ(field = field, val = val, exact = exact)
    
    dat <- tcplQuery(query = qstring, tbl=tbl)
    dat <- as.data.table(dat)
    
    if (nrow(dat) == 0) {
      warning("The given ", field,"(s) are not in the tcpl database.")
      return(dat[])
    }
  }
  
  dat[ , code := NA_character_]
  dat[!is.na(casn) , code := paste0("C", gsub("-|_", "", casn))]
  dat[ , chid := as.integer(chid)]  
  dat <- unique(dat)
  
  if (include.spid) return(unique(dat, by = c("spid", "chid")))
  
  dat <- unique(dat[ , list(chid, chnm, casn, code, dsstox_substance_id)])

  unique(dat, by = c("spid", "chid"))[]
  
}

#-------------------------------------------------------------------------------
