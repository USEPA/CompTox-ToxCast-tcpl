#-------------------------------------------------------------------------------
# tcplLoadChem: Load sample/chemical information
#-------------------------------------------------------------------------------

#' @title Load sample/chemical information
#' 
#' @description
#' \code{tcplLoadChem} queries the tcpl database and returns the chemcial 
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
#' The functionality of the 'exact' parameter cannot be demonstrated within
#' the SQLite environment. However, in the MySQL environment the user should
#' be able to give parital chemcial name strings, to find chemicals with 
#' similar names. For example, setting 'val' to "phenol" when 'field' is "chnm"
#' and 'exact' is \code{FALSE} might pull up the chemicals "Bisphenol A" and
#' "4-Butylphenol". More technically, setting 'exact' to \code{FALSE} passes
#' the string in 'val' to an RLIKE statement within the MySQL query.  
#' 
#' @examples 
#' ## Store the current config settings, so they can be reloaded at the end 
#' ## of the examples
#' conf_store <- tcplConfList()
#' tcplConfDefault()
#' 
#' ## Passing no parameters gives all of the registered chemicals with their
#' ## sample IDs
#' tcplLoadChem()
#' 
#' ## Or the user can exclude spid and get a unique list of chemicals
#' tcplLoadChem(include.spid = FALSE)
#' 
#' ## Other examples:
#' tcplLoadChem(field = "chnm", val = "Bisphenol A")
#' tcplLoadChem(field = "chid", val = 1:5)
#' 
#' ## Reset configuration
#' options(conf_store)
#' 
#' @return A data.table with the chemical information for the given parameters
#' 
#' @import data.table
#' @export

tcplLoadChem <- function(field = NULL, val = NULL, exact = TRUE, tcplLiteChem = NULL,
                         include.spid = TRUE) {
  tbl <- c("chemical", "sample")
  ## Variable-binding to pass R CMD Check
  code <- casn <- chid <- chnm <- NULL
  
  if (!is.null(field)) {
    vfield <- c("chid", "spid", "chnm", "casn", "code")
    if (!field %in% vfield) stop("Invalid 'field' value.")
  }
  
  # if (getOption("TCPL_DRVR") == "SQLite") {
  #   if (!exact) warning("The exact = FALSE option is not supported in SQLite.")
  #   exact <- TRUE
  # }
  
  #if (getOption("TCPL_DRVR") == "tcplLite") {
  if (!is.null(tcplLiteChem)) {
    # If the tcplLiteChem file is defined, preprocessing needs to occur
    # Need to preprocess: Load in the SPID's from the current assay and map to the CHID in 'chemical'. Then write to 'sample'. Once written, tcplLoadChem will work
    db <- getOption("TCPL_DB")
    for (t in tbl) {
      fpath <- paste(db, t, sep='/')
      fpath <- paste(fpath, 'csv', sep='.')
      assign(t, read.table(fpath, header=T, sep=',')) # Define the chemical and sample data tables
    }
    if (nrow(sample) > 0) {
      warning("'sample' table is already populated, double check you need to init the chemical map")
    }
    else { # Write the whole sample using chemical AND the assay-specific file: tcplLiteChem
      assayChem <- read.table(tcplLiteChem, header=T, sep=',')
      tempDT <- as.data.table(merge(assayChem, chemical, by=c('casn')))
      tcplRegister(what='spid', flds=tempDT[, list(spid, chid)]) # Add extra fields later
    }
  }
  
  qstring <- .ChemQ(field = field, val = val, exact = exact)
    
  dat <- tcplQuery(query = qstring, tbl=tbl)
  dat <- as.data.table(dat)
  if (nrow(dat) == 0) {
    warning("The given ", field,"(s) are not in the tcpl database.")
    return(dat[])
  }
  
  dat[ , code := NA_character_]
  dat[!is.na(casn) , code := paste0("C", gsub("-|_", "", casn))]
  dat[ , chid := as.integer(chid)]  
  dat <- unique(dat)
  
  if (include.spid) return (dat)
  
  dat <- unique(dat[ , list(chid, chnm, casn, code)])

  dat[]
  
}

#-------------------------------------------------------------------------------
