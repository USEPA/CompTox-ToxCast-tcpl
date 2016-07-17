#-------------------------------------------------------------------------------
# tcplPrepOtpt: Map assay/chemcial ID values to names
#-------------------------------------------------------------------------------

#' @title Map assay/chemcial ID values to annotation information
#' 
#' @description
#' \code{tcplPrepOtpt} queries the chemical and assay information from the tcpl
#' database, and maps the annotation information to the given data.
#' 
#' @param dat data.table, output from \code{\link{tcplLoadData}}
#' @param ids Character, (optional) a subset of ID fields to map
#' 
#' @details
#' \code{tcplPrepOtpt} is used to map chemical and assay identifiers to their
#' respective names and annotation information to create a human-readable table
#' that is more suitable for an export/output.
#' 
#' By default the function will map sample ID (spid), assay component id (acid),
#' and assay endpoint ID (aeid) values. However, if 'ids' is not null, the 
#' function will only attempt to map the ID fields given by 'ids.'
#' 
#' @examples
#' ## Store the current config settings, so they can be reloaded at the end 
#' ## of the examples
#' conf_store <- tcplConfList()
#' tcplConfDefault()
#' 
#' ## Load some example data
#' d1 <- tcplLoadData(1)
#' 
#' ## Check for chemical name in 'dat'
#' "chnm" %in% names(d1) ## FALSE
#' 
#' ## Map chemical annotation only
#' d2 <- tcplPrepOtpt(d1, ids = "spid")
#' "chnm" %in% names(d2) ## TRUE
#' "acnm" %in% names(d2) ## FALSE
#' 
#' ## Map all annotations 
#' d3 <- tcplPrepOtpt(d1) ## Also works if function is given d2
#' "chnm" %in% names(d2) ## TRUE
#' "acnm" %in% names(d2) ## TRUE
#' 
#' ## Reset configuration
#' options(conf_store)
#' 
#' @return The given data.table with chemical and assay information mapped
#' @export

tcplPrepOtpt <- function(dat, ids = NULL) {
  
  ## Variable-binding to pass R CMD Check
  acnm <- acid <- aenm <- resp_unit <- aeid <- spid <- chid <- NULL
  code <- chnm <- casn <- NULL
  
  if (!"data.table" %in% class(dat)) {
    stop("'dat' must be a data.table.")
  }
  
  dnames <- names(dat)
  
  if (is.null(ids)) ids <- dnames
  
  if ("acid" %in% ids) { # Map acid names
    if (!"acid" %in% dnames) {
      warning("'acid' field is not in dat. No 'acid' mapping performed.")
    } else {
      if ("acnm" %in% dnames) dat[ , acnm := NULL]
      dat <- merge(x = tcplLoadAcid("acid", dat[ , unique(acid)]), 
                   y = dat, 
                   by = "acid", 
                   all.y = TRUE)
    }
  } 
  
  if ("aeid" %in% ids) { # Map aeid names and resp_units
    if (!"aeid" %in% dnames) {
      warning("'aeid' field is not in dat. No 'aeid' mapping performed.")
    } else {
      if ("aenm" %in% dnames) dat[ , aenm := NULL]
      if ("resp_unit" %in% dnames) dat[ , resp_unit := NULL]
      dat <- merge(x = tcplLoadAeid("aeid", dat[ , unique(aeid)]), 
                   y = dat, 
                   by = "aeid",
                   all.y = TRUE)
      dat <- merge(dat, tcplLoadUnit(dat[ , unique(aeid)]), by = "aeid")
    }
  }
  
  if ("spid" %in% ids) {
    if (!"spid" %in% dnames) {
      warning("'spid' field is not in dat. No 'spid' mapping performed.")
    } else {
      if ("chid" %in% dnames) dat[ , chid := NULL]
      if ("casn" %in% dnames) dat[ , casn := NULL]
      if ("chnm" %in% dnames) dat[ , chnm := NULL]
      if ("code" %in% dnames) dat[ , code := NULL]
      cmap <- suppressWarnings(tcplLoadChem("spid", dat[ , unique(spid)]))
      dat <- merge(cmap, dat, by = "spid", all.y = TRUE)
    }
  }
  
  dat[]
  
}

#-------------------------------------------------------------------------------
