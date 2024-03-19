#-------------------------------------------------------------------------------
# tcplQueryAPI: Query the CCTE Bioactivity API
#-------------------------------------------------------------------------------

#' @rdname query_funcs
#' 
#' @import data.table
#' @export


tcplQueryAPI <- function(resource = "data", fld = NULL, val = NULL, return_flds = NULL) {
  
  if (resource == "data") {
    
    # check fld
    if (is.null(fld)) stop("'fld' cannot be NULL")
    fld <- if(tolower(fld) == "m4id") tolower(fld) else toupper(fld)
    if (!(fld %in% c("AEID", "SPID", "m4id", "DTXSID"))) stop("'fld' must be one of 'AEID', 'SPID', 'm4id', or 'DTXSID'")
    
    # get data from API using ccdR
    dat <- exec(get_bioactivity_details_batch, !!sym(fld) := val, Server := paste0(getOption("TCPL_HOST"), "/data"))
    dat <- do.call(rbind, dat)
    
    # adjust column names
    colnames(dat) <- gsub("([a-z])([A-Z])", "\\1_\\L\\2", colnames(dat), perl = TRUE)
    dat$dsstox_substance_id <- dat$dtxsid
    
    # unlist logc to conc
    dat <- dat |> rowwise() |> mutate(conc = list(10^unlist(logc)))
    
    if (is.null(return_flds)) return(dat)
    else return(dat[, ..return_flds])
    
  } else if (resource == "assay") {
    
    # check fld
    if (is.null(fld)) fld == "AEID"
    fld <- toupper(fld)
    if (fld != "AEID") stop("'fld' must be 'AEID' if resource is 'assay'")
    
  } else stop("'resource' must = 'data' or 'assay'.")
  
}