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
    
    dat$dsstox_substance_id <- dat$dtxsid
    
    # unlist logc to conc
    dat <- dat |> rowwise() |> mutate(conc = list(10^unlist(logc)))
    
  } else if (resource == "assay") {
    
    # check fld
    if (is.null(fld)) fld == "AEID"
    
    # get data from API using ccdR
    if (toupper(fld) == "AEID") { 
      # using get annotations by aeid
      dat <- get_annotation_by_aeid_batch(AEID = val, Server = paste0(getOption("TCPL_HOST")))
      dat <- do.call(rbind, dat)
    } else { 
      # using get all assays and then filtering
      dat <- get_all_assays(Server = paste0(getOption("TCPL_HOST")))
    }
    
  } else stop("'resource' must = 'data' or 'assay'.")
  
  # adjust column names
  colnames(dat) <- gsub("([a-z])([A-Z])", "\\1_\\L\\2", colnames(dat), perl = TRUE)
  
  # filter if searching by assay list for non-aeid val
  if (resource == "assay" && toupper(fld) != "AEID") 
    dat <- dat[dat[[fld]]== val,]
  
  if (is.null(return_flds)) return(dat)
  else {
    return_flds <- intersect(c(tolower(fld), return_flds), colnames(dat))
    return(dat[, return_flds])
  }
}