#-------------------------------------------------------------------------------
# tcplQueryAPI: Query the CCTE Bioactivity API
#-------------------------------------------------------------------------------

#' @rdname query_funcs
#' 
#' @import data.table
#' @importFrom ccdR get_bioactivity_details_batch get_all_assays
#' @export


tcplQueryAPI <- function(resource = "data", fld = NULL, val = NULL, return_flds = NULL) {
  
  if (getOption("TCPL_DRVR") != "API") stop("TCPL_DRVR must be set to 'API'. See ?tcplConf.")
  
  if (resource == "data") {
    
    # check fld
    if (is.null(fld)) stop("'fld' cannot be NULL")
    if (length(fld) > 1) stop("'fld' must be length 1")
    fld <- if(tolower(fld) == "m4id") tolower(fld) else toupper(fld)
    if (!(fld %in% c("AEID", "SPID", "m4id", "DTXSID"))) 
      stop("'fld' must be one of 'AEID', 'SPID', 'm4id', or 'DTXSID'")
    
    # get data from API using ccdR
    dat <- suppressMessages(exec(get_bioactivity_details_batch, !!sym(fld) := val, Server := getOption("TCPL_HOST")))
    
    # remove missing elements
    lb <- length(dat) # store length before
    na_names <- names(dat[sapply(dat, nrow) == 0])
    dat <- dat[sapply(dat, nrow) > 0]
    if (lb != length(dat)) warning(paste0("Data not found for the following 'fld' and 'val' combos: \n", paste0(fld, ": ", na_names, collapse = "\n")))
    
    dat <- rbindlist(dat, use.names = TRUE, fill = TRUE)
    if (nrow(dat) == 0) return(dat)
    
    dat$dsstox_substance_id <- dat$dtxsid
    
    # unlist logc to conc
    dat <- dat %>% rowwise() %>% mutate(conc = list(10^unlist(logc))) %>% as.data.table()
    if(tolower(fld) != "m4id") dat <- dat %>% tidyr::unnest(cols = c("resp", "logc", "flag", "mc6MthdId")) %>% as.data.table()
    
  } else if (resource == "assay") {
    
    # using get all assays and then filtering
    dat <- get_all_assays(Server = paste0(getOption("TCPL_HOST"))) %>% as.data.table()
    
  } else stop("'resource' must = 'data' or 'assay'.")
  
  # adjust column names
  colnames(dat) <- gsub("([a-z])([A-Z])", "\\1_\\L\\2", colnames(dat), perl = TRUE)
  
  # filter if searching by assay list
  if (resource == "assay" && !is.null(fld)) {
    
    if (any(!(fld %in% colnames(dat)))) 
      stop(paste0("Query field(s) '", 
                  paste0(fld[which(!(fld %in% colnames(dat)))], collapse = "', '"), 
                  "' not available. Try using from the following: \n", 
                  paste0(colnames(dat), collapse = "\n")))
    
    # create index vector to length of field
    fld_indices <- if (length(fld) == 1 | length(fld) == length(val)) 1:length(fld) 
    else stop("'fld' and 'val' must be the same size if length(fld) > 1")
    
    # put val into a list or move items into first element
    if (length(fld) == 1) val <- list(unlist(val))
    
    # for each field filter by same indexed val
    for (i in fld_indices) {
      dat <- dat[dat[[fld[i]]] %in% val[[i]],]
    }
    
  }
  
  if (is.null(return_flds)) return(dat)
  else {
    return_flds <- intersect(c(tolower(fld), return_flds), colnames(dat))
    return(dat[, ..return_flds])
  }
}