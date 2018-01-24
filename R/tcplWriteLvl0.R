#-------------------------------------------------------------------------------
# tcplWriteLvl0: Write level 0 screening data into the tcpl databases
#-------------------------------------------------------------------------------

#' @title Write level 0 screening data into the tcpl databases
#' 
#' @description
#' \code{tcplWriteLvl0} takes a data.table with level 0 screening data and 
#' writes the data into the level 0 tables in the tcpl databases.
#' 
#' @param dat data.table, the screening data to load
#' @param type Character of length 1, the data type, "sc" or "mc"
#' 
#' @details
#' This function appends data onto the existing table. It also deletes all the 
#' data for any acids or aeids dat contains from the given and all downstream 
#' tables.
#' 
#' Before writing any data the function maps the assay component source name(s)
#' (acsn) to assay component id (acid), ensures the proper class on each field
#' and checks for every test compound sample id (spid where wllt == "t") in the
#' tcpl chemical database. If field types get changed a warning is given 
#' listing the affected fields and they type they were coerced to. If the 
#' acsn(s) or spid(s) do not map to the tcpl databases the function will return
#' an error and the data will not be written.
#' 
#' The data type can be either 'mc' for mutliple concentration data, or 'sc'
#' for single concentration data. Multiple concentration data will be loaded
#' into the level tables, whereas the single concentration will be loaded into
#' the single tables.
#' 
#' @note
#' This function should only be used to load level 0 data.
#' 
#' @seealso \code{\link{tcplCascade}}, \code{\link{tcplAppend}}
#' 
#' @import data.table
#' @export

tcplWriteLvl0 <- function(dat, type) {
  
  ## Variable-binding to pass R CMD Check
  acsn <- acid <- wllt <- spid <- conc <- NULL
  
  if (!type %in% c("mc", "sc")) stop("Invalid 'type', see help page.")
  
  ## Map assay component source name to assay component id, if required
  if("acsn" %in% names(dat)){
    acid_info <- tcplLoadAcid("acsn", dat[ , unique(acsn)])[ , list(acsn, acid)]
    setkey(dat, acsn)
    setkey(acid_info, acsn)
    dat <- acid_info[dat]
    if (dat[ , any(is.na(acid))]) {
      cat("The following acsn(s) did not map to acid:\n")
      print(dat[is.na(acid), unique(acsn)])
      stop("Must correct the acsn mapping before loading the data.")
    }
    dat[ , acsn := NULL]
  }
  if(!"acid" %in% names(dat)){
    stop("Must supply either acsn or acid before loading the data.")
  }
  
  ## Ensure correct formatting
  char <- c("spid", "apid", "wllt", "srcf")
  char_test <- dat[ , sapply(.SD, class) != "character", .SDcols = char]
  if (any(char_test)) {
    char <- char[char_test]
    warning(paste(char, collapse = ";"), " coerced to character. May affect ",
            "data integrity.")
    dat[ , (char) := lapply(.SD, as.character), .SDcols = char]
  }
  intg <- c("acid", "rowi", "coli", "wllq")
  intg_test <- dat[ , sapply(.SD, class) != "integer", .SDcols = intg]
  if (any(intg_test)) {
    intg <- intg[intg_test]
    warning(paste(intg, collapse = ";"), " coerced to integer. May affect ",
            "data integrity.")
    dat[ , (intg) := lapply(.SD, as.integer), .SDcols = intg]
  } 
  real <- c("conc", "rval")
  real_test <- dat[ , sapply(.SD, class) != "numeric", .SDcols = real]
  if (any(real_test)) {
    real <- real[real_test]
    warning(paste(real, collapse = ";"), " coerced to numeric. May affect ",
            "data integrity.")
    dat[ , (real) := lapply(.SD, as.numeric), .SDcols = real]
  } 
  
  ## Check for samples in inventorydb
  cmap <- tcplLoadChem(field = "spid", val = dat[wllt == "t", unique(spid)])
  if (dat[wllt == "t" , lw(!spid %in% cmap$spid)] > 0) {
    cat("The following test compounds did not map to the tcpl databases:\n")
    print(dat[wllt == "t" & !spid %in% cmap$spid, unique(spid)])
    stop("Must correct the test compound mapping before loading the data.")
  }
  
  ## Check for concentration values on test wells
  if (dat[wllt == "t", any(is.na(conc))]) {
    stop("No concentration values for some test compounds.")
  }
  
  ## Likely unnecessary step to correct some unexplained string lookup 
  ## behavior, ie. dat[spid == "DMSO"] returns only 5 rows, but 
  ## dat[wllt == "n"] returns 10 rows AND dat[wllt == "n", unique(spid)] only 
  ## returns "DMSO"
  dat <- dat[ , .SD]
  
  ## Subset to level 0 fields
  outcols <- c("acid", "spid", "apid", "rowi", "coli", "wllt", "wllq", "conc",
               "rval", "srcf")
  if (any(!names(dat) %in% outcols)) {
    not_used <- names(dat)[!names(dat) %in% outcols]
    warning(paste(not_used, collapse = ","), " not inserted to databse.")
  }
  tcplWriteData(dat = dat[ , .SD, .SDcols = outcols], lvl = 0L, type = type)
  
}

#-------------------------------------------------------------------------------
