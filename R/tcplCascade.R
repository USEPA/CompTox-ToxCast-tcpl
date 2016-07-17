#-------------------------------------------------------------------------------
# tcplCascade: Do a cascading delete on tcpl screening data
#-------------------------------------------------------------------------------

#' @title Do a cascading delete on tcpl screening data
#' 
#' @description
#' \code{tcplCascade} deletes the data for the given id(s) starting at 
#' the processing level given. The delete will cascade through all subsequent 
#' tables.
#' 
#' @param lvl Integer of length 1, the first level to delete from
#' @param type Character of length 1, the data type, "sc" or "mc"
#' @param id Integer, the id(s) to delete. See details for more information.
#' 
#' @details
#' The data type can be either 'mc' for mutliple concentration data, or 'sc'
#' for single concentration data. Multiple concentration data will be loaded
#' into the level tables, whereas the single concentration will be loaded into
#' the single tables. 
#' 
#' If lvl is less than 3, id is interpreted as acid(s) and if lvl is greater 
#' than or equal to 3, id is interpreted as aeid(s).
#' 
#' @note
#' This function is not exported and not intended to be used by the user.
#' 
#' @import data.table
#' @importFrom methods is

tcplCascade <- function(lvl, type, id) {
    
  stime <- Sys.time()
  
  if (length(lvl) > 1) {
    stop("Invalid lvl input - must be an integer of length 1.")
  }
  
  db <- getOption("TCPL_DB")
  
  if (type == "mc") {
    
    if (lvl == 0) tcplDelete(tbl = "mc0", fld = "acid", val = id, db = db)
    if (lvl <= 1) tcplDelete(tbl = "mc1", fld = "acid", val = id, db = db)
    if (lvl <= 2) tcplDelete(tbl = "mc2", fld = "acid", val = id, db = db)
    if (lvl <  3) {
      id <- suppressWarnings(try(tcplLoadAeid("acid", id)$aeid, silent = TRUE))
    }
    if (is(id, "try-error")) return(TRUE)
    if (lvl <= 3) tcplDelete(tbl = "mc3", fld = "aeid", val = id, db = db)
    if (lvl <= 4) tcplDelete(tbl = "mc4", fld = "aeid", val = id, db = db)
    if (lvl <= 4) tcplDelete(tbl = "mc4_agg", fld = "aeid", val = id, db = db)
    if (lvl <= 5) tcplDelete(tbl = "mc5", fld = "aeid", val = id, db = db)
    if (lvl <= 6) tcplDelete(tbl = "mc6", fld = "aeid", val = id, db = db)
    
  }
  
  if (type == "sc") {
    
    if (lvl == 0) tcplDelete(tbl = "sc0", fld = "acid", val = id, db = db)
    if (lvl <  1) {
      id <- suppressWarnings(try(tcplLoadAeid("acid", id)$aeid, silent = TRUE))
    }
    if (is(id, "try-error")) return(TRUE)
    if (lvl <= 1) tcplDelete(tbl = "sc1", fld = "aeid", val = id, db = db)
    if (lvl <= 2) tcplDelete(tbl = "sc2", fld = "aeid", val = id, db = db)
    if (lvl <= 2) tcplDelete(tbl = "sc2_agg", fld = "aeid", val = id, db = db)
    
  }
  
  ttime <- round(difftime(Sys.time(), stime, units = "sec"), 2)
  ttime <- paste(unclass(ttime), units(ttime))
  cat("Completed delete cascade for ", length(id), " ids (", ttime, 
      ")\n", sep = "")
  
}

#-------------------------------------------------------------------------------
