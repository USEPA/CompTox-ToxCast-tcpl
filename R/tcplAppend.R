#-------------------------------------------------------------------------------
# tcplAppend: Append rows to a table
#-------------------------------------------------------------------------------

#' @title Append rows to a table
#' 
#' @description
#' \code{tcplAppend} takes a data.table (dat) and appends the data.table into 
#' a database table. 
#' 
#' @param dat data.table, the data to append to a table
#' @param tbl Character of length 1, the table to append to
#' @param db Character of length 1, the database containing \code{tbl}
#' 
#' @note
#' This function is not exported and not intended to be used by the user.
#' 
#' @import DBI
#' @importFrom RSQLite SQLite
#' @import data.table
#' @importFrom RMySQL MySQL

tcplAppend <- function(dat, tbl, db) {
  
  ## Variable-binding to pass R CMD Check
  created_date <- modified_date <- NULL
  
  db_pars <- NULL
  
  if (getOption("TCPL_DRVR") == "SQLite") {
    
    tbl_flds <- tcplListFlds(tbl, db)
    if ("created_date" %in% tbl_flds) dat[ , created_date := Sys.time()]
    if ("modified_date" %in% tbl_flds) dat[ , modified_date := Sys.time()]
        
    db_pars <- list(drv = SQLite(),
                    dbname = db)
    
    dbcon <- do.call(dbConnect, db_pars)
    
    tempTbl <- "temp_table"
    if (dbExistsTable(dbcon, tempTbl)) dbRemoveTable(dbcon, tempTbl)
    dbWriteTable(conn = dbcon, 
                 name = tempTbl, 
                 value = dat, 
                 row.names = FALSE, 
                 append = FALSE)
    
    # Add any columns to input data.frame that are in target table, then merge
    tmp_flds <- names(dat)
    status <- dbSendQuery(dbcon, 
                          paste("INSERT INTO", tbl, 
                                "(", paste(tmp_flds, collapse = ","), ")",
                                "SELECT",
                                paste(tmp_flds, collapse = ","),
                                "FROM",
                                tempTbl))
    dbClearResult(status)
    
    # Remove temporary table
    dbRemoveTable(dbcon, tempTbl)
    
    dbDisconnect(dbcon)
        
  }
  
  if (getOption("TCPL_DRVR") == "MySQL") {
    
    if (any(is.na(options()[c("TCPL_USER", "TCPL_HOST", "TCPL_PASS")]))) {
      stop("Must configure TCPL_USER, TCPL_HOST, and TCPL_PASS options. See ",
           "?tcplConf for more details.")
    }
    
    db_pars <- list(drv = RMySQL::MySQL(),
                    user = getOption("TCPL_USER"),
                    password = getOption("TCPL_PASS"),
                    host = getOption("TCPL_HOST"),
                    dbname = db)
    
    dbcon <- do.call(dbConnect, db_pars)
    
    dbWriteTable(conn = dbcon, 
                 name = tbl, 
                 value = dat, 
                 row.names = FALSE, 
                 append = TRUE)
    
    dbDisconnect(dbcon)
    
    return(TRUE)
    
  }
  
  if (is.null(db_pars)) {
    
    stop(getOption("TCPL_DRVR"), " is not a supported database system. See ",
         "?tcplConf for more details.")
    
  }
    
}

#-------------------------------------------------------------------------------
