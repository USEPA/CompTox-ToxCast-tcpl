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
#' @param lvl Usually Integer to indicate what level to auto-increment
#' 
#' @note
#' This function is not exported and not intended to be used by the user.
#' 
#' @import DBI
#' @import data.table
#' @importFrom RMariaDB MariaDB
#' @importFrom utils read.csv read.table tail write.table

tcplAppend <- function(dat, tbl, db, lvl=NULL) {
  
  ## Variable-binding to pass R CMD Check
  created_date <- modified_date <- NULL
  
  db_pars <- NULL
  
  if (getOption("TCPL_DRVR") == "API") {
    stop("'API' driver not supported in tcplAppend.")
  }
  
  if (getOption("TCPL_DRVR") == "MySQL") {
    
    if (any(is.na(options()[c("TCPL_USER", "TCPL_HOST", "TCPL_PASS")]))) {
      stop("Must configure TCPL_USER, TCPL_HOST, and TCPL_PASS options. See ",
           "?tcplConf for more details.")
    }
    
    db_pars <- list(drv = RMariaDB::MariaDB(),
                    user = getOption("TCPL_USER"),
                    password = getOption("TCPL_PASS"),
                    host = getOption("TCPL_HOST"),
                    dbname = db,
                    bigint = "numeric")
    additional_pars <- .Options[grepl("TCPL_(?!USER|HOST|DB|DRVR|HOST|PASS)",names(.Options),perl = TRUE)]
    names(additional_pars) <- tolower(gsub("TCPL_","",names(additional_pars)))
    db_pars <- append(db_pars,additional_pars)
    
    if("RMySQL" %in% loadedNamespaces()){
      unloadNamespace("RMySQL")
      warning("'RMySQL' package is not supported with tcpl and has been detached.")
    }
    
    dbcon <- do.call(dbConnect, db_pars)

    
    dbWriteTable(conn = dbcon, 
                 name = tbl, 
                 value = dat, 
                 row.names = FALSE, 
                 append = TRUE)
    
    on.exit(dbDisconnect(dbcon), add = TRUE)
    
    return(TRUE)
    
  }
  
  if (is.null(db_pars)) {
    
    stop(getOption("TCPL_DRVR"), " is not a supported database system. See ",
         "?tcplConf for more details.")
    
  }
    
}

#-------------------------------------------------------------------------------
