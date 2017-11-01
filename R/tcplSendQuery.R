#-------------------------------------------------------------------------------
# tcplSendQuery: Send query to the tcpl databases
#-------------------------------------------------------------------------------

#' @rdname query_funcs
#' 
#' @import DBI
#' @importFrom RSQLite SQLite 
#' @importMethodsFrom RSQLite dbSendQuery dbClearResult dbDisconnect dbConnect
#' @import data.table
#' @importFrom RMySQL MySQL
#' @importMethodsFrom RMySQL dbSendQuery dbClearResult dbDisconnect dbConnect
#' @importFrom methods is
#' @importFrom sqldf sqldf
#' @export

tcplSendQuery <- function(query, db = getOption("TCPL_DB"), 
                          drvr = getOption("TCPL_DRVR"), tbl=NULL, delete=F) {
  
  #Check for valid inputs
  if (length(query) != 1 | class(query) != "character") {
    stop("The input 'query' must be a character of length one.")
  }
  if (length(db) != 1 | class(db) != "character") {
    stop("The input 'db' must be a character of length one.")
  }
  
  db_pars <- NULL
  
  if (getOption("TCPL_DRVR") == "SQLite") {
    
    db_pars <- list(drv = SQLite(),
                    dbname = db)
    
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
    
  }
  
  if (getOption("TCPL_DRVR") == "tcplLite") {
    db_pars <- "Just running tcplLite, we're OK"
    
    for (t in tbl) {
      fpath <- paste(db, t, sep='/')
      fpath <- paste(fpath, 'csv', sep='.')
      assign(t, read.table(fpath, header=T, sep=','))
    }

    temp <- as.data.table(sqldf(query, stringsAsFactors=F))
    
    if (delete == T) {
      if (length(tbl) > 1) {
        stop("Can't execute delete on more that one table")
      }
      db_pars <- db
      fpath <- paste(db, tbl, sep='/')
      fpath <- paste(fpath, 'csv', sep='.')
      write.table(temp, file=fpath, append=F, row.names=F, sep=',', col.names=T) # Need to rewrite whole table
    }
  }
  
  if (is.null(db_pars)) {
    
    stop(getOption("TCPL_DRVR"), " is not a supported database system. See ",
         "?tcplConf for more details.")
    
  }
  
  if ((drvr == 'SQLite') | (drvr == 'MySQL')) {
    dbcon <- do.call(dbConnect, db_pars)
    temp <- try(dbSendQuery(dbcon, query), silent = TRUE)
    if (!is(temp, "try-error")) dbClearResult(temp)
    dbDisconnect(dbcon)
  }
  
  if (!is(temp, "try-error")) return(TRUE)
  
  temp[1]
  
}

#-------------------------------------------------------------------------------
