#-------------------------------------------------------------------------------
# tcplSendQuery: Send query to the tcpl databases
#-------------------------------------------------------------------------------

#' @param tbl Tables to be read queried
#' @param delete Logical of length 1, execute delete on queried table
#'
#' @rdname query_funcs
#' 
#' @import DBI
#' @import data.table
#' @importFrom RMariaDB MariaDB
#' @importFrom methods is
#' @importFrom sqldf sqldf
#' @export

tcplSendQuery <- function(query, db = getOption("TCPL_DB"), 
                          drvr = getOption("TCPL_DRVR"), tbl=NULL, delete=F) {
  
  #Check for valid inputs
  if (getOption("TCPL_DRVR") == "API") {
    stop("'API' driver not supported in tcplSendQuery.")
  }
  if (length(query) != 1 || !is(query,"character")) {
    stop("The input 'query' must be a character of length one.")
  }
  if (length(db) != 1 || !is(db,"character")) {
    stop("The input 'db' must be a character of length one.")
  }
  
  db_pars <- NULL
  
  
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
    
  }
  
  if (is.null(db_pars)) {
    
    stop(getOption("TCPL_DRVR"), " is not a supported database system. See ",
         "?tcplConf for more details.")
    
  }
  
  if (drvr == 'MySQL') {
    if("RMySQL" %in% loadedNamespaces()){
      unloadNamespace("RMySQL")
      warning("'RMySQL' package is not supported with tcpl and has been detached.")
    }
    dbcon <- do.call(dbConnect, db_pars)
    temp <- try(dbSendQuery(dbcon, query), silent = TRUE)
    if (!is(temp, "try-error")) dbClearResult(temp)
    on.exit(dbDisconnect(dbcon), add = TRUE)
  }
  
  if (!is(temp, "try-error")) return(TRUE)
  
  temp[1]
  
}

#-------------------------------------------------------------------------------
