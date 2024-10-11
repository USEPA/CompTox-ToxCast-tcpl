#-------------------------------------------------------------------------------
# tcplQuery: Query the tcpl databases
#-------------------------------------------------------------------------------

#' @rdname query_funcs
#' 
#' @import DBI
#' @import data.table
#' @importFrom RMariaDB MariaDB
#' @importFrom sqldf sqldf
#' @export


tcplQuery <- function(query, db = getOption("TCPL_DB"), 
                      drvr = getOption("TCPL_DRVR"), tbl=NULL) {
  if (is.null(db)) db <- getOption("TCPL_DB")
  if (is.null(drvr)) drvr <- getOption("TCPL_DRVR")
  
  #Check for valid inputs
  if (drvr == "API") {
    stop("'API' driver not supported in tcplQuery.")
  }
  if (length(query) != 1 || !is(query, "character")) {
    stop("The input 'query' must be a character of length one.")
  }
  if (length(db) != 1 || !is(db,"character")) {
    stop("The input 'db' must be a character of length one.")
  }
  
  db_pars <- NULL
  
  if (drvr == "MySQL") {
    
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
    result <- dbGetQuery(dbcon, query)
    
    on.exit(dbDisconnect(dbcon), add = TRUE)
    
    result <- as.data.table(result)
  }
  
  result[]
  
}

#-------------------------------------------------------------------------------
