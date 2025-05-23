#' @name Query functions
#' @rdname query_funcs
#' @title Wrappers for sending queries and fetching results
#'
#' @description 
#' These functions send a query to the given database, and are the access point
#' for all tcpl functions that query or update the tcpl database.
#' 
#' @param query Character of length 1, the query string
#' @inheritParams tcplConf
#' 
#' @details
#' Currently, the tcpl package supports the "MySQL", "example", and "API"
#' database drivers.
#' 
#' \code{tcplQuery} returns a data.table object with the query results.
#' \code{tcplSendQuery} sends a query, but does not fetch any results, and 
#' returns 'TRUE' or the error message given by the database. 
#' \code{tcplQueryAPI} returns a data.table object with the query results when
#' connected using "API" as driver.
#' 
#' @examples
#' \dontrun{
#' # only with MySQL driver
#' tcplQuery("SELECT 'Hello World';")
#' 
#' # only with API driver
#' tcplConfDefault()
#' tcplQueryAPI(resource = "data", fld = "aeid", val = 2)
#' }
#' 
NULL