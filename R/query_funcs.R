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
#' Currently, the tcpl package only supports the "MySQL" and "SQLite" database
#' drivers.
#' 
#' \code{tcplQuery} returns a data.table object with the query results.
#' \code{tcplSendQuery} sends a query, but does not fetch any results, and 
#' returns 'TRUE' or the error message given by the database. 
#' 
#' @examples
#' 
#' ## Store the current config settings, so they can be reloaded at the end 
#' ## of the examples
#' conf_store <- tcplConfList()
#' tcplConfDefault()
#' 
#' tcplQuery("SELECT 'Hello World';")
#' tcplQuery("SELECT * FROM assay;")
#'  
#' ## Reset configuration
#' options(conf_store)
#' 
NULL