#' @title Load level <%= LVL %> data
#' 
#' @description 
#' \code{<%= paste0("loadLvl", LVL, "Data") %>} queries the ToxCast databases 
#' and returns a data.table with level <%= LVL %> data.
#' 
#' @param field Character, the field(s) to query on
#' @param val List, vectors of values for each field to query on. Must be in 
#'            the same order as field. 
#' 
#' @details Leaving \code{field} and \code{vals} will return all data. 
#' 
#' @return A data.table containing level <%= LVL %> data for the given fields.
#' 
#' @seealso \code{\link{tcplQuery}}, \code{\link{data.table}}
#' 
#' @family data loading functions