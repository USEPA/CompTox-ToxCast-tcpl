#' @name <%= type %><%= LVL %>
#' @title Perform level <%= LVL %>  
#' <%= if (type == "mc") "multiple" else "single" %>-concentration processing
#' 
#' @description
#' \code{<%= type %><%= LVL %>} loads level <%= LVL - 1 %> data from the tcpl 
#' database for the given id and performs level <%= LVL %> 
#' <%= if (type == "mc") "multiple" else "single" %>-concentration processing. 
#' The processed data is then loaded into the <%= type %><%= LVL %> table and 
#' all subsequent data is deleted with \code{\link{tcplCascade}}. See details 
#' for more information.
#' 
#' The individual processing functions are no longer exported, as it is 
#' typically more convenient and suggested to use the \code{\link{tcplRun}} 
#' wrapper function. 
#' 
#' @return A boolean of length 1, indicating the success of the processing, or 
#' when 'wr' is FALSE, a list where the first element is a boolean indiciating 
#' the success of processing and the second element is a data.table containing 
#' the processed data
#' 
#' @family <%= if (type == "mc") "multiple" else "single" %>-concentration data 
#' processing functions
