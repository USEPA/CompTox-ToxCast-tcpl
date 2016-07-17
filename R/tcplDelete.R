#-------------------------------------------------------------------------------
# tcplDelete: Delete rows from tcpl databases
#-------------------------------------------------------------------------------

#' @title Delete rows from tcpl databases
#' 
#' @description
#' \code{tcplDelete} deletes rows from the given table and database.
#' 
#' @param tbl Character, length 1, the table to delete from
#' @param fld Character, the field(s) to query on
#' @param val   List, vectors of values for each field to query on. Must be in 
#'              the same order as 'fld'.
#' @param db Character, the database containing the table
#' 
#' @note
#' This function is not exported and not intended to be used by the user.
#' 
#' @seealso \code{\link{tcplSendQuery}}
#' 
#' @import data.table

tcplDelete <- function(tbl, fld, val, db) {
  
  # Check for valid inputs
  if (length(tbl) != 1 | class(tbl) != "character") {
    stop("The input 'tbl' must be a character of length one.")
  }
  
  qformat <- paste("DELETE FROM", tbl, "WHERE")
  
  qformat <- paste0(qformat, "  ", paste(fld, "IN (%s)", collapse = " AND "))
  qformat <- paste0(qformat, ";")
  
  if (!is.list(val)) val <- list(val)
  val <- lapply(val, function(x) paste0("\"", x, "\"", collapse = ","))
  
  qstring <- do.call(sprintf, args = c(qformat, val))
  
  tcplSendQuery(query = qstring, db = db)
  
}

#-------------------------------------------------------------------------------
