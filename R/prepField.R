#-------------------------------------------------------------------------------
# prepField: Paste appropriate table name to field name
#-------------------------------------------------------------------------------

#' @title Paste appropriate table name to field name
#' 
#' @description Paste appropriate table name to field name
#' 
#' @param fld Character, the table fields
#' @param tbl Character, the possible tables
#' @param db Character, the database containing the tables
#' 
#' @details
#' The function loops through the given tables, and for each field i it assigns
#' the last table containing i to i. ORDER OF FLD MATTERS!!
#' 
#' @import data.table

.prepField <- function(fld, tbl, db) {
  
  tbl_flds <- lapply(tbl, tcplListFlds, db = db)
  pre <- rep(NA_character_, length(fld))
  
  for (i in seq_along(tbl)) {
    
    pre[fld %in% tbl_flds[[i]]] <- tbl[i]
    
  }
  
  if (any(is.na(pre))) stop("Not all given fields available in query.")
  
  paste(pre, fld, sep = ".")
  
}

#-------------------------------------------------------------------------------
