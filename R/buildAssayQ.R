#-------------------------------------------------------------------------------
# buildAssayQ: Generate query for assay information
#-------------------------------------------------------------------------------

#' @title Generate query for assay information
#' 
#' @description 
#' \code{.buildAssayQ} generates a query string to load assay information
#' 
#' @param out Character, the default fields to include
#' @param tblo Integer, the order to send the fields to prepOutput
#' @param fld Character, the field(s) to query/subset on
#' @param val List, vectors of values for each field to query/subset on. Must 
#' be in the same order as 'fld'.
#' @param add.fld Character, additional field(s) to include, but not query/
#' subset on
#' 
#' @return A character containing the query to send to tcplQuery
#' 
#' @import data.table

.buildAssayQ <- function(out, tblo, fld = NULL, val = NULL, add.fld = NULL) {
  
  tbls <- c("assay_source", "assay", "assay_component", "assay_component_map", 
            "assay_component", "assay_component_endpoint")
  
  tblo <- tbls[tblo]
  
  fld     <- .convertNames(fld)
  add.fld <- .convertNames(add.fld)
  
  fld <- .prepField(fld = fld, tbl = tblo, db = getOption("TCPL_DB"))
  add.fld <- .prepField(fld = add.fld, tbl = tblo, db = getOption("TCPL_DB"))
  afld <- c(fld, 
            out,
            add.fld)
  afld <- afld[!duplicated(afld)]
  
  atbl <- unique(sapply(strsplit(afld, "[:.:]"), "[[", 1))
  tbls <- unique(tbls[min(match(atbl, tbls)):max(match(atbl, tbls))])
  if (!any(grepl("map", afld))) tbls <- tbls[tbls != "assay_component_map"]
  
  if (length(tbls) > 1) {
    
    tbl_link <- character()
    if (all(c("assay_source", "assay") %in% tbls)) {
      tbl_link <- c(tbl_link, "assay_source.asid = assay.asid")
    }
    if (all(c("assay", "assay_component") %in% tbls)) {
      tbl_link <- c(tbl_link, "assay.aid = assay_component.aid")
    }
    if (all(c("assay_component", "assay_component_map") %in% tbls)) {
      tbl_link <- c(tbl_link, "assay_component.acid = assay_component_map.acid")
    }
    if (all(c("assay_component", "assay_component_endpoint") %in% tbls)) {
      tbl_link <- c(tbl_link, paste0("assay_component.acid = ",
                                     "assay_component_endpoint.acid"))
    }
    
  } else {
    
    tbl_link <- character()
    
  }
  
  
  qformat <- paste("SELECT",
                   paste(afld, collapse = ", "),
                   "FROM",
                   paste(tbls, collapse = ", "),
                   if (length(tbl_link) > 0) "WHERE" else "",
                   paste(tbl_link, collapse = " AND "))
  
  if (length(fld) > 0) {
    
    qformat <- paste(qformat, if (length(tbl_link) > 0) "AND" else "WHERE")
    
    qformat <- paste0(qformat, 
                      "  ", 
                      paste(fld, "IN (%s)", collapse = " AND "))
    qformat <- paste0(qformat, ";")
    
    if (!is.list(val)) val <- list(val)
    val <- lapply(val, function(x) paste0("\"", x, "\"", collapse = ","))
    
    qstring <- do.call(sprintf, args = c(qformat, val))
    
  } else {
    
    qstring <- qformat
    
  }
  
  qstring <- sub("assay_source_name", "assay_source_name AS asnm", qstring)
  qstring <- sub("assay_name", "assay_name AS anm", qstring)
  qstring <- sub("assay_component_name", 
                 "assay_component_name AS acnm", 
                 qstring)
  qstring <- sub("assay_component_endpoint_name", 
                 "assay_component_endpoint_name AS aenm", 
                 qstring)
  
  qstring
  
}

#-------------------------------------------------------------------------------