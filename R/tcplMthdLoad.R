#-------------------------------------------------------------------------------
# tcpltcplMthdLoad: Load analysis method(s) 
#-------------------------------------------------------------------------------

#' @rdname mthd_funcs
#' @export

tcplMthdLoad <- function(lvl, id = NULL, type = "mc") {
  
  if (length(lvl) > 1) stop("'lvl' must be an integer of length 1.")
  if (!type %in% c("mc", "sc")) stop("Invalid 'type' value.")
  if (type == "mc" & !lvl %in% c(2, 3, 5, 6)) stop("Invalid 'lvl' value.")
  if (type == "sc" & !lvl %in% 1:2) stop("Invalid 'lvl' value.")
    
  id_name <- if (type == "mc" & lvl == 2) "acid" else "aeid" 
  flds <- c(id_name, 
            "b.%s_mthd AS mthd",
            "b.%s_mthd_id AS mthd_id")
  if ((lvl < 4 & type == "mc") | (lvl == 1 & type == "sc")) {
    flds <- c(flds, "a.exec_ordr AS ordr")
  }
  if (lvl == 6) flds <- c(flds, "nddr")
  
  tbls <- c(paste0("%s_", id_name, " AS a"), "%s_methods AS b")
  
  qformat <- paste("SELECT", paste(flds, collapse = ","),
                   "FROM", paste(tbls, collapse = ","),
                   "WHERE a.%s_mthd_id = b.%s_mthd_id")
  qformat <- gsub("%s", paste0(type, lvl), qformat)
  
  if (!is.null(id)) {
    qformat <- paste(qformat, "AND", id_name, "IN (%s)")
    qformat <- sprintf(qformat, paste(id, collapse = ","))
  }
    
  if ((lvl < 4 & type == "mc") | (lvl == 1 & type == "sc")) {
    qstring <- paste0(qformat, " ORDER BY ", id_name, ", a.exec_ordr")
  } else {
    qstring <- qformat
  }    
  
  dat <- tcplQuery(query = qstring, db = getOption("TCPL_DB"))
  
  if (nrow(dat) == 0) {
    warning("The given id(s) do not have ", type, lvl, " methods.")
    return(dat[])
  }
  
  len_miss <- lw(!id %in% dat[[id_name]])
  if (len_miss > 0) {
    warning(len_miss, " of the given ids do not have ", type, lvl, " methods.")
  }
  
  dat[]
    
}

#-------------------------------------------------------------------------------
