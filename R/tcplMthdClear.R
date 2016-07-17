#-------------------------------------------------------------------------------
# tcplMthdClear: Clear analysis method(s) 
#-------------------------------------------------------------------------------

#' @rdname mthd_funcs
#' @export

tcplMthdClear <- function(lvl, id, mthd_id = NULL, type) {
  
  if (length(lvl) > 1) stop("'lvl' must be an integer of length 1.")
  if (!type %in% c("mc", "sc")) stop("Invalid 'type' value.")
  if (type == "mc" & !lvl %in% c(2, 3, 5, 6)) stop("Invalid 'lvl' value.")
  if (type == "sc" & !lvl %in% 1:2) stop("Invalid 'lvl' value.")
  
  fld <- if (type == "mc" & lvl == 2) "acid" else "aeid"
  tbl <- paste0(type, lvl, "_", fld)
  if (!is.null(mthd_id)) {
    fld <- c(fld, sprintf("%s%s_mthd_id", type, lvl))
    val <- list(id, mthd_id)
  } else {
    val <- id
  }
  
  tcplDelete(tbl = tbl, fld = fld, val = val, db = getOption("TCPL_DB"))
  
  tcplCascade(lvl = lvl, type = type, id = id)
  
}

#-------------------------------------------------------------------------------
