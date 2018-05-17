#-------------------------------------------------------------------------------
# tcplMthdAssign: Assign analysis method 
#-------------------------------------------------------------------------------

#' @rdname mthd_funcs
#' @export

tcplMthdAssign <- function(lvl, id, mthd_id, ordr = NULL, type) {
  
  ## Variable-binding to pass R CMD Check
  exec_ordr <- modified_by <- NULL
  
  if (length(lvl) > 1) stop("'lvl' must be an integer of length 1.")
  if (!type %in% c("mc", "sc")) stop("Invalid 'type' value.")
  if (type == "mc" & !lvl %in% c(2, 3, 5, 6)) stop("Invalid 'lvl' value.")
  if (type == "sc" & !lvl %in% 1:2) stop("Invalid 'lvl' value.")
  
  id_name <- if (type == "mc" & lvl == 2) "acid" else "aeid" 
  flds <- c(id_name, sprintf("%s%s_mthd_id", type, lvl))
  
  dat <- expand.grid(id = id, 
                     mthd = mthd_id, 
                     stringsAsFactors = FALSE)
  dat <- as.data.table(dat)
  
  if ((lvl < 4 & type == "mc") | (lvl == 1 & type == "sc")) {
    
    if (is.null(ordr) | length(mthd_id) != length(ordr)) {
      stop("'ordr' must be specified and the same length as 'mthd_id'")
    }
    
    dat[ , exec_ordr := ordr[match(get("mthd"), mthd_id)]]
        
  }
  
  setnames(dat, old = c("id", "mthd"), flds)
  
  mb <- paste(Sys.info()[c("login", "user", "effective_user")], collapse = ".")
  dat[ , modified_by := mb]
  
  tcplAppend(dat = dat, 
             tbl = paste0(type, lvl, "_", flds[1]), 
             db = getOption("TCPL_DB"))
  
  tcplCascade(lvl = lvl, type = type, id = id)
  
}

#-------------------------------------------------------------------------------
