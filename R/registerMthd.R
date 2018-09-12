#-------------------------------------------------------------------------------
# registerMthd: Add new analysis method 
#-------------------------------------------------------------------------------

#' @title Add a new analysis method
#' 
#' @description
#' \code{registerMthd} registers a new analysis method to the tcpl 
#' databases.
#' 
#' @param lvl Integer of length 1, the level for the analysis method 
#' @param mthd Character, the name of the method
#' @param desc Character, same length as mthd, the method description
#' @param nddr Integer, 0 or 1, 1 if the method requires loading the dose-
#'             response data
#' @param type Character of length 1, the data type, "sc" or "mc"
#' 
#' @details
#' 'mthd' must match a corresponding function name in the functions that load
#' the methods, ie. \code{mc2_mthds}. 'nddr' only applies to level 6 
#' methods.
#' 
#' @import data.table

registerMthd <- function(lvl, mthd, desc, nddr = 0L, type) {
  
  if (length(mthd) != length(desc)) {
    stop("length of mthd must equal length of desc.")
  }
  
  if (length(lvl) > 1) stop("'lvl' must be an integer of length 1.")
  if (!type %in% c("mc", "sc")) stop("Invalid 'type' value.")
  if (type == "mc" & !lvl %in% c(2, 3, 5, 6)) stop("Invalid 'lvl' value.")
  if (type == "sc" & !lvl %in% 1:2) stop("Invalid 'lvl' value.")
  
  mb <- paste(Sys.info()[c("login", "user", "effective_user")], collapse = ".")
  
  dat <- data.table(modified_by = rep(mb, length(mthd)))
  dat[ , (c(paste0(type, lvl, "_mthd"), "desc")) := list(mthd, desc)]
  
  if (lvl == 6L) dat[ , ("nddr") := nddr]
  
  tcplAppend(dat = dat, 
             tbl = paste0(type, lvl, "_methods"), 
             db = getOption("TCPL_DB"))
  
}

#-------------------------------------------------------------------------------
