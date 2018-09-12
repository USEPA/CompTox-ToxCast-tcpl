#-------------------------------------------------------------------------------
# .clearSQLite: clear out the sqlite db so it can be regenerated from scratch
#-------------------------------------------------------------------------------

.clearSQLite <- function(db) {
  
  stopifnot(options()$TCPL_DRVR == "SQLite")
  
  tbls <- tcplQuery("SELECT name FROM sqlite_master WHERE type='table';")
  tbls <- unname(unlist(tbls))
  keep <- c(grep("_methods", tbls, value = TRUE), "mc5_fit_categories")
  reset_qs <- paste0("DELETE FROM ", tbls[!tbls %in% keep], ";")
  sapply(reset_qs, tcplSendQuery)
  
}

#-------------------------------------------------------------------------------
