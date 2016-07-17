#-------------------------------------------------------------------------------
# tcplUpdate: Update assay or chemical information
#-------------------------------------------------------------------------------

#' @rdname rgstr_funcs
#' 
#' @import data.table
#' @export


tcplUpdate <- function(what, id, flds) {
  
  lens <- c(length(id), sapply(flds, length))
  if (!all.equal(max(lens), min(lens))) {
    stop("The length of 'id' and the lengths of each list element in 'flds'",
         " must be equal.")
  }
  
  i <- switch(what,
              asid = "assay_source",
              aid  = "assay",
              acid = "assay_component",
              aeid = "assay_component_endpoint",
              acsn = "assay_component_map",
              spid = "sample",
              chid = "chemical",
              clib = "chemical_library")
  
  if (is.null(i)) stop("Not a valid 'what' input.")
  
  pot_flds <- tcplListFlds(tbl = i[[1]], db = getOption("TCPL_DB"))
  flds <- as.data.table(flds)
  setnames(flds, .convertNames(names(flds)))
  
  if (any(!names(flds) %in% pot_flds)) {
    warning("Some of the given fields are not in the ", i[[1]], " table. ",
            "Extra fields will be ignored.")
    xtra <- names(flds)[!names(flds) %in% pot_flds]
    flds[ , c(xtra) := NULL]
  }
    
  qf <- paste("UPDATE", i, "%s", "WHERE", what, "=", id)
  
  for (i in 1:nrow(flds)) {
    inst <- paste0(names(flds), " = ", "\"", flds[i], "\"", collapse = ", ")
    inst <- paste("SET", inst)
    qf[i] <- sprintf(qf[i], inst)
  }
  
  res <- lapply(qf, tcplSendQuery)
  
  test <- !sapply(res, isTRUE)
  if (any(test)) {
    warning("Error updating the following ids: ", 
            paste(id[test], collapse = ", "))
  }
  
  TRUE
  
}