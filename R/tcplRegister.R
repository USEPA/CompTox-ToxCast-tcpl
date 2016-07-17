#-------------------------------------------------------------------------------
# tcplRegister: Register new assay or chemical information
#-------------------------------------------------------------------------------

#' @rdname rgstr_funcs
#' 
#' @import data.table
#' @export


tcplRegister <- function(what, flds) {
  
  i <- switch(what,
              asid = list("assay_source", "assay_source_name"),
              aid  = list("assay", c("asid", "assay_name", "assay_footprint")),
              acid = list("assay_component", c("aid", "assay_component_name")),
              aeid = list("assay_component_endpoint",
                          c("acid", 
                            "assay_component_endpoint_name", 
                            "normalized_data_type")),
              acsn = list("assay_component_map", c("acid", "acsn")),
              spid = list("sample", c("spid", "chid")),
              chid = list("chemical", c("chnm", "casn")),
              clib = list("chemical_library", c("chid", "clib")))
  
  if (is.null(i)) stop("Not a valid 'what' input.")
  
  pot_flds <- tcplListFlds(tbl = i[[1]], db = getOption("TCPL_DB"))
  flds <- as.data.table(flds)
  setnames(flds, .convertNames(names(flds)))
  
  if (any(!i[[2]] %in% names(flds))) {
    stop("Missing required fields for registering a(n) ", what, 
         ". See ?tcplRegister")
  }
  
  if (any(!names(flds) %in% pot_flds)) {
    warning("Some of the given fields are not in the ", i[[1]], " table. ",
            "Extra fields will be ignored.")
    xtra <- names(flds)[!names(flds) %in% pot_flds]
    flds[ , c(xtra) := NULL]
  }
  
  tcplAppend(dat = flds, tbl = i[[1]], db = getOption("TCPL_DB"))
  
  TRUE
  
}