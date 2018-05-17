#' @name Register/update annotation
#' @rdname rgstr_funcs
#' @title Functions for registering & updating annotation information
#'
#' @description 
#' These functions are used to register and update the chemical and assay 
#' annotation information.
#' 
#' @param what Character of length 1, the name of the ID to register or update
#' @param flds Named list, the other fields and their values
#' @param id Integer, the ID value(s) to update
#' 
#' 
#' @details
#' These functions are used to populate the tcpl database with the necessary
#' annotation information to complete the processing. As shown in the package
#' vignette, the package requires some information about the samples and assays
#' before data can be loaded into the tcpl database. 
#' 
#' Depending on what is being registered, different information is required. 
#' The following table lists the fields that can be registered/updated by these
#' functions, and the minimal fields required for registering a new ID. (The
#' database table affected is in parentheses.) 
#' 
#' \itemize{
#'  \item asid (assay_source): assay_source_name
#'  \item aid (assay): asid, assay_name, assay_footprint
#'  \item acid (assay_component): aid, assay_component_name
#'  \item aeid (assay_component_endpoint): acid, assay_component_endpoint_name,
#'  normalized_data_type
#'  \item acsn (assay_component_map): acid, acsn
#'  \item spid (sample): spid, chid
#'  \item chid (chemical): chid, casn
#'  \item clib (chemical_library): chid, clib
#' } 
#' 
#' Note: The functions accept the abbreviated forms of the names, ie. "aenm" 
#' rather than the full "assay_component_endpoint_name." More information about
#' the registration process and all of the fields is available in the vignette. 
#' 
#' @examples
#' 
#' \dontrun{
#' ## Store the current config settings, so they can be reloaded at the end 
#' ## of the examples
#' conf_store <- tcplConfList()
#' tcplConfDefault()
#' 
#' ## Load current ASID information
#' tcplLoadAsid()
#' 
#' ## Register a new assay source
#' tcplRegister(what = "asid", flds = list(asnm = "example_asid"))
#' 
#' ## Show the newly registered ASID
#' tcplLoadAsid(add.fld = "assay_source_desc")
#' 
#' ## Notice that the newly created ASID does not have an assay_source_desc.
#' ## The field could have been defined during the registration process, but
#' ## can also be updated using tcplUpdate
#' i1 <- tcplLoadAsid()[asnm == "example_asid", asid]
#' tcplUpdate(what = "asid", 
#'            id = i1, 
#'            flds = list(assay_source_desc = "example asid description"))
#' tcplLoadAsid(add.fld = "assay_source_desc")
#' 
#' ## Remove the created ASID. Note: Manually deleting primary keys can cause
#' ## serious database problems and should not generally be done. 
#' tcplSendQuery(paste0("DELETE FROM assay_source WHERE asid = ", i1, ";"))
#' 
#' ## Reset configuration
#' options(conf_store)
#' }
#' 
NULL