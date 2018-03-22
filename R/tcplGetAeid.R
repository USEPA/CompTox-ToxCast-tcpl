#--------------------------------------------------------------
# Get Assay component endpoint ids (aeid) by matching assay component endpoint name(aenm)
#--------------------------------------------------------------

#' @title get Aeid for endpoint name
#' 
#' @description 
#' \code{tcplGetAeid} takes a string(name) and finds the assay component endpoint names that match the string and the 
#' aeids associated with those names.The function perfomes a regular expression like matching for strings in the 
#' assay component endpoint name column in the assay component endpoint table. 
#' 
#' @param name A string that will be matched to the assay component endpoint name
#' @export

tcplGetAeid <- function(name){
  # get all the aeid and aenm from the table
  query <- "Select aeid,assay_component_endpoint_name FROM assay_component_endpoint;"
}