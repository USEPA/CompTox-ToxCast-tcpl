#--------------------------------------------------------------
# Get Assay component endpoint ids (aeid) by matching assay component endpoint name(aenm)
#--------------------------------------------------------------

#' @title get Aeid for endpoint name
#' 
#' @description 
#' \code{tcplGetAeid} takes a string(name) and finds the assay component endpoint names that match the string and the 
#' aeids associated with those names.The function performs a regular expression like matching for strings in the 
#' assay component endpoint name column in the assay component endpoint table. 
#' 
#' @param name A string that will be matched to the assay component endpoint name
#' 
#' @examples
#' ## Store the current config settings, so they can be reloaded at the end 
#' ## of the examples 
#' conf_store <- tcplConfList()
#' tcplConfExample()
#' 
#' ## Search for aenm (assay name) case insensitive
#' tcplGetAeid("TOX21")
#' tcplGetAeid("tox21")
#' 
#' ## Reset configuration
#' options(conf_store)
#' 
#' @export

tcplGetAeid <- function(name){
  # get all the aeid and aenm from the table
  query <- "Select aeid,assay_component_endpoint_name FROM assay_component_endpoint;"
  all_aeid_tble <- tcplQuery(query,tbl = "assay_component_endpoint")
  selected_aenm <- grep(name,all_aeid_tble$assay_component_endpoint_name,ignore.case = T,value =T)
  selected_aeid <- all_aeid_tble$aeid[grep(name,
                                           all_aeid_tble$assay_component_endpoint_name,
                                           ignore.case = T,
                                           value =F)
                                      ]
  selected_aeid_tble <- data.frame("aeid"=selected_aeid,"aenm"=selected_aenm,stringsAsFactors = F)
  return(selected_aeid_tble)
}