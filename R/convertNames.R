#-------------------------------------------------------------------------------
# convertNames: Convert assay names to their abbreviations
#-------------------------------------------------------------------------------

#' @title Convert assay names to their abbreviations
#' 
#' @description 
#' \code{.convertNames} converts the assay names as they appear in the tcpl
#' database to their respective abbreviations
#' 
#' @param names Character, strings to convert
#' 
#' @return The same character vector given with any name strings converted to 
#' the abbreviated version

.convertNames <- function(names) {
  
  names <- sub("aenm", "assay_component_endpoint_name", names)
  names <- sub("acnm", "assay_component_name", names)
  names <- sub("anm",  "assay_name", names)
  names <- sub("asnm", "assay_source_name", names)
  
  names
  
}
  