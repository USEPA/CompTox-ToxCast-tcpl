#' tcpl Wrapper for tcplfit2_core including additional calculations to fit into new schema
#'
#' @param dat 
#'
#' @return
#'
#' @examples
tcplFit2 <- function(dat){
  # do all the regular fitting things that still need to be done
  
  # fit with tcplfit2_core per spid
  output = tcplfit2_core(conc,resp, 0, force.fit = TRUE)
  
  # return wide format dat
  dat
}