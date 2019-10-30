#' A function for hitting the chemtrack api to get sample details
#'
#' @param spid single spid or an array of spids
#' @param verbose if true we want to return all values from api, not just those that match the sample table format
#'
#' @return a data_table containing the results of hitting the chemtrack api for each spid
#'
#' @examples 
#' tcpl:::tcplspidAPIsearch("PINTO01C04")
tcplspidAPIsearch <- function(spid, verbose = FALSE){
  #variable binding
  errors <- NULL
  
  # check that we have one or more spids
  
  # URL we are hitting to get the chemtrack results
  baseurl <- "http://chemtrack-prod.epa.gov/api/sample_details/"
  
  
  #spid <- spids
  #add check to see if spid are already in table?
  
  
  # need to add 100 ms delay
  df <- lapply(spid, function(x) {
    Sys.sleep(.1)
    httr::content(httr::GET(paste0(baseurl,x)))}
  )
  
  df <- dplyr::bind_rows(df)
  df$spid <- spid
  
  #convert raw chemtrack to sample table format
  if("sample_concentration_unit" %in% names(df)){
  df <- dplyr::mutate(df, tested_conc_unit = dplyr::case_when(
      sample_concentration_unit == "mM" ~ "uM",
      sample_concentration_unit == "mg/ml" ~ "mg/l",
      sample_concentration_unit == "kCF" ~ "CF"
    ))
  }
  
  if("errors" %in% names(df)){
    df$errors <- unlist(tidyr::replace_na(df$errors,NA))
    df <- dplyr::mutate(df,errors = unlist(errors))
  }
  
  df <- dplyr::select(df,spid,dplyr::everything())
  
  # warn if errors or not unblinded
  if("errors" %in% names(df)){
    errs <- unique(unlist(df$errors))[!is.na(unique(unlist(df$errors)))]
    for(e in errs){
      total_err <- sum(e == df$errors, na.rm = TRUE)
      warning(paste0(total_err, " spid(s) returned the following error: ", e))
    }
  }

  #could do some formatting here to match sample table in invitrodb  
  df

  
}