tcplspidAPIsearch <- function(spid){
  # check that we have one or more spids
  
  # URL we are hitting to get the chemtrack results
  baseurl <- "http://chemtrack-prod.epa.gov/api/sample_details/"
  
  
  #spid <- spids
  #add check to see if spid are already in table?
  
  
  # need to add 100 ms delay
  df <- lapply(spid, function(x) {
    Sys.sleep(.1)
    httr::content(GET(paste0(baseurl,x)))}
  )
  
  #convert raw chemtrack to sample table format
  spid_add <- df %>%
    {
      tibble(
        spid = spid,
        stkc = map_dbl(., "sample_concentration"),
        stkc_unit = map_chr(., "sample_concentration_unit"),
        dsstox_substance_id = map_chr(., "dtxsid")
      )
    } %>%
    mutate(tested_conc_unit = case_when(
      stkc_unit == "mM" ~ "uM",
      stkc_unit == "mg/ml" ~ "mg/l",
      stkc_unit == "kCF" ~ "CF"
    )) %>%
    rowwise() %>%
    mutate(chid = unlist(tcplQuery(paste0("SELECT id FROM ro_stg_dsstox.generic_substances where dsstox_substance_id = '", dsstox_substance_id, "';"),
                                   db = "ro_stg_dsstox",
                                   tbl = "generic_substances"))) %>% 
    select(spid,chid,stkc,stkc_unit,tested_conc_unit)
  
}