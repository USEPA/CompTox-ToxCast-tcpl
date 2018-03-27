#----------------------------------------------------------------------------------
# Generate summary stats for the table
#----------------------------------------------------------------------------------

tcpldbStats <- function(){
  
  # get a list of acid to number of aeids for each acid
  query <- "Select acid,Count(aeid) as 'num_aeids' from assay_component_endpoint Group By acid;"
  acid_aeid_list <- tcplQuery(query,tbl = "assay_component_endpoint")
  acid_aeid_list <- setNames(acid_aeid_list$num_aeids,acid_aeid_list$acid)
  #Get a list of spids with the m0ids as key
  query <- "Select spid, m0id from mc0;"
  m0id_spid_list <- tcplQuery(query,tbl = "mc0")
  m0id_spid_list <- setNames(m0id_spid_list$spid,m0id_spid_list$m0id)
  # get all m0ids at L1 
  query <- "Select distinct m0id from mc1;"
  L1m0 <- tcplQuery(query, tbl = "mc1")
  # get all m0ids at L2
  query <- "Select distinct m0id from mc2;"
  L2m0 <- tcplQuery(query, tbl = "mc2")
  # get all m0ids at L3
  query <- "Select distinct m0id from mc3;"
  L3m0 <- tcplQuery(query, tbl = "mc3")
  
}