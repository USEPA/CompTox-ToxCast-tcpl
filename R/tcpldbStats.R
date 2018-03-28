#----------------------------------------------------------------------------------
# Generate summary stats for the table
#----------------------------------------------------------------------------------

tcpldbStats <- function(type = "all", val = NULL){
  # get a list of acid to number of aeids for each acid
  query <- "Select acid,Count(aeid) as 'num_aeids' from assay_component_endpoint Group By acid;"
  acid_aeid_list <- tcplQuery(query,tbl = "assay_component_endpoint")
  acid_aeid_list <- setNames(acid_aeid_list$num_aeids,acid_aeid_list$acid)
  #Get a list of spids with the m0ids as key
  query <- "Select spid, m0id from mc0;"
  m0id_spid_list <- tcplQuery(query,tbl = "mc0")
  m0id_spid_list <- setNames(m0id_spid_list$spid,m0id_spid_list$m0id)
  if (type == "all"){
    # Table 1 Number of distinct spid and aeid per processing level
    num_spid <- list()
    num_aeid <- list()
   
    
    ##SPIDS per processing level
    # for level 0 processing
    query <- "Select count(distinct spid) as 'spid' from mc0;"
    num_spid <- c(num_spid,tcplQuery(query,tbl = "mc0")$spid)
    # for the other spid for level 1 through 3 we can use a for loop
    for (x in c(1:2)){
      mc_tble <- paste0("mc",x)
      query <- sprintf("Select count(distinct spid) as 'spid' From mc0 inner join %s on mc0.m0id = %s.m0id;",
                       mc_tble,mc_tble)
      val <- tcplQuery(query, tble = c("mc0",mc_tble))
      num_spid <- c(num_spid,val$spid)
    }
    # for level 4 processing 
    query <- "Select count(distinct spid) as 'spid' from mc4;"
    num_spid <- c(num_spid,tcplQuery(query,tbl = "mc4")$spid)
    #for level 5 through 6 we need to use another loop since the common id is not m4id
    for (x in c(5,6)){
      mc_tble <- paste0("mc",x)
      query <- sprintf("Select count(distinct spid) as 'spid' From mc4 inner join %s on mc4.m4id = %s.m4id;",
                       mc_tble,mc_tble)
      val <- tcplQuery(query, tble = c("mc4",mc_tble))
      num_spid <- c(num_spid,val$spid)
    }
    #Aeids per processing level
    # level 0 though 2 track acids. the follwing loop will get aeids for them
    for (x in c(0:2)){
      mc_tble <- paste0("mc",x)
      query <- sprintf("Select count(distinct aeid) as 'num_aeid' From assay_component_endpoint inner join %s on %s.acid = assay_component_endpoint.acid;",
                       mc_tble,mc_tble)
      val <- tcplQuery(query, tbl = c("assay_component_endpoint",mc_tble))
      num_aeid <- c(num_aeid,val$num_aeid)
    }
    #levels 3 through 6 have aeid 
    for (x in c(3:6)){
      mc_tble <- paste0("mc",x)
      query <- sprint("Select count(distinct aeid) as 'num_aeid' from %s;",mc_tble)
      val <- tcplQuery(query, tbl = c("assay_component_endpoint",mc_tble))
      num_aeid <- c(num_aeid,val$num_aeid)
    }
    tble1 <- data.frame("Levels"=c("mc0","mc1","mc2","mc3","mc4","mc5","mc6"),
                        "spid" = num_spid,
                        "aeid" = num_aeid,
                        stringsAsFactors = F)
  }else if(type == "aeid"){
    aeid <- val
    # find the acid associated with the aeid to use in L0 to L2
    acid <- as.numeric(tcplQuery(sprintf("Select acid from assay_component_endpoint where aeid = %f;",aeid),
                      tbl = "assay_component_endpoint")$acid)
    
  }
  
  
  
  
  ## Table 2 : for a given aeid, how many spids are present at each level
  
  
  query <- "Select count(distinct aeid) as 'aeid' From assay_component_endpoint inner join mc0 on mc0.acid = assay_component_endpoint.acid"
  num_aeid <- c(num_aeid,tcplQuery(query,tbl = c("assay_component_endpoint","mc0"))$aeid)
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