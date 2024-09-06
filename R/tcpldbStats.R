#----------------------------------------------------------------------------------
# Generate summary stats for the table
#----------------------------------------------------------------------------------

#' @title Get summary statistics for the database
#' 
#' @description 
#' \code{tcpldbStats} takes a string(type) and an optional parameter(val) to return the summary statistics on the entire tcpl database
#'  When type = "all" the val is ignored. the function returns the number of distinct spid and aeids in the database at each level
#'  When type = "aeid", the val parameter has to be a valid aeid in the database. The function returns a 
#'  table consisting of the number of distinct spids at each level of processing for the aeid given in 'val'
#'  When type = "spid", the val parameter has to be a valid spid in the database. The function returns a 
#'  table consisting of the number of distinct aeids at each level of processing for the given spid in 'val'
#' 
#' @param type String either "all", "aeid" or "spid"
#' @param val integer if type = "aeid" , string if type  = "spid"
#' @importFrom stats setNames
#' @export

tcpldbStats <- function(type = "all", val = NULL){
  # get a list of acid to number of aeids for each acid
  query <- "Select acid,Count(aeid) as 'num_aeids' from assay_component_endpoint Group By acid;"
  acid_aeid_list <- tcplQuery(query,tbl = "assay_component_endpoint")
  acid_aeid_list <- setNames(acid_aeid_list$num_aeids,acid_aeid_list$acid)
  # get the list of m0ids for each spid at mc0
  query <- "select spid, group_concat(m0id) as 'm0id_list' from mc0 group by spid;"
  spid_mc0_lst <- tcplQuery(query,tbl = "mc0")
  spid_mc0_lst <- setNames(spid_mc0_lst$m0id_list,spid_mc0_lst$spid)
  # get the list of m4ids for each spid at mc4
  query <- "select spid, group_concat(m4id) as 'm4id_list' from mc4 group by spid;"
  spid_mc4_lst <- tcplQuery(query,tbl = "mc4")
  spid_mc4_lst <- setNames(spid_mc4_lst$m4id_list,spid_mc4_lst$spid)
  
  # #Get a list of spids with the m0ids as key
  # query <- "Select spid, m0id from mc0;"
  # m0id_spid_list <- tcplQuery(query,tbl = "mc0")
  # m0id_spid_list <- setNames(m0id_spid_list$spid,m0id_spid_list$m0id)
  num_spid <- list()
  num_aeid <- list()
  if (type == "all"){
    # Table 1 Number of distinct spid and aeid per processing level
   
   
    
    ##SPIDS per processing level
    # for level 0 processing
    query <- "Select count(distinct spid) as 'spid' from mc0;"
    num_spid <- c(num_spid,tcplQuery(query,tbl = "mc0")$spid)
    # for the other spid for level 1 through 3 we can use a for loop
    for (x in c(1:2)){
      mc_tble <- paste0("mc",x)
      query <- sprintf("Select count(distinct spid) as 'spid' From mc0 inner join %s on mc0.m0id = %s.m0id;",
                       mc_tble,mc_tble)
      val <- tcplQuery(query, tbl = c("mc0",mc_tble))
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
      val <- tcplQuery(query, tbl = c("mc4",mc_tble))
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
      query <- sprintf("Select count(distinct aeid) as 'num_aeid' from %s;",mc_tble)
      val <- tcplQuery(query, tbl = c("assay_component_endpoint",mc_tble))
      num_aeid <- c(num_aeid,val$num_aeid)
    }
    tble <- data.frame("Levels"=c("mc0","mc1","mc2","mc3","mc4","mc5","mc6"),
                        "spid" = num_spid,
                        "aeid" = num_aeid,
                        stringsAsFactors = F)
  }
  ## Table 2 : for a given aeid, how many spids are present at each level
  else if(type == "aeid"){
    aeid <- val
    # find the acid associated with the aeid to use in L0 to L3
    acid <- as.numeric(tcplQuery(sprintf("Select acid from assay_component_endpoint where aeid = %d;",aeid),
                      tbl = "assay_component_endpoint")$acid)
    query <- sprintf("Select count(distinct spid) as 'num_spid' from mc0 where mc0.acid = %d", acid)
    num_spid <- c(num_spid, tcplQuery(query, tbl= "mc0")$num_spid)
    # for level 1 to level 3, find number of distinct spid for a given acid
    for (x in c(1:3)){
      mc_tble <- paste0("mc",x)
      query <- sprintf("Select count(distinct spid) as 'spid' From mc0 inner join %s on mc0.m0id = %s.m0id where %s.acid = %d;",
                       mc_tble,mc_tble,mc_tble,acid)
      val <- tcplQuery(query, tbl = c("mc0",mc_tble))
      num_spid <- c(num_spid,val$spid)
    }
    # for L4
    query <- sprintf("Select count(distinct spid) as 'num_spid' from mc4 where mc4.aeid = %d", aeid)
    num_spid <- c(num_spid, tcplQuery(query, tbl= "mc4")$num_spid)
    # for L5 to L6
    for (x in c(5:6)){
      mc_tble <- paste0("mc",x)
      query <- sprintf("Select count(distinct spid) as 'spid' From mc4 inner join %s on mc4.m4id = %s.m4id where %s.aeid = %d;",
                       mc_tble,mc_tble,mc_tble,aeid)
      val <- tcplQuery(query, tbl = c("mc0",mc_tble))
      num_spid <- c(num_spid,val$spid)
    }
    tble <- data.frame("Levels"=c("mc0","mc1","mc2","mc3","mc4","mc5","mc6"),
                        "spid" = num_spid,
                        "aeid" = rep(val,7),
                        stringsAsFactors = F)
  }
  ## Table 3 : for a given spid, how many aeids are present at each level
  else if (type == "spid"){
    # find m0ids for the given spid
    m0id_list <- spid_mc0_lst[val]
    # find m0ids for the given spid
    m4id_list <- spid_mc4_lst[val]
    # for level 0 through 3, find the list of distinct acid that serve these m0ids
    for(x in c(0:3)){
      mc_tble <- paste0("mc",x)
      query <- sprintf("Select distinct acid from %s where %s.m0id in (%s);",mc_tble,mc_tble,m0id_list)
      val <- tcplQuery(query, tbl = mc_tble)
      num_aeid <- c(num_aeid,sum(acid_aeid_list[val$acid]))
    }
    # for level 4 through 6, find the list of distinct aeid that serve these m4ids
    for(x in c(4:6)){
      mc_tble <- paste0("mc",x)
      query <- sprintf("Select count(distinct aeid) as 'num_aeid' from %s where %s.m4id in (%s);",mc_tble,mc_tble,m4id_list)
      val <- tcplQuery(query, tbl = mc_tble)
      num_aeid <- c(num_aeid,sum(val$num_aeid))
    }
    tble <- data.frame("Levels"=c("mc0","mc1","mc2","mc3","mc4","mc5","mc6"),
                       "aeid" = num_aeid,
                       "spid" = rep(val,7),
                       stringsAsFactors = F)
     
  }
  return(tble)
}