#==============================================================================#
# NOTE: This script is written such that it is run from 'top' to 'bottom'
#       or programmatically via the Terminal.
#         ('R CMD BATCH --vanilla <script.R>'.)
#       Please do not jump around when running this script.
#==============================================================================#
# NOTE: You MUST temporarily update tcplQuery() by adding a line at the top of the
#       function: print(query). 
#       This is because the queries will be captured from output and saved as
#       part of the mocking data.
#==============================================================================#
## r packages
devtools::load_all()

library(here)
library(dplyr)
library(stringr)
#---------------------------#
## code to prepare `sc_test` dataset goes here
# source the user ID, password, host, and database information for connection
#  - NOTE: To replicate one will need to save their own 'db_cred.R', including
#          the 'userid', 'userpwd', 'host', and DB collection via 'ivtdb'.
source(file = here::here("data-raw/db_cred.R"),verbose = FALSE)
# connect to the DB
tcplConf(user = userid,
         pass = userpwd,
         host = host,
         db = ivtdb,
         drvr = "MySQL")

# pick endpoints and ids
# load the number of rows and max hitc per aeid
sc2_counts <- tcplQuery("SELECT DISTINCT aeid, 
                        COUNT( aeid ) as n, 
                        max(hitc) as max_hitc
                        FROM invitrodb.sc2 GROUP BY aeid")
# filter to only include where at least one sample is active and n = 7
sc2_counts <- sc2_counts %>% filter(max_hitc > 0.9 & n == 7)
# pick one aeid
aeid <- selected <- sc2_counts[sample(1:nrow(sc2_counts),size = 1,replace = FALSE),aeid]
# obtain the acid for the example dataset
acid <- tcplLoadAcid(fld = 'aeid',val = aeid)$acid
# pick one sample/row from each level (lvl 1 contains ids back to lvl 0)
l1 <- tcplLoadData(type = "sc", lvl = 1, fld = "acid", val = acid)
l1_sample1 <- l1[sample(1:nrow(l1),size = 1,replace = FALSE)]
l1_sample2 <- l1[sample(1:nrow(l1),size = 2,replace = FALSE)]
l2 <- tcplLoadData(type = "sc", lvl = 2, fld = "aeid", val = aeid)
l2_sample1 <- l2[sample(1:nrow(l2),size = 1,replace = FALSE)]
l2_sample2 <- l2[sample(1:nrow(l2),size = 2,replace = FALSE)]
# pick compare.val endpoints and ids
# be sure to only allow to choose from endpoints with the same number of samples
sc2_counts <- filter(sc2_counts, n == sc2_counts[aeid == selected]$n & aeid != selected)
compare.aeid <- sc2_counts[sample(1:nrow(sc2_counts),size = 1,replace = FALSE),aeid]
compare.l2 <- tcplLoadData(type = "sc", lvl = 2, fld = "aeid", val = compare.aeid)
compare.l2_sample1 <- compare.l2[sample(1:nrow(compare.l2),size = 1,replace = FALSE)]
compare.l2_sample2 <- compare.l2[sample(1:nrow(compare.l2),size = 2,replace = FALSE)]


get_query_data <- function(lvl, fld, val, compare.val = NULL, add.fld = TRUE, func = "tcplLoadData") {
  
  # IMPORTANT || MUST ADD TEMPORARY LINE TO TCPLQUERY --------------------------
  # add temporary line to top of tcplQuery to get the query string: print(query)
  if (func == "tcplLoadData") {
    query_strings <- capture.output(result<-tcplLoadData(type = "sc", lvl = lvl, fld = fld, val = val, add.fld = add.fld))
  } else if (func == "tcplPlot") {
    query_strings <- capture.output(result<-tcplPlot(type = "sc", fld = fld, 
                                                     val = val, compare.val = compare.val, 
                                                     output = "pdf", multi = TRUE, verbose = TRUE,
                                                     fileprefix = "temp_tcplPlot"))
    file.remove(stringr::str_subset(list.files(), "^temp_tcplPlot")) # clean up
  }
  
  query_strings <- unique(gsub("\\\\", "\\\"", gsub("\"", "", gsub("\\\\n", "\\\n", gsub("\\[1\\] ", "", query_strings)))))
  
  # use queries to save data
  dat <- lapply(query_strings, function(query_string) {
    return(tcplQuery(query_string))
  })
  names(dat) <- query_strings
  
  # also store fld and val in list object for use in test case
  dat[fld] <- val
  if (!is.null(compare.val)) dat[sprintf("compare.%s", fld)] <- compare.val
  return(dat)
  
}



# to add more tests with new/different data to test-tcplLoadData.R, add lines below and run script
sc_test <- list(
  tcplConfQuery = tcplQuery("SHOW VARIABLES LIKE 'max_allowed_packet'"),
  sc0_by_s0id = get_query_data(lvl = 0, fld = "s0id", val = l1_sample1$s0id),
  sc0_by_acid = get_query_data(lvl = 0, fld = "acid", val = acid),
  sc1_by_s1id = get_query_data(lvl = 1, fld = "s1id", val = l1_sample1$s1id),
  sc1_by_acid = get_query_data(lvl = 1, fld = "acid", val = acid),
  sc2_by_s2id = get_query_data(lvl = 2, fld = "s2id", val = l2_sample1$s2id),
  sc2_by_aeid = get_query_data(lvl = 2, fld = "aeid", val = aeid),
  scagg_by_aeid = get_query_data(lvl = "agg", fld = "aeid", val = aeid),
  plot_single_s2id = get_query_data(fld = "s2id", 
                                    val = l2_sample1$s2id, 
                                    func = "tcplPlot"),
  plot_multiple_s2id = get_query_data(fld = "s2id", 
                                      val = list(l2_sample2$s2id), 
                                      func = "tcplPlot"),
  plot_single_aeid = get_query_data(fld = "aeid", 
                                    val = aeid, 
                                    func = "tcplPlot"),
  plot_multiple_aeid = get_query_data(fld = "aeid", 
                                      val = list(c(aeid, compare.aeid)), 
                                      func = "tcplPlot"),
  plot_single_spid = get_query_data(fld = c("spid", "aeid"), 
                                    val = list(l2_sample1$spid, aeid), 
                                    func = "tcplPlot"),
  plot_multiple_spid = get_query_data(fld = c("spid", "aeid"), 
                                      val = list(l2_sample2$spid, aeid), 
                                      func = "tcplPlot"),
  plot_single_s2id_compare = get_query_data(fld = "s2id", 
                                            val = l2_sample1$s2id, 
                                            compare.val = compare.l2_sample1$s2id, 
                                            func = "tcplPlot"),
  plot_multiple_s2id_compare = get_query_data(fld = "s2id", 
                                              val = list(l2_sample2$s2id), 
                                              compare.val = list(compare.l2_sample2$s2id), 
                                              func = "tcplPlot"),
  plot_single_aeid_compare = get_query_data(fld = "aeid", 
                                            val = aeid, 
                                            compare.val = compare.aeid, 
                                            func = "tcplPlot"),
  plot_multiple_aeid_compare = get_query_data(fld = "aeid", 
                                              val = list(c(aeid, compare.aeid)), 
                                              compare.val = list(c(compare.aeid, aeid)), 
                                              func = "tcplPlot"),
  plot_single_spid_compare = get_query_data(fld = c("spid", "aeid"), 
                                            val = list(l2_sample1$spid, aeid), 
                                            compare.val = list(compare.l2_sample1$spid, compare.aeid), 
                                            func = "tcplPlot"),
  plot_multiple_spid_compare = get_query_data(fld = c("spid", "aeid"), 
                                              val = list(l2_sample2$spid, aeid), 
                                              compare.val = list(compare.l2_sample2$spid, compare.aeid), 
                                              func = "tcplPlot")
)
#---------------------------#
## save the data
usethis::use_data(sc_test, overwrite = TRUE)
#---------------------------#