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
## code to prepare `mc_test` dataset goes here
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
mc5_counts <- tcplQuery("SELECT DISTINCT aeid, 
                        COUNT( aeid ) as n, 
                        max(hitc) as max_hitc
                        FROM invitrodb.mc5 GROUP BY aeid")
# filter to only include where at least one sample is active and n < 10
mc5_counts <- mc5_counts %>% filter(max_hitc > 0.9 & n == 2)
# pick one aeid
aeid <- selected <- mc5_counts[sample(1:nrow(mc5_counts),size = 1,replace = FALSE),aeid]
# obtain the acid for the example dataset
acid <- tcplLoadAcid(fld = 'aeid',val = aeid)$acid
# pick one sample/row from each level (lvl 3 contains ids back to lvl 0 and lvl 6 does back to lvl 4)
l3 <- tcplLoadData(lvl = 3, fld = "acid", val = acid)
l3_sample1 <- l3[sample(1:nrow(l3),size = 1,replace = FALSE)]
l3_sample2 <- l3[sample(1:nrow(l3),size = 2,replace = FALSE)]
l5 <- tcplLoadData(lvl = 5, fld = "aeid", val = aeid, add.fld = FALSE)
l5_sample1 <- l5[sample(1:nrow(l5),size = 1,replace = FALSE)]
l5_sample2 <- l5[sample(1:nrow(l5),size = 2,replace = FALSE)]
l6 <- tcplLoadData(lvl = 6, fld = "aeid", val = aeid, add.fld = FALSE)
l6_sample1 <- l6[sample(1:nrow(l6),size = 1,replace = FALSE)]
l6_sample2 <- l6[sample(1:nrow(l6),size = 2,replace = FALSE)]
l7 <- tcplLoadData(lvl = 7, fld = "aeid", val = aeid, add.fld = FALSE)
l7_sample1 <- l7[sample(1:nrow(l7),size = 1,replace = FALSE)]
l7_sample2 <- l7[sample(1:nrow(l7),size = 2,replace = FALSE)]
# pick compare.val endpoints and ids
# be sure to only allow to choose from endpoints with the same number of samples
mc5_counts <- filter(mc5_counts, n == mc5_counts[aeid == selected]$n & aeid != selected)
compare.aeid <- mc5_counts[sample(1:nrow(mc5_counts),size = 1,replace = FALSE),aeid]
compare.l5 <- tcplLoadData(lvl = 5, fld = "aeid", val = compare.aeid)
compare.l5_sample1 <- compare.l5[sample(1:nrow(compare.l5),size = 1,replace = FALSE)]
compare.l5_sample2 <- compare.l5[sample(1:nrow(compare.l5),size = 2,replace = FALSE)]


get_query_data <- function(lvl, fld, val, compare.val = NULL, add.fld = TRUE, func = "tcplLoadData") {
  message(compare.val)
  if (func == "tcplLoadData") {
    # IMPORTANT || MUST ADD TEMPORARY LINE TO TCPLQUERY --------------------------
    # add temporary line to top of tcplQuery to get the query string: print(query)
    query_strings <- capture.output(result<-tcplLoadData(lvl = lvl, fld = fld, val = val, add.fld = add.fld))
  } else if (func == "tcplPlot") {
    query_strings <- capture.output(result<-tcplPlot(type = "mc", fld = fld, 
                                                     val = val, compare.val = compare.val, 
                                                     output = "pdf", multi = TRUE, flags = TRUE, 
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
mc_test <- list(
  tcplConfQuery = tcplQuery("SHOW VARIABLES LIKE 'max_allowed_packet'"),
  mc0_by_m0id = get_query_data(lvl = 0, fld = "m0id", val = l3_sample1$m0id),
  mc0_by_acid = get_query_data(lvl = 0, fld = "acid", val = acid),
  mc1_by_m1id = get_query_data(lvl = 1, fld = "m1id", val = l3_sample1$m1id),
  mc1_by_acid = get_query_data(lvl = 1, fld = "acid", val = acid),
  mc2_by_m2id = get_query_data(lvl = 2, fld = "m2id", val = l3_sample1$m2id),
  mc2_by_acid = get_query_data(lvl = 2, fld = "acid", val = acid),
  mc3_by_m3id = get_query_data(lvl = 3, fld = "m3id", val = l3_sample1$m3id),
  mc3_by_aeid = get_query_data(lvl = 3, fld = "aeid", val = aeid),
  mc4_by_m4id = get_query_data(lvl = 4, fld = "m4id", val = l5_sample1$m4id),
  mc4_by_aeid = get_query_data(lvl = 4, fld = "aeid", val = aeid, add.fld = FALSE),
  mc5_by_m5id = get_query_data(lvl = 5, fld = "m5id", val = l5_sample1$m5id),
  mc5_by_aeid = get_query_data(lvl = 5, fld = "aeid", val = aeid, add.fld = FALSE),
  mc6_by_m6id = get_query_data(lvl = 6, fld = "m6id", val = l6_sample1$m6id),
  mc6_by_aeid = get_query_data(lvl = 6, fld = "aeid", val = aeid),
  mc7_by_m7id = get_query_data(lvl = 7, fld = "m7id", val = l7_sample1$m7id),
  mc7_by_aeid = get_query_data(lvl = 7, fld = "aeid", val = aeid),
  mcagg_by_aeid = get_query_data(lvl = "agg", fld = "aeid", val = aeid),
  plot_single_m4id = get_query_data(fld = "m4id", 
                                    val = l5_sample1$m4id, 
                                    func = "tcplPlot"),
  plot_multiple_m4id = get_query_data(fld = "m4id", 
                                      val = list(l5_sample2$m4id), 
                                      func = "tcplPlot"),
  plot_single_aeid = get_query_data(fld = "aeid", 
                                    val = aeid, 
                                    func = "tcplPlot"),
  plot_multiple_aeid = get_query_data(fld = "aeid", 
                                      val = list(c(aeid, compare.aeid)), 
                                      func = "tcplPlot"),
  plot_single_spid = get_query_data(fld = c("spid", "aeid"), 
                                    val = list(l5_sample1$spid, aeid), 
                                    func = "tcplPlot"),
  plot_multiple_spid = get_query_data(fld = c("spid", "aeid"), 
                                      val = list(l5_sample2$spid, aeid), 
                                      func = "tcplPlot"),
  plot_single_m4id_compare = get_query_data(fld = "m4id", 
                                            val = l5_sample1$m4id, 
                                            compare.val = compare.l5_sample1$m4id, 
                                            func = "tcplPlot"),
  plot_multiple_m4id_compare = get_query_data(fld = "m4id", 
                                              val = list(l5_sample2$m4id), 
                                              compare.val = list(compare.l5_sample2$m4id), 
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
                                            val = list(l5_sample1$spid, aeid), 
                                            compare.val = list(compare.l5_sample1$spid, compare.aeid), 
                                            func = "tcplPlot"),
  plot_multiple_spid_compare = get_query_data(fld = c("spid", "aeid"), 
                                              val = list(l5_sample2$spid, aeid), 
                                              compare.val = list(compare.l5_sample2$spid, compare.aeid), 
                                              func = "tcplPlot")
)
#---------------------------#
## save the data
usethis::use_data(mc_test, overwrite = TRUE)
#---------------------------#