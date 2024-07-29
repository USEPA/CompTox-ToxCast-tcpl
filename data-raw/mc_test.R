#==============================================================================#
# NOTE: This script is written such that it is run from 'top' to 'bottom'
#       or programmatically via the Terminal.
#         ('R CMD BATCH --vanilla <script.R>'.)
#       Please do not jump around when running this script.
#==============================================================================#
## r packages
devtools::load_all()

library(here)
library(dplyr)
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
mc5_counts <- mc5_counts %>% filter(max_hitc > 0.9 & n < 10)
# pick one aeid
aeid <- mc5_counts[sample(1:nrow(mc5_counts),size = 1,replace = FALSE),aeid]
# obtain the acid for the example dataset
acid <- tcplLoadAcid(fld = 'aeid',val = aeid)$acid
# pick one sample/row from each level (lvl 3 contains ids back to lvl 0 and lvl 6 does back to lvl 4)
l3_sample <- tcplLoadData(lvl = 3, fld = "acid", val = acid)[sample(1:nrow(l3),size = 1,replace = FALSE)]
l6_sample <- tcplLoadData(lvl = 6, fld = "aeid", val = aeid, add.fld = FALSE)[sample(1:nrow(l6),size = 1,replace = FALSE)]
l7_sample <- tcplLoadData(lvl = 7, fld = "aeid", val = aeid, add.fld = FALSE)[sample(1:nrow(l7),size = 1,replace = FALSE)]


get_query_data <- function(lvl, fld, val, add.fld = TRUE) {
  
  # IMPORTANT || MUST ADD TEMPORARY LINE TO TCPLQUERY --------------------------
  # add temporary line to top of tcplQuery to get the query string: print(query)
  query_strings <- capture.output(result<-tcplLoadData(lvl = lvl, fld = fld, val = val, add.fld = add.fld))
  query_strings <- unique(gsub("\\\\", "\\\"", gsub("\"", "", gsub("\\\\n", "\\\n", gsub("\\[1\\] ", "", query_strings)))))
  
  # use queries to save data
  dat <- lapply(query_strings, function(query_string) {
    return(tcplQuery(query_string))
  })
  names(dat) <- query_strings
  
  # also store fld and val in list object for use in test case
  dat[fld] <- val
  return(dat)
  
}


# to add more tests with new/different data to test-tcplLoadData.R, add lines below and run script
mc_test <- list(
  tcplConfQuery = tcplQuery("SHOW VARIABLES LIKE 'max_allowed_packet'"),
  mc0_by_m0id = get_query_data(lvl = 0, fld = "m0id", val = l3_sample$m0id),
  mc0_by_acid = get_query_data(lvl = 0, fld = "acid", val = acid),
  mc1_by_m1id = get_query_data(lvl = 1, fld = "m1id", val = l3_sample$m1id),
  mc1_by_acid = get_query_data(lvl = 1, fld = "acid", val = acid),
  mc2_by_m2id = get_query_data(lvl = 2, fld = "m2id", val = l3_sample$m2id),
  mc2_by_acid = get_query_data(lvl = 2, fld = "acid", val = acid),
  mc3_by_m3id = get_query_data(lvl = 3, fld = "m3id", val = l3_sample$m3id),
  mc3_by_aeid = get_query_data(lvl = 3, fld = "aeid", val = aeid),
  mc4_by_m4id = get_query_data(lvl = 4, fld = "m4id", val = l6_sample$m4id),
  mc4_by_aeid = get_query_data(lvl = 4, fld = "aeid", val = aeid, add.fld = FALSE),
  mc5_by_m5id = get_query_data(lvl = 5, fld = "m5id", val = l6_sample$m5id),
  mc5_by_aeid = get_query_data(lvl = 5, fld = "aeid", val = aeid, add.fld = FALSE),
  mc6_by_m6id = get_query_data(lvl = 6, fld = "m6id", val = l6_sample$m6id),
  mc6_by_aeid = get_query_data(lvl = 6, fld = "aeid", val = aeid),
  mc7_by_m7id = get_query_data(lvl = 7, fld = "m7id", val = l7_sample$m7id),
  mc7_by_aeid = get_query_data(lvl = 7, fld = "aeid", val = aeid),
  mcagg_by_aeid = get_query_data(lvl = "agg", fld = "aeid", val = aeid)
)
#---------------------------#
## save the data
usethis::use_data(mc_test, overwrite = TRUE)
#---------------------------#