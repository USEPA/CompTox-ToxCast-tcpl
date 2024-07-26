#==============================================================================#
# NOTE: This script is written such that it is run from 'top' to 'bottom'
#       or programmatically via the Terminal.
#         ('R CMD BATCH --vanilla <script.R>'.)
#       Please do not jump around when running this script.
#==============================================================================#
## r packages
devtools::load_all()

library(here)
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


# #add temporary line to tcplLoadData to print 'fld' from .prepField and qstring
# hide_response <- tcplLoadData(lvl = 4, fld = "m4id", val = 12699347)
# 
# 
# # use queries to save data
# mc4_m4id_12699347 <- list(.prepField = "mc4_param.m4id", 
#                           tcplQuery = tcplQuery('SELECT * FROM mc4,mc4_param WHERE mc4.m4id = mc4_param.m4id AND  mc4_param.m4id IN ("12699347");'))
# 
# 
# # collate the data into a single object 'mc_test'
# mc_test <- list(
#   tcplConfQuery = tcplQuery("SHOW VARIABLES LIKE 'max_allowed_packet'"),
#   mc4_m4id_12699347 = mc4_m4id_12699347
# )
# #---------------------------#
# ## save the data
# usethis::use_data(mc_test, overwrite = TRUE)
# #---------------------------#



get_query_data <- function(lvl, fld, val, add.fld = TRUE) {
  
  # different style of saving just tcplQuery output
  # add temporary line to top of tcplQuery to get the query string: print(query)
  query_strings <- capture.output(result<-tcplLoadData(lvl = lvl, fld = fld, val = val, add.fld = add.fld))
  query_strings <- unique(gsub("\\\\", "\\\"", gsub("\"", "", gsub("\\\\n", "\\\n", gsub("\\[1\\] ", "", query_strings)))))
  
  # use queries to save data
  dat <- lapply(query_strings, function(query_string) {
    return(tcplQuery(query_string))
  })
  names(dat) <- query_strings
  return(dat)
  
}

mc4_m4id_12699347 <- get_query_data(lvl = 4, fld = "m4id", val = 12699347)
mc5_m4id_12699347 <- get_query_data(lvl = 5, fld = "m4id", val = 12699347)

# collate the data into a single object 'mc_test'
mc_test <- list(
  tcplConfQuery = tcplQuery("SHOW VARIABLES LIKE 'max_allowed_packet'"),
  mc4_m4id_12699347 = mc4_m4id_12699347,
  mc5_m4id_12699347 = mc5_m4id_12699347
)
#---------------------------#
## save the data
usethis::use_data(mc_test, overwrite = TRUE)
#---------------------------#