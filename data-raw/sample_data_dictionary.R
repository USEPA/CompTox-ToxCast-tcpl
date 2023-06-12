#==============================================================================#
# NOTE: This script is written such that it is run from 'top' to 'bottom'
#       or programmatically via the Terminal.
#         ('R CMD BATCH --vanilla <script.R>'.)
#       Please do not jump around when running this script.
#==============================================================================#
## r packages
library(tcpl)
library(here)
#---------------------------#
## code to prepare `sample_data_dictionary` dataset goes here
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
#---------------------------#
# obtain all data dictionary descriptions for the following tables
invitrodb_dd <- tcplDefine(c("sample", "chemical", "sc0", "sc1", "mc0", "mc1"))
#---------------------------#
## save the data
usethis::use_data(invitrodb_dd, overwrite = TRUE)