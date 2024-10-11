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
## code to prepare `load_data_columns` dataset goes here
# source the user ID, password, host, and database information for connection
#  - NOTE: To replicate one will need to save their own 'db_cred.R', including
#          the 'userid', 'userpwd', 'host', and DB collection via 'ivtdb'.
source(file = here::here("data-raw/db_cred.R"),verbose = FALSE)
# connect to the DB, update 
tcplConf(user = userid,
         pass = userpwd,
         host = host,
         db = "prod_internal_invitrodb_v4_1",
         drvr = "MySQL")
#---------------------------#
# obtain all data tables for each level and pull out the column names
mc0 <- tcplLoadData(lvl = 0, fld = "acid", val = 2850, type = "mc", add.fld = FALSE)
mc1 <- tcplLoadData(lvl = 1, fld = "acid", val = 2850, type = "mc", add.fld = FALSE)
mc2 <- tcplLoadData(lvl = 2, fld = "acid", val = 2850, type = "mc", add.fld = FALSE)
mc3 <- tcplLoadData(lvl = 3, fld = "aeid", val = 3088, type = "mc", add.fld = FALSE)
mc4 <- tcplLoadData(lvl = 4, fld = "aeid", val = 3088, type = "mc", add.fld = FALSE)
mc5 <- tcplLoadData(lvl = 5, fld = "aeid", val = 3088, type = "mc", add.fld = FALSE)
mc6 <- tcplLoadData(lvl = 6, fld = "aeid", val = 3088, type = "mc", add.fld = FALSE)
mcagg <- tcplLoadData(lvl = "agg", fld = "aeid", val = 3088, type = "mc", add.fld = FALSE)
sc0 <- tcplLoadData(lvl = 0, fld = "acid", val = 2850, type = "sc", add.fld = FALSE)
sc1 <- tcplLoadData(lvl = 1, fld = "acid", val = 2850, type = "sc", add.fld = FALSE)
sc2 <- tcplLoadData(lvl = 2, fld = "aeid", val = 3088, type = "sc", add.fld = FALSE)
scagg <- tcplLoadData(lvl = "agg", fld = "aeid", val = 3088, type = "sc", add.fld = FALSE)
load_data_columns <- list(mc0 = colnames(mc0), mc1 = colnames(mc1), 
                          mc2 = colnames(mc2), mc3 = colnames(mc3), 
                          mc4 = colnames(mc4), mc5 = colnames(mc5), 
                          mc6 = colnames(mc6), mcagg = colnames(mcagg), 
                          sc0 = colnames(sc0), sc1 = colnames(sc1), 
                          sc2 = colnames(sc2), scagg = colnames(scagg))
#---------------------------#
## save the data
usethis::use_data(load_data_columns, overwrite = TRUE)