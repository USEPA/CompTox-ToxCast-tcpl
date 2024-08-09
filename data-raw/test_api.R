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
library(ctxR)
library(here)
library(dplyr)
library(stringr)
#---------------------------#
## code to prepare `test_api` dataset goes here
# source the user ID, password, host, and database information for connection
#  - NOTE: To replicate one will need to save their own 'db_cred.R', including
#          the 'apikey'.
source(file = here::here("data-raw/db_cred.R"),verbose = FALSE)
# connect to the DB
tcplConf(pass = apikey,
         drvr = "API")

assays <- get_all_assays(Server = paste0(getOption("TCPL_HOST")))
aeid <- assays[sample(1:nrow(assays), 1),]$aeid
acid <- tcplLoadAcid(fld = "aeid", val = aeid)$acid
dat <- tcplLoadData(lvl = 5, fld = "aeid", val = aeid)
sample <- dat[sample(1:nrow(dat), 1),]

# to add more tests with new/different data to test-tcplLoadData.R, add lines below and run script
test_api <- list(
  aeid = aeid,
  acid = acid,
  dtxsid = sample$dtxsid,
  spid = sample$spid,
  m4id = sample$m4id
)
#---------------------------#
## save the data
usethis::use_data(test_api, overwrite = TRUE)
#---------------------------#