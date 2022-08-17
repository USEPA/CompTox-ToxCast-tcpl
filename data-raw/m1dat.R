#==============================================================================#
# NOTE: This script is written such that it is run from 'top' to 'bottom'
#       or programmatically via the Terminal.
#         ('R CMD BATCH --vanilla <script.R>'.)
#       Please do not jump around when running this script.
#==============================================================================#
## r packages
library(tcplfit2)
library(tcpl)

library(here)
library(data.table)
library(devtools)
library(dplyr)
library(tidyr)
library(tictoc)
library(numbers)
#---------------------------#
## code to prepare `mc_vignette` dataset goes herelibrary(tcpl)
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
# load the level 1 data from the database for the plot plate figure
tictoc::tic()
m1dat <- tcplLoadData(lvl = 1,
                      fld = "acid",
                      val = 766,
                      type = "mc")
tictoc::toc()
# prepare the data into a readable format
m1dat <- tcplPrepOtpt(m1dat)
# print the head of the data
head(m1dat)
# check the data is completion
sapply(m1dat,function(x)any(is.na(x)))
# manually checked the generated data with the following code
#   (commented out since it is not necessary for this script)
# tcplPlotPlate(dat = m1dat,apid = "TP0001915")
#---------------------------#
## save the data
usethis::use_data(m1dat, overwrite = TRUE)
#---------------------------#
## session information
Sys.time()
sessionInfo()
#---------------------------#