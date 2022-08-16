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
## code to prepare `mc_vignette` dataset goes here
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
# obtain the attagene (atg) aeid's (Assay Endpoint ID's) from the DB
atg.aeid <- tcplGetAeid("ATG") %>% 
  # keep only the 'up' assay endpoints
  filter(.,grepl(aenm,pattern = "up")) %>% 
  # convert to a data.table object
  as.data.table()
# print the data.table of aeid's
atg.aeid
# number of BioSeek endpoints
nrow(atg.aeid)
# choose a random 'aeid' to utilize as an example dataset
set.seed(8011)
atg.aeid.ss <- sample(1:nrow(atg.aeid),size = 1,replace = FALSE) %>% 
  atg.aeid[.,]
# obtain the 'acid' for the endpoints in the example dataset
atg.acid.ss <- tcplLoadAcid(fld = 'aeid',val = atg.aeid.ss[,aeid])
# obtain level 0 mc data
tictoc::tic()
atg.mc0 <- tcplPrepOtpt(
  tcplLoadData(lvl = 0,type = "mc",
               fld = 'acid',val = atg.acid.ss[,acid])
)
tictoc::toc()
# obtain level 1 mc data
tictoc::tic()
atg.mc1 <- tcplPrepOtpt(
  tcplLoadData(lvl = 1,type = "mc",
               fld = 'acid',val = atg.acid.ss[,acid])
)
tictoc::toc()
# obtain level 2 mc data
tictoc::tic()
atg.mc2 <- tcplPrepOtpt(
  tcplLoadData(lvl = 2,type = "mc",
               fld = 'acid',val = atg.acid.ss[,acid])
)
tictoc::toc()
# obtain level 3 mc data
tictoc::tic()
atg.mc3 <- tcplPrepOtpt(
  tcplLoadData(lvl = 3,type = "mc",
               fld = 'aeid',val = atg.aeid.ss[,aeid],add.fld = TRUE)
)
tictoc::toc()
# obtain level 4 mc data
tictoc::tic()
atg.mc4 <- tcplPrepOtpt(
  tcplLoadData(lvl = 4,type = "mc",
               fld = 'aeid',val = atg.aeid.ss[,aeid],add.fld = TRUE)
)
tictoc::toc()
# obtain level 5 mc data
tictoc::tic()
atg.mc5 <- tcplPrepOtpt(
  tcplLoadData(lvl = 5,type = "mc",
               fld = 'aeid',val = atg.aeid.ss[,aeid],add.fld = TRUE)
)
tictoc::toc()
# narrow down to several active compounds/spids
tictoc::tic()
atg.mc5.ss <- atg.mc5 %>%
  # keep only the observations with a minimum of 2 reps per dose group
  dplyr::filter(.,nrep >=2) %>%
  # keep only the observations that have a hitcall >= 0.95
  dplyr::filter(.,hitc >= 0.95) %>%
  # re-arrange the observations by hicall in descending order
  dplyr::arrange(desc(hitc)) %>% 
  # keep the top 5 observations in the data.table
  .[1:5,]
tictoc::toc()
# subset levels 0 through 4 data based on spids in the level 5 subset
tictoc::tic()
atg.mc0.ss <- atg.mc0 %>% 
  # keep only those observations that are related to the level 5 spids
  dplyr::filter(spid %in% atg.mc5.ss[,spid])
tictoc::toc()

tictoc::tic()
atg.mc1.ss <- atg.mc1 %>% 
  # keep only those observations that are related to the level 5 spids
  dplyr::filter(spid %in% atg.mc5.ss[,spid])
tictoc::toc()

tictoc::tic()
atg.mc2.ss <- atg.mc2 %>% 
  # keep only those observations that are related to the level 5 spids
  dplyr::filter(spid %in% atg.mc5.ss[,spid])
tictoc::toc()

tictoc::tic()
atg.mc3.ss <- atg.mc3 %>% 
  # keep only those observations that are related to the level 5 spids
  dplyr::filter(spid %in% atg.mc5.ss[,spid])
tictoc::toc()

tictoc::tic()
atg.mc4.ss <- atg.mc4 %>% 
  # keep only those observations that are related to the level 5 spids
  dplyr::filter(spid %in% atg.mc5.ss[,spid])
tictoc::toc()
# collate the data into a single object 'mc_vignette'
mc_vignette <- list(
  mc0 = atg.mc0.ss,mc1 = atg.mc1.ss,mc2 = atg.mc2.ss,
  mc3 = atg.mc3.ss,mc4 = atg.mc4.ss,mc5 = atg.mc5.ss
)
#---------------------------#
## save the data
usethis::use_data(mc_vignette, overwrite = TRUE)
#---------------------------#
## session information
Sys.time()
sessionInfo()
#---------------------------#