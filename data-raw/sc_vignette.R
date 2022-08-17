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
## code to prepare `sc_vignette` dataset goes here
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
# obtain all level 0 data
sc.lvl0 <- tcplLoadData(lvl = 0,type = 'sc')
# obtain all acid's for Attagene ('ATG')
atg.acid <- tcplLoadAcid() %>%
  # keep only the 'ATG' assay components
  filter(.,grepl(acnm,pattern = "^ATG"))
# check if there are any attagene acid's in single concentration data
any(unique(atg.acid[,acid])%in%unique(sc.lvl0[,acid]))
# obtain the atg acid's in single concentration data
atg.sc.ids <- which(unique(atg.acid[,acid])%in%unique(sc.lvl0[,acid])) %>% 
  unique(atg.acid[,acid])[.]
# obtain the data.table of acid's in single concentration data
atg.acid.sc <- atg.acid %>% filter(.,acid%in%atg.sc.ids) %>% as.data.table() 
# print the data.table of acid's
atg.acid.sc
# obtain the aeid's for the atg single concentration data
atg.aeid.sc <- tcplLoadAeid(fld = 'acid',val = atg.acid.sc) %>% 
  filter(.,grepl(aenm,pattern = "up$"))
# obtain the level 2 sc data
tictoc::tic()
atg.sc2 <- tcplPrepOtpt(
  tcplLoadData(lvl = 2,type = "sc",
               fld = 'aeid',val = atg.aeid.sc[,aeid])
)
tictoc::toc()
# randomly choose acid's that have at least 1 hitcall = 1
atg.aenm.active <- atg.sc2 %>% 
  # keep only the cases where there is an active hit call
  filter(.,hitc == 1)

set.seed(1007)
atg.aenm.sc.ss <- sample(1:nrow(atg.aenm.active),size = 1,replace = FALSE) %>% 
  atg.aenm.active[.,aenm]
# obtain the acid for the example dataset
atg.acid.sc.ss <- tcplLoadAcid(fld = 'aenm',val = atg.aenm.sc.ss)
# obtain the aeid for the example dataset
atg.aeid.sc.ss <- tcplLoadAeid(fld = 'aenm',val = atg.aenm.sc.ss)
# obtain level 0 sc data
tictoc::tic()
atg.sc0 <- tcplPrepOtpt(
  tcplLoadData(lvl = 0,type = "sc",
               fld = 'acid',val = atg.acid.sc.ss[,acid])
)
tictoc::toc()
# obtain level 1 sc data
tictoc::tic()
atg.sc1 <- tcplPrepOtpt(
  tcplLoadData(lvl = 1,type = "sc",
               fld = 'acid',val = atg.acid.sc.ss[,acid])
)
tictoc::toc()
# reload the level 2 sc data
#   This time we only need the selected example dataset
tictoc::tic()
atg.sc2 <- tcplPrepOtpt(
  tcplLoadData(lvl = 2,type = "sc",
               fld = 'aeid',val = atg.aeid.sc.ss[,aeid])
)
tictoc::toc()
# narrow down to several compounds/spids
tictoc::tic()
atg.sc2.ss <- atg.sc2 %>%
  # group the data by their hitcall - i.e. activity
  dplyr::group_by(hitc) %>% 
  # randomly select 10 compounds (5 active & 5 inactive)
  dplyr::slice_sample(.,n = 5,replace = FALSE) %>% 
  # ungroup the data
  dplyr::ungroup() %>% 
  # re-arrange the observations by compound name
  dplyr::arrange(chnm) %>% 
  # convert to a data.table format
  as.data.table()
tictoc::toc()
# subset levels 0 and 1 data based on spids in the level 5 subset
tictoc::tic()
atg.sc0.ss <- atg.sc0 %>% 
  # keep only those observations that are related to the level 2 spids
  dplyr::filter(spid %in% atg.sc2.ss[,spid])
tictoc::toc()

tictoc::tic()
atg.sc1.ss <- atg.sc1 %>% 
  # keep only those observations that are related to the level 2 spids
  dplyr::filter(spid %in% atg.sc2.ss[,spid]) %>% 
  # keep only those observations that are related to the 'up' endpoints
  dplyr::filter(.,grepl(aenm,pattern = "up$"))
tictoc::toc()

# collate the data into a single object 'sc_vignette'
sc_vignette <- list(
  sc0 = atg.sc0.ss,sc1 = atg.sc1.ss,sc2 = atg.sc2.ss
)
#---------------------------#
## save the data
usethis::use_data(sc_vignette, overwrite = TRUE)
#---------------------------#
## session information
Sys.time()
sessionInfo()
#---------------------------#