## ----eval = TRUE, echo = FALSE, message = FALSE--------------------------
rm(list=ls())
library(htmlTable)
library(tcpl)

## ----eval = TRUE, message = FALSE----------------------------------------
## Add a new assay source, call it CTox,
## that produced the data
tcplRegister(what = "asid", flds = list(asid = 1, 
                      asnm = "CTox"))

## ----eval = TRUE---------------------------------------------------------
tcplLoadAsid()

## ----eval = TRUE, message = FALSE----------------------------------------
tcplRegister(what = "aid", 
             flds = list(asid = 1, 
                         anm = "TOX21_ERa_BLA_Agonist", 
                         assay_footprint = "1536 well"))

## ----eval = TRUE, message = FALSE----------------------------------------
tcplRegister(what = "acid", 
             flds = list(aid = 1,
                         acnm = "TOX21_ERa_BLA_Agonist_ratio"))
tcplRegister(what = "aeid", 
             flds = list(acid = c(1, 1), 
                         aenm = c("TOX21_ERa_BLA_Agonist_ratio_gain", 
                                  "TOX21_ERa_BLA_Agonist_ratio_loss"),
                         normalized_data_type = 
                         rep("percent_activity", 1),
                         export_ready = c(1, 1),
                         burst_assay = c(0, 0),
                         fit_all = c(0, 0)))

## ----eval = TRUE---------------------------------------------------------
data(chdat, package = "tcpl")
setDT(chdat)
head(chdat)

## ----eval = TRUE, message = FALSE----------------------------------------
## Register the unique chemicals
tcplRegister(what = "chid", 
             flds = chdat[, 
                        unique(.SD), 
                        .SDcols = c("casn", "chnm")])

## ----eval = TRUE, message = FALSE----------------------------------------
cmap <- tcplLoadChem()
tcplRegister(what = "spid", 
             flds = merge(chdat[ , list(spid, casn)], 
                          cmap[ , list(casn, chid)], 
                          by = "casn")[ , list(spid, chid)])

## ----eval = FALSE--------------------------------------------------------
#  grp1 <- cmap[chid %% 2 == 0, unique(chid)]
#  grp2 <- cmap[chid %% 2 == 1, unique(chid)]
#  tcplRegister(what = "clib",
#               flds = list(clib = "group_1", chid = grp1))

## ----eval = FALSE--------------------------------------------------------
#  tcplRegister(what = "clib",
#               flds = list(clib = "group_2", chid = grp2))

## ----eval = FALSE--------------------------------------------------------
#  tcplRegister(what = "clib",
#               flds = list(clib = "other", chid = 1:2))

## ----eval = FALSE--------------------------------------------------------
#  tcplLoadClib(field = "chid", val = 1:2)

## ----eval = TRUE---------------------------------------------------------
data(mcdat, package = 'tcpl')
setDT(mcdat)


## ----eval = TRUE, message = FALSE----------------------------------------
tcplRegister(what = "acsn", 
             flds = list(acid = 1, acsn = "TCPL-MC-Demo"))

## ----eval = TRUE, message = FALSE----------------------------------------
tcplWriteLvl0(dat = mcdat, type = "mc")


## ----eval = TRUE---------------------------------------------------------
tcplLoadData(lvl = 0, fld = "acid", val = 1, type = "mc")

## ----warning = FALSE, echo = FALSE---------------------------------------
Type <- c('SC', 'SC', 'MC', 'MC', 'MC', 'MC', 'MC', 'MC')
Level <- c('Lvl1', 'Lvl2', 'Lvl1', 'Lvl2', 'Lvl3', 'Lvl4', 'Lvl5', 'Lvl6')
InputID <- c('acid', 'aeid', 'acid', 'acid', 'acid', 'aeid', 'aeid', 'aeid')
MethodID <- c('aeid', 'aeid', 'N/A', 'acid', 'aeid', 'N/A', 'aeid', 'aeid')
Table <- data.frame(Type, Level, InputID, MethodID)
library(htmlTable)
htmlTable(Table,
         rnames = FALSE ,
        
          caption="Table 1: Processing checklist.",
          tfoot = " The Input ID column indicates the ID used for each processing step; Method ID indicates the ID used for assigning methods for data processing, when necessary. SC = single-concentration; MC = multiple-concentration.")
          

## ----eval= FALSE---------------------------------------------------------
#  ## For illustrative purposes, assign level 2 MC methods to
#  ## ACIDs 97, 98, and 99. First check for available methods.
#  mthds <- tcplMthdList(lvl = 2, type = "mc")
#  mthds[1:2]
#  ## Assign some methods to ACID 97, 98, and 99
#  tcplMthdAssign(lvl = 2,
#                 id = 97:99,
#                 mthd_id = c(3, 4, 2),
#                 ordr = 1:3,
#                 type = "mc")
#  tcplMthdLoad(lvl = 2, id = 97:99, type = "mc")
#  ## Methods can be cleared one at a time for the given id(s)
#  tcplMthdClear(lvl = 2, id = 99, mthd_id = 2, type = "mc")
#  tcplMthdLoad(lvl = 2, id = 99, type = "mc")
#  ## Or all methods can be cleared for the given id(s)
#  tcplMthdClear(lvl = 2, id = 97:98, type = "mc")
#  tcplMthdLoad(lvl = 2, id = 97:98, type = "mc")

## ----warning = FALSE, echo = FALSE---------------------------------------
output <- 
  matrix(c("1. bval.apid.nwlls.med", "2. resp.fc", "1. bval.apid.lowconc.med", "2. bval.apid.pwlls.med",
"3. resp.log2", "4. resp.mult.neg1", "3. resp.pc", "4. resp.multneg1 ", 
"1. bval.apid.lowconc.med", "2. resp.fc", "1. bval.spid.lowconc.med", "2. pval.apid.mwlls.med", 
"3. resp.log2", "4. \t", "3. resp.pc", "4. \t" ,
"1. none", "2. resp.log10", "1. none", "2. resp.multneg1",
"3. resp.blineshift.50.spid", "4. \t", "3. \t", "4. \t"), 
         ncol=4, byrow = TRUE)

library(htmlTable)
htmlTable(output,
         
         
          rgroup = c("Scheme 1",
                     "Scheme 2", "Scheme3"),
          n.rgroup = c(2,2),
          cgroup = c("Fold-Change", "\\%Control"),
          n.cgroup = c(2,2), 
          caption="Table 2: Example normalization method assignments.")
          

## ----warning = FALSE, echo = FALSE---------------------------------------
Level <- c(" Lvl 0", "Lvl 1  ", "Lvl 2  ")
Description <- c("Pre-processing: Vendor/dataset-specific pre-processing to organize heterogeneous raw data to the uniform format for processing by the *tcpl* package&dagger;",
                 "Normalize: Apply assay endpoint-specific normalization listed in the \'sc1_aeid\' table to the raw data to define response",
                 "Activity Call: Collapse replicates by median response, define the response cutoff based on methods in the \'sc2_aeid\' table, and determine activity"
                 )

output <- 
  data.frame(Level, Description)

library(htmlTable)
htmlTable(output,
        
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
      
        align = 'l',
        align.header = 'l',
        caption="Table 3: Summary of the tcpl single-concentration pipeline.",
        tfoot="&dagger; Level 0 pre-processing is outside the scope of this package")


## ----eval = FALSE--------------------------------------------------------
#  tcplLoadAeid(fld = "acid", val = 2)

## ----eval = FALSE--------------------------------------------------------
#  tcplMthdAssign(lvl = 1,
#                 id = 1:2,
#                 mthd_id = c(1, 11, 13),
#                 ordr = 1:3,
#                 type = "sc")
#  

## ----eval = FALSE--------------------------------------------------------
#  tcplMthdAssign(lvl = 1,
#                 id = 2,
#                 mthd_id = 16,
#                 ordr = 1,
#                 type = "sc")

## ----eval = FALSE--------------------------------------------------------
#  ## Do level 1 processing for acid 1
#  sc1_res <- tcplRun(id = 1, slvl = 1, elvl = 1, type = "sc")

## ----eval = FALSE--------------------------------------------------------
#  ## Assign a cutoff value of log2(1.2)
#  tcplMthdAssign(lvl = 2,
#                 id = 1,
#                 mthd_id = 3,
#                 type = "sc")

## ----eval = FALSE--------------------------------------------------------
#  ## Do level 2 processing for acid 1
#  sc2_res <- tcplRun(id = 1, slvl = 2, elvl = 2, type = "sc")

## ----warning = FALSE, echo = FALSE---------------------------------------
Level <- c("Lvl 0 ", "Lvl 1", "Lvl 2", "Lvl 3", "Lvl 4", "Lvl 5", "Lvl 6")
Description <- c("Pre-processing: Vendor/dataset-specific pre-processing to organize heterogeneous raw data to the uniform format for processing by the *tcpl* package&dagger;",
                 "Index: Defne the replicate and concentration indices to facilitate
all subsequent processing",
                 "Transform: Apply assay component-specifc transformations
listed in the \'mc2_acid\' table to the raw data to defne the
corrected data",
"Normalize: Apply assay endpoint-specifc normalization listed in
the \'mc3_aeid\' table to the corrected data to define response",
"Fit: Model the concentration-response data utilizing three
objective functions: (1) constant, (2) hill, and (3) gain-loss",
"Model Selection/Acitivty Call: Select the winning model, define
the response cutoff based on methods in the \'mc5_aeid\' table, and
determine activity",
"Flag: Flag potential false positive and false negative endings based
on methods in the \'mc6_aeid\' table"
                 )

output <- 
  data.frame(Level, Description)

library(htmlTable)
htmlTable(output,
        
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        align = 'l',
        align.header = 'l',
        caption="Table 4: Summary of the tcpl multiple-concentration pipeline.",
        tfoot="&dagger; Level 0 pre-processing is outside the scope of this package")


## ----eval = TRUE, message = FALSE----------------------------------------
## Do level 1 processing for acid 1
mc1_res <- tcplRun(id = 1, slvl = 1, elvl = 1, type = "mc")

## ----eval = TRUE---------------------------------------------------------
## Load the level 1 data and look at the cndx and repi values
m1dat <- tcplLoadData(lvl = 1, 
                      fld = "acid", 
                      val = 1, 
                      type = "mc")
m1dat <- tcplPrepOtpt(m1dat)
setkeyv(m1dat, c("repi", "cndx"))
m1dat[chnm == "Bisphenol A", 
      list(chnm, conc, cndx, repi)]

## ----eval = TRUE, warning = FALSE, message = FALSE, fig.width = 30, fig.height= 20----
tcplPlotPlate(dat = m1dat, apid = "4009721")


## ----eval = TRUE, message = FALSE----------------------------------------
tcplMthdAssign(lvl = 2,
                id =1,
                mthd_id = 1,
                ordr = 1, 
                type = "mc")

## ----eval = TRUE,  warning = FALSE, message = FALSE----------------------
## Do level 2 processing for acid 1
mc2_res <- tcplRun(id = 1, slvl = 2, elvl = 2, type = "mc")

## ----eval = TRUE---------------------------------------------------------
## Look at the assay endpoints for acid 1
tcplLoadAeid(fld = "acid", val = 1)

## ----eval = TRUE, message = FALSE----------------------------------------
tcplMthdAssign(lvl = 3, 
               id = 1:2,
               mthd_id = c(17, 9, 7),
               ordr = 1:3, type = "mc")


## ----eval = TRUE,warning = FALSE, message = FALSE------------------------
## Do level 3 processing for acid 1
mc3_res <- tcplRun(id = 1, slvl = 3, elvl = 3, type = "mc")

## ----eval = TRUE, message = FALSE, warning = FALSE-----------------------
## Do level 4 processing for aeids 1&2 and load the data
tcplMthdAssign(lvl = 4, id = 1:2, mthd_id = c(1,2), type = "mc" )
mc4_res <- tcplRun(id = 1:2, slvl = 4, elvl = 4, type = "mc")

## ----eval = TRUE---------------------------------------------------------
## Load the level 4 data
m4dat <- tcplPrepOtpt(tcplLoadData(lvl = 4, type = "mc"))
## List the first m4ids where the hill model converged
## for AEID 1
m4dat[hill == 1 & aeid == 1, head(m4id)]

## ----eval = TRUE, fig.width = 15, fig.height= 10-------------------------
## Plot a fit for m4id 7
tcplPlotM4ID(m4id = 7, lvl = 4)


## ----eval = TRUE, warning = FALSE----------------------------------------
## Assign a cutoff value of 6*bmad
tcplMthdAssign(lvl = 5,
               id = 1:2,
               mthd_id = 6,
               ordr = 1,
               type = "mc")

## ----eval = TRUE---------------------------------------------------------

par(family = "mono", mar = rep(1, 4), pty = "m")
plot.new()
plot.window(xlim = c(0, 30), ylim = c(-30, 100))
axis(side = 2, lwd = 2, col = "gray35")
rect(xleft = par()$usr[1],
     xright = par()$usr[2], 
     ybottom = -15, 
     ytop = 15,
     border = NA, 
     col = "gray45",
     density = 15, 
     angle = 45)
abline(h = 26, lwd = 3, lty = "dashed", col = "gray30")
tmp <- list(modl = "gnls", gnls_ga = 12, gnls_tp = 80, 
            gnls_gw = 0.18, gnls_lw = 0.7, gnls_la = 25)
tcplAddModel(pars = tmp, lwd = 3, col = "dodgerblue2")

abline(v = 8.46, lwd = 3, lty = "solid", col = "firebrick")
text(x = 8.46, y = par()$usr[4]*0.9, 
     font = 2, labels = "ACB", cex = 2, pos = 2, srt = 90)
abline(v = 10.24, lwd = 3, lty = "solid", col = "yellow2")
text(x = 10.24, y = par()$usr[4]*0.9, 
     font = 2, labels = "ACC", cex = 2, pos = 2, srt = 90)
abline(v = 12, lwd = 3, lty = "solid", col = "dodgerblue2")
text(x = 12, y = par()$usr[4]*0.9, 
     font = 2, labels = "AC50", cex = 2, pos = 2, srt = 90)

points(x = c(8.46, 10.24, 12), y = c(15, 26, 40),
       pch = 21, cex = 2, col = "gray30", lwd = 2,
       bg = c("firebrick", "yellow2", "dodgerblue2"))


## ----eval = TRUE---------------------------------------------------------
mc5_fit_categories <- fread(system.file("/example/mc5_fit_categories.csv",
  package = "tcpl"), 
  sep = ",", 
  header = TRUE)
tcplPlotFitc(mc5_fit_categories)

## ----eval = TRUE, warning = FALSE, message = FALSE-----------------------
## Do level 5 processing for aeids 1&2 and load the data
mc5_res <- tcplRun(id = 1:2, slvl = 5, elvl = 5, type = "mc")

## ----eval = TRUE, fig.width = 15, fig.height= 10-------------------------
tcplPlotM4ID(m4id = 4, lvl = 5)

## ----eval = TRUE---------------------------------------------------------
m5dat <- tcplLoadData(lvl = 5, type = "mc")
tcplPlotFitc(fitc = m5dat$fitc)

## ----eval = TRUE---------------------------------------------------------
head(m5dat[fitc == 21, 
           list(m4id, hill_tp, gnls_tp, 
                max_med, coff, hitc)])

## ----eval = TRUE, fig.width = 15, fig.height= 10-------------------------
tcplPlotM4ID(m4id = 3, lvl = 5)

## ----eval = TRUE, warning = FALSE, message = FALSE, error = TRUE---------
## Clear old methods
tcplMthdClear(lvl = 6, id = 1:2, type = "mc")
tcplMthdAssign(lvl = 6, id = 1:2,
               mthd_id = c(6:8, 10:12, 15:16),
               type = "mc")
tcplMthdLoad(lvl = 6, id = 1, type = "mc")

## ----eval = TRUE, warning = FALSE, error = TRUE--------------------------

## Do level 6 processing
mc6_res <- tcplRun(id = 1:2, slvl = 5, elvl = 6, type = "mc")

## ----eval = TRUE, warning = FALSE----------------------------------------
m6dat <- tcplLoadData(lvl = 6, type = "mc")

## ----eval = TRUE---------------------------------------------------------
m6dat[m4id == 6]

## ----eval = TRUE, fig.width = 15, fig.height= 10-------------------------
tcplPlotM4ID(m4id = 6, lvl = 6)

## ----eval = FALSE, echo = FALSE, message = FALSE--------------------------

knitr::opts_chunk$set(root.dir = normalizePath(".."))

mc0 <- data.table(m0id = "", acid = "", spid = "", apid = "", rowi = "",
                  coli = "", wllt = "", wllq = "", conc = "", rval = "",
                  srcf ="", created_date = "", modified_date = "", 
                  modified_by = "")
mc1 <- data.table(m1id = "", m0id = "", acid = "", cndx = "", repi = "", created_date = "",
                  modified_date = "", modified_by = "")
mc2 <- data.table(m2id = "", m0id = "", acid = "", m1id = "", cval = "", created_date = "",
                  modified_date = "", modified_by = "")
mc3 <- data.table(m3id = "", aeid = "", m0id = "", acid = "", m1id = "", m2id = "", bval = "", pval = "",
                  logc = "", resp = "", created_date = "", modified_date = "", modified_by = "")

mc4 <- data.table(m4id = "", aeid = "",	spid = "",	bmad = "",	resp_max = "",	resp_min = "",	max_mean = "",	max_mean_conc = "",	max_med = "",	max_med_conc = "",	logc_max = "",	logc_min = "",	cnst = "",	hill = "",	hcov = "",	gnls = "",	gcov = "",	cnst_er = "",	cnst_aic = "",	cnst_rmse = "",	cnst_prob = "",	hill_tp = "",	hill_tp_sd = "",	hill_ga = "",	hill_ga_sd = "",	hill_gw = "",	hill_gw_sd = "",	hill_er = "",	hill_er_sd = "",	hill_aic = "",	hill_rmse = "",	hill_prob = "",	gnls_tp = "",	gnls_tp_sd = "",	gnls_ga = "",	gnls_ga_sd = "",	gnls_gw = "",	gnls_gw_sd = "",	gnls_la = "",	gnls_la_sd = "",	gnls_lw = "",	gnls_lw_sd = "",	gnls_er = "",	gnls_er_sd = "",	gnls_aic = "",	gnls_rmse = "",	gnls_prob = "",	nconc = "",	npts = "",	nrep = "",	nmed_gtbl = "",	tmpi = "",	created_date = "",	modified_date = "",	modified_by = "")

mc5 <- data.table(m5id = "",	m4id = "",	aeid = "",	modl = "",	hitc = "",	fitc = "",	coff = "",	actp = "",	modl_er = "",	modl_tp = "",	modl_ga = "",	modl_gw = "",	modl_la = "",	modl_lw = "",	modl_prob = "",	modl_rmse = "",	modl_acc = "",	modl_acb = "",	modl_ac10 = "",	created_date = "",	modified_date = "",	modified_by = "")

mc6 <- data.table (m6id = "",	m5id = "",	m4id = "",	aeid = "",	mc6_mthd_id = "",	flag = "",	fval = "",	fval_unit = "",	created_date = "",	modified_date = "",	modified_by = "")

write.csv(mc0, file = "../inst/csv/mc0.csv", row.names = F)
write.csv(mc1, file = "../inst/csv/mc1.csv", row.names = F)
write.csv(mc2, file = "../inst/csv/mc2.csv", row.names = F)
write.csv(mc3, file = "../inst/csv/mc3.csv", row.names = F)
write.csv(mc4, file = "../inst/csv/mc4.csv", row.names = F)
write.csv(mc5, file = "../inst/csv/mc5.csv", row.names = F)
write.csv(mc6, file = "../inst/csv/mc6.csv", row.names = F)
write.csv(mc6, file = "../inst/csv/mc6.csv", row.names = F)

