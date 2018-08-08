## ----eval = TRUE, echo = FALSE, message = FALSE, warning = FALSE---------

library(htmlTable)
library(data.table)

#devtools::install_git('http://bitbucket.zn.epa.gov/scm/tox/tcpl.git', branch='master')

library(tcpl)

## ----eval=TRUE, message=FALSE--------------------------------------------
library(data.table)
library(tcpl)
## Store the path for the tcpl directory for loading data
pkg_dir <- system.file(package = "tcpl")


## ----eval = FALSE--------------------------------------------------------
#  tcpl (v1.3) loaded with the following settings:
#    TCPL_DB:    C:/Users/mshobair/R-3.4.4/library/tcpl/csv
#    TCPL_USER:  NA
#    TCPL_HOST:  NA
#    TCPL_DRVR:  tcplLite
#  Default settings stored in TCPL.conf. See ?tcplConf for more information.

## ----eval = FALSE--------------------------------------------------------
#  tcplConf(drvr = "MySQL",
#           user = "root",
#           pass = "",
#           host = "localhost",
#           db   = "invitrodb")
#  

## ----eval = FALSE--------------------------------------------------------
#  tcplConf(drvr = "tcplLite",
#           user = "",
#           pass = "",
#           host = "",
#           db   = "/localdirectory/csv/")
#  

## ----eval = TRUE---------------------------------------------------------
## Add a new assay source, call it CTox,
## that produced the data
tcplRegister(what = "asid", flds = list(asid = 1, 
                      asnm = "CTox"))

## ----eval = TRUE---------------------------------------------------------
tcplLoadAsid()

## ----eval = TRUE---------------------------------------------------------
tcplRegister(what = "aid", 
             flds = list(asid = 1, 
                         anm = "TOX21_ERa_BLA_Agonist", 
                         assay_footprint = "1536 well"))

## ----eval = TRUE---------------------------------------------------------
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
ch <- fread(system.file("/example/chdat.csv",
                        package = "tcpl"), 
                        sep = ",", 
                        header = TRUE)
head(ch)

## ----eval = TRUE---------------------------------------------------------
## Register the unique chemicals
tcplRegister(what = "chid", 
             flds = ch[ , 
                        unique(.SD), 
                        .SDcols = c("casn", "chnm")])

## ----eval = TRUE---------------------------------------------------------
cmap <- tcplLoadChem()
tcplRegister(what = "spid", 
             flds = merge(ch[ , list(spid, casn)], 
                          cmap[ , list(casn, chid)], 
                          by = "casn")[ , list(spid, chid)])

## ----eval = TRUE---------------------------------------------------------
grp1 <- cmap[chid %% 2 == 0, unique(chid)]
grp2 <- cmap[chid %% 2 == 1, unique(chid)]
tcplRegister(what = "clib", 
             flds = list(clib = "group_1", chid = grp1))

## ----eval = TRUE---------------------------------------------------------
tcplRegister(what = "clib", 
             flds = list(clib = "group_2", chid = grp2))

## ----eval = TRUE, error=TRUE, message = FALSE, warning = FALSE-----------
tcplRegister(what = "clib", 
             flds = list(clib = "other", chid = 1:2))

## ----eval = FALSE, error=TRUE, message = FALSE, warning = FALSE----------
#  tcplLoadClib(field = "chid", val = 1:2)

## ----eval = TRUE---------------------------------------------------------
mcdat <- fread(system.file("/example/mcdat.csv",
  package = "tcpl"), 
  sep = ",", 
  header = TRUE)


## ----eval = TRUE---------------------------------------------------------
tcplRegister(what = "acsn", 
             flds = list(acid = 1, acsn = "TCPL-MC-Demo"))

## ----eval = TRUE---------------------------------------------------------
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


## ----eval = TRUE---------------------------------------------------------
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
m1dat[chnm == "4-Chloro-1,2-diaminobenzene", 
      list(chnm, conc, cndx, repi)]

## ----eval = TRUE, warning = FALSE, message = FALSE, fig.width = 30, fig.height= 20----
tcplPlotPlate(dat = m1dat, apid = "4009721")


## ----eval = TRUE---------------------------------------------------------
tcplMthdAssign(lvl = 2,
                id =1,
                mthd_id = 1,
                ordr = 1, 
                type = "mc")

## ----eval = TRUE,  warning= FALSE----------------------------------------
## Do level 2 processing for acid 1
mc2_res <- tcplRun(id = 1, slvl = 2, elvl = 2, type = "mc")

## ----eval = TRUE---------------------------------------------------------
## Look at the assay endpoints for acid 1
tcplLoadAeid(fld = "acid", val = 1)

## ----eval = TRUE---------------------------------------------------------
tcplMthdAssign(lvl = 3, 
               id = 1:2,
               mthd_id = c(17, 9, 7),
               ordr = 1:3, type = "mc")


## ----eval = TRUE,  warning = FALSE---------------------------------------
## Do level 3 processing for acid 1
mc3_res <- tcplRun(id = 1, slvl = 3, elvl = 3, type = "mc")

## ----eval = TRUE, warning = FALSE----------------------------------------
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
## Plot a fit for m4id 15536
tcplPlotM4ID(m4id = 15536, lvl = 4)


## ----eval = TRUE---------------------------------------------------------
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
mc5_fit_categories <- fread(system.file("/csv/mc5_fit_categories.csv",
  package = "tcpl"), 
  sep = ",", 
  header = TRUE)
tcplPlotFitc(mc5_fit_categories)

## ----eval = TRUE, warning = FALSE----------------------------------------
## Do level 5 processing for aeids 1&2 and load the data
mc5_res <- tcplRun(id = 1:2, slvl = 5, elvl = 5, type = "mc")

## ----eval = TRUE, fig.width = 15, fig.height= 10-------------------------
tcplPlotM4ID(m4id = 15444, lvl = 5)

## ----eval = TRUE---------------------------------------------------------
m5dat <- tcplLoadData(lvl = 5, type = "mc")
tcplPlotFitc(fitc = m5dat$fitc)

## ----eval = TRUE---------------------------------------------------------
head(m5dat[fitc == 21, 
           list(m4id, hill_tp, gnls_tp, 
                max_med, coff, hitc)])

## ----eval = TRUE, fig.width = 15, fig.height= 10-------------------------
tcplPlotM4ID(m4id = 13, lvl = 5)

## ----eval = TRUE, warning = FALSE, message = FALSE-----------------------
## Clear old methods
tcplMthdClear(lvl = 6, id = 1:2, type = "mc")
tcplMthdAssign(lvl = 6, id = 1:2,
               mthd_id = c(6:8, 10:12, 15:16),
               type = "mc")
tcplMthdLoad(lvl = 6, id = 1, type = "mc")

## ----eval = TRUE, warning = FALSE----------------------------------------
## Do level 6 processing
mc6_res <- tcplRun(id = 1:2, slvl = 6, elvl = 6, type = "mc")
m6dat <- tcplLoadData(lvl = 6, type = "mc")

## ----eval = TRUE---------------------------------------------------------
m6dat[m4id == 15529]

## ----eval = TRUE, fig.width = 15, fig.height= 10-------------------------
tcplPlotM4ID(m4id = 15529, lvl = 6)

## ----eval = TRUE---------------------------------------------------------
## Load mc tables from levels of interest
mc4 <- tcplLoadData(lvl = 4, type = "mc")
mc5 <- tcplLoadData(lvl = 5, type = "mc")
mc6 <- tcplLoadData(lvl = 6, type = "mc")

## ----eval = TRUE---------------------------------------------------------
## Find all sample IDs (spids) associated with Bisphenol A
chnm <- 'Bisphenol A'
ch <- fread(system.file("/csv/chemical.csv",
                   package = "tcpl"),
                   sep = ",",
                   header = TRUE)
chem <- tcplLoadChem('chnm', chnm)
dat4spid <- mc4[mc4$spid %in% chem$spid]

## ----eval = TRUE---------------------------------------------------------
dat5spid <- mc5[mc5$m4id %in% dat4spid$m4id]
dat5hit <- dat5spid[hitc == 1]

## ----eval = TRUE---------------------------------------------------------
mc6_flags <- mc6[ , .( flag = paste(flag, collapse=";")),
                    by = m5id]
dat5dat6 <- merge(x = mc6_flags,
                  y = dat5hit,
                  by = "m5id", all.y = TRUE)

## ----eval = FALSE--------------------------------------------------------
#  tcplConf(
#            user = 'XXXXXXX',
#            pass = 'XXXXXXX',
#            host = 'XXXXXXX',
#            db = 'invitrodb',
#            drvr = 'MySQL')

## ----eval = FALSE--------------------------------------------------------
#  chnm <- 'Bisphenol A'
#  chem <- tcplLoadChem('chnm', chnm)

## ----eval = FALSE--------------------------------------------------------
#  dat5 <- tcplPrepOtpt(tcplLoadData
#                       (lvl = 5, fld = 'spid',
#                         val = chem$spid, type = 'mc'))
#  ## For positives only, hit call (hitc) should equal 1
#  dat5 <- dat5[hitc == 1]
#  dat6 <- tcplPrepOtpt(tcplLoadData
#                       (lvl = 6, fld = 'spid', val = chem$spid,
#                         type = 'mc'))
#  mc6_mthds <- dat6[ , .( mc6_mthd_id =
#                            paste(mc6_mthd_id, collapse = ",")),
#                     by = m4id]
#  mc6_flags <- dat6[ , .( flag =
#                            paste(flag, collapse = ";")),
#                     by = m4id]

## ----eval = FALSE--------------------------------------------------------
#  m4ids <- dat5[ , m4id]
#  graphics.off()
#  pdf(file = file.path(getwd(),
#                     paste("mc6",
#                           paste(chnm,collapse = "."),
#                           format(Sys.Date(),
#                                  "%y%m%d.pdf"),
#                           sep = "_")),
#      height = 6,
#      width = 10,
#      pointsize = 10)
#  tcplPlotM4ID(m4ids, lvl = 6)
#  graphics.off()

## ----eval = FALSE--------------------------------------------------------
#  ## List the assay source IDs
#  tcplLoadAsid()
#  ## Find the assay source (NVS)
#  nvs.assays <- tcplLoadAeid(fld='asid', val = 5)
#  ## Find the assay name (hTRa)
#  aeids <- nvs.assays[grep("hTRa", aenm)]$aeid
#  ## Load the mc5 to determine hit call and summary information
#  dat5 <- tcplPrepOtpt(tcplLoadData(lvl = 5, type = 'mc',
#                                    fld = 'aeid', val = aeids))
#  dat5 <- dat5[hitc == 1]
#  ## Make the level 6 plots for the positive responses
#  assay <- 'NVS_NR_hTRa_Antagonist'
#  m4ids <- dat5[ , m4id]
#  graphics.off()
#  pdf(file = file.path(getwd(),
#                     paste("mc6",
#                           paste(assay,collapse = "."),
#                           format(Sys.Date(),
#                                  "%y%m%d.pdf"),
#                           sep = "_")),
#      height = 6,
#      width = 10,
#      pointsize = 10)
#  tcplPlotM4ID(m4ids, lvl = 6)
#  graphics.off()

## ----eval = FALSE--------------------------------------------------------
#  spids <- unique(dat5[,spid])
#  ## logc = log10concentration, starting with micromolar
#  ## units (x-axis), resp = normalized response value (y-axis)
#  mc3 <- tcplPrepOtpt(tcplLoadData
#                      (lvl = 3, type = 'mc', fld = 'spid',
#                        val = spids))
#  mc3 <- mc3[aeid %in% aeids]

## ----eval = FALSE--------------------------------------------------------
#  library(ggplot2)
#  
#  graphics.off()
#  pdf(file = file.path(getwd(),
#                     paste("mc3",
#                           paste(assay,collapse = "."),
#                           format(Sys.Date(),
#                                  "%y%m%d.pdf"),
#                           sep = "_")),
#      height = 6,
#      width = 10,
#      pointsize = 10)
#  
#  by(mc3, mc3$spid, function (x){
#    ggplot(x, aes(x = logc, y = resp), tab) +
#      geom_point(aes(group=spid, color=spid)) +
#      theme(axis.ticks.x = element_blank(),
#            axis.text.x = element_text(size=rel(0.5)),
#            axis.ticks.y = element_blank(),
#            axis.text.y = element_blank()) +
#      xlab("logc") + ylab("resp")
#  }
#  )
#  graphics.off()

## ----warning = FALSE, echo = FALSE---------------------------------------
Field <- c("s0id ", "acid", "spid", "cpid", "apid", "rowi", "coli", "wllt", "wllq", "conc", "rval", "srcf")
Description <- c("Level 0 ID",
                 "Assay component ID",
                 "Sample ID",
                 "Chemical plate ID",
                 "Assay plate ID",
                 "Assay plate row index",
                 "Assay plate column index",
                 "Well type&dagger;",
                 "1 if the well quality was good, else 0&ddagger;",
                 "Concentration is micromolar",
                 "Raw assay component value/readout from vendor",
                 "Filename of the source file containing the data"
                 )

output <- 
  data.frame(Field, Description)

library(htmlTable)
htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        caption="Table 5: Fields in sc0 table.",
        tfoot="&dagger;Information about the different well types is available in Appendix B.")


## ----warning = FALSE, echo = FALSE---------------------------------------
Field <- c("s1id ", "s0id", "acid", "aeid", "logc", "bval", "pval", "resp")
Description <- c("Level 1 ID",
                 "Level 0 ID",
                 "Assay component ID",
                 "Assay component endpoint ID",
                 "Log base 10 concentration",
                 "Baseline value",
                 "Positive control value",
                 "Normalized response value"
              
                 )

output <- 
  data.frame(Field, Description)

library(htmlTable)
htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        caption="Table 6: Fields in sc1 table."
)


## ----warning = FALSE, echo = FALSE---------------------------------------
Field <- c("aeid ", "s0id", "s1id", "s2id")
Description <- c("Assay component endpoint ID",
                 "Level 0 ID",
                 "Level 1 ID",
                 "Level 2 ID"
              
                 )

output <- 
  data.frame(Field, Description)

library(htmlTable)
htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        caption="Table 7: Fields in sc2_agg table."
)


## ----warning = FALSE, echo = FALSE---------------------------------------
Field <- c("s2id ", "aeid", "spid", "bmad", "max_med", "hitc", "coff", "tmpi")
Description <- c("Level 2 ID",
                 "Assay component endpoint ID",
                 "Sample ID",
                 "Baseline median absolute deviation",
                 "Maximum median response value",
                 "Hit-/activity-call, 1 if active, 0 if inactive",
                 "Efficacy cutoff value",
                 "Ignore, temporary index used for uploading purposes"
                 )

output <- 
  data.frame(Field, Description)

library(htmlTable)
htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        caption="Table 8: Fields in sc2 table."
)


## ----warning = FALSE, echo = FALSE---------------------------------------
Field <- c("m1id", "m0id", "acid", "cndx", "repi")
Description <- c("Level 1 ID",
                 "Level 0 ID",
                 "Assay component ID",
                 "Concentration index",
                 "Replicate index"
                 )

output <- 
  data.frame(Field, Description)

library(htmlTable)
htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        caption="Table 9: Fields in mc1 table."
)


## ----warning = FALSE, echo = FALSE---------------------------------------
Field <- c("m2id", "m0id", "acid", "m1id", "cval")
Description <- c("Level 2 ID",
                 "Level 0 ID",
                 "Assay component ID",
                 "Level 1 ID",
                 "Corrected value"
                 )

output <- 
  data.frame(Field, Description)

library(htmlTable)
htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        caption="Table 10: Fields in mc2 table."
)


## ----warning = FALSE, echo = FALSE---------------------------------------
Field <- c("m3id", "aeid", "m0id", "acid", "m1id", "m2id", "bval", "pval", "logc", "resp")
Description <- c("Level 3 ID",
                 "Assay endpoint ID",
                 "Level 0 ID",
                 "Assay component ID",
                 "Level 1 ID",
                 "Level 2 ID",
                 "Baseline value",
                 "Positive control value",
                 "Log base 10 concentration",
                 "Normalized response value"
                 )

output <- 
  data.frame(Field, Description)

library(htmlTable)
htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        caption="Table 11: Fields in mc3 table."
)


## ----warning = FALSE, echo = FALSE---------------------------------------
Field <- c("aeid", "m0id", "m1id", "m2id", "m3id", "m4id")
Description <- c(
   "Assay endpoint ID","Level 0 ID",
                 "Level 1 ID",
                 "Level 2 ID",
                 "Level 3 ID",
                 "Level 4 ID"
                 
                 )

output <- 
  data.frame(Field, Description)

library(htmlTable)
htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        caption="Table 12: Fields in mc4_agg table."
)


## ----warning = FALSE, echo = FALSE---------------------------------------
Field <- c("m4id", "aeid", "spid", "bmad", "resp_max", "resp_min", "max_mean", "max_mean_conc", "max_med", "max_med_conc", "logc_max", "logc_min", "cnst", "hill", "hcov", "gnls", "gcov", "cnst_er", "cnst_aic", "cnst_rmse", "cnst_prob", "hill_tp", "hill_tp_sd", "hill_ga", "hill_ga_sd")

Description <- c("Level 4 ID",
                 "Assay endpoint ID",
                 "Sample ID",
                 "Baseline median absolute deviation",
                 "Maximum response value",
                 "Minimum response value",
                 "Maximum mean response value",
                 "Log concentration at *max_mean*",
                 "Maximum median response value",
                 "Log concentration at *max_med*",
                 "Maximum log concentration tested",
                 "Minimum log concentration tested",
                 "1 if the constant model converged, 0 if it failed to converge, N/A if series had less than four concentrations",
                  "1 if the Hill model converged, 0 if it failed to converge, N/A if series had less than four concentrations or if *max_med* < *3bmad*",
                "1 if the Hill model Hessian matrix could be inverted, else 0",
      "1 if the gain-loss model converged, 0 if it failed to converge, N/A
if series had less than four concentrations or if *max_med* < *3bmad*",
                "1 if the gain-loss model Hessian matrix could be inverted, else 0",
                "Scale term for the constant model",
                "AIC for the constant model",
                "RMSE for the constant model",
                "Probability the constant model is the true model",
                "Top asymptote for the Hill model",
                "Standard deviation for *hill_tp*",
                "AC~50~ for the Hill model",
                 "Standard deviation for *hill_ga* "
                 )

output <- 
  data.frame(Field, Description)

library(htmlTable)
htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        caption="Table 13: Fields in mc4 table (Part 1)."
)


## ----warning = FALSE, echo = FALSE---------------------------------------
Field <- c("hill_gw", "hill_gw_sd", "hill_er", "hill_er_sd", "hill_aic", "hill_rmse", "hill_prob", "gnls_tp", "gnls_tp_sd", "gnls_ga", "gnls_ga_sd", "gnls_gw", "gnls_gw_sd", "gnls_la", "gnls_la_sd", "gnls_lw", "gnls_lw_sd", "gnls_er", "gnls_er_sd", "gnls_aic", "gnls_rmse", "gnls_prob", "nconc", "npts", "nrep", "nmed_gtbl", "tmpi")

Description <- c("Hill coefficient",
                 "Standard deviation for *hill_gw*",
                 "Scale term for the Hill model",
                 "Standard deviation for *hill_er*",
                 "AIC for the Hill model",
                 "RMSE for the Hill model",
                 "Probability the Hill model is the true model",
                 "Top asymptote for the gain-loss model",
                 "Standard deviation for *gnls_tp*",
                 "AC~50~ in the gain direction for the gain-loss model",
                 "Standard deviation for *gnls_ga*",
                 "Hill coefficient in the gain direction",
                 "Standard deviation for *gnls_gw* ",
                 "AC~50~ in the loss direction for the gain-loss model",
                 "Standard deviation for *gnls_la*",
                "Hill coefficient in the loss direction",
                "Standard deviation for the *gnls_lw*",
                 "Scale term for the gain-loss model",
                 "Standard deviation for *gnls_er*",
                 "AIC for the gain-loss model",
                 "RMSE for the gain-loss model",
                "Probability the gain-loss model is the true model",
                "Number of concentrations tested ",
                "Number of points in the concentration series",
                "Number of replicates in the concentration series",
                "Number of median values greater than *3bmad*",
                "Ignore, temporary index used for uploading purposes"
                 )

output <- 
  data.frame(Field, Description)

library(htmlTable)
htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        caption="Table 14: Fields in mc4 table (Part 2)."
)


## ----warning = FALSE, echo = FALSE---------------------------------------
Field <- c("m5id", "m4id", "aeid", "modl", "hitc", "fitc", "coff", "actp", "modl_er", "modl_tp", "modl_ga", "modl_gw", "modl_la", "modl_lw", "modl_prob", "modl_rmse", "modl_acc", "modl_acb", "modl_ac10")

Description <- c("Level 5 ID",
                 "Level 4 ID",
                 "Assay endpoint ID",
                 "Winning model: \"cnst\", \"hill\", or \"gnls\"",
                 "Hit-/activity-call, 1 if active, 0 if inactive, -1 if cannot determine",
                 "Fit category",
                 "Effcacy cutoff value",

                "Activity probability (1 - *const_prob*)",
                "Scale term for the winning model",
                "Top asymptote for the winning model",
                "Gain AC~50~ for the winning model",
              "Gain Hill coefficient for the winning model",
              "Loss AC~50~ for the winning model",
       "Loss Hill coefficient for the winning model",
       "Probability for the winning model",
       "RMSE for the winning model",
       "Activity concentration at cutoff for the winning model",
       "Activity concentration at baseline for the winning model",
       "AC10 for the winning model"
                 )

output <- 
  data.frame(Field, Description)

library(htmlTable)
htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        caption="Table 15: Fields in mc5 table."
)


## ----warning = FALSE, echo = FALSE---------------------------------------
Field <- c("m6id", "m5id", "m4id", "aeid", "m6_mthd_id", "flag", "fval", "fval_unit")

Description <- c("Level 6 ID",
                 "Level 5 ID",
                 "Level 4 ID",
                 "Assay endpoint ID",
                 "Level 6 method ID",
                 "Text output for the level 6 method",
                 "Value from the flag method, if applicable",
                 "Units for *fval* , if applicable" )

output <- 
  data.frame(Field, Description)

library(htmlTable)
htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        caption="Table 16: Fields in mc6 table."
)


## ----warning = FALSE, echo = FALSE---------------------------------------
Field <- c("m4id", "Aeid", "Aenm", "Asid", "Acid", "Hit_pct", "Total_hitc", "Modl_ga_min", "Modl_ga_max", "Modl_ga_med", "Modl_gw_med", "Modl_ga_delta", "Cnst_pct", "Hill_pct", "Gnls_pct")

Description <- c("Level 4 ID",
                 "Assay endpoint ID",
                 "Assay endpoint name",
                 "Assay source ID",
                 "Assay component ID",
                 "Total percent of hit calls made after 1000 bootstraps",
                 "Total number of hit calls made after 1000 bootstraps",
                 "Low bound of the 95% confidence interval for the AC~50~",
                 "Upper bound of the 95% confidence interval for the AC~50~",
                 "Median AC~50~ after 1000 bootstraps",
                 "Median gain Hill coefficient for 1000 bootstraps",
                 "AC~50~ confidence interval width in log units",
                 "Percent of 1000 bootstraps that the constant model was selected as the winning model", 
                 "Percent of 1000 bootstraps that the Hill model was selected as the winning model",
                 "Percent of 1000 bootstraps that the gain-loss was selected as the winning model")

output <- 
  data.frame(Field, Description)

library(htmlTable)
htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        caption="Table 17: Fields in mc7 table."
)


## ----warning = FALSE, echo = FALSE---------------------------------------
Field <- c("assay", "assay_component", "assay_component_endpoint", "assay_component_map", "assay_reagent**", "assay_reference**", "assay_source", "chemical", "chemical_library", "citations**", "gene", "intended target**", "mc5_fit_categories", "organism**", "sample", "technological_target**")

Description <- c("Assay-level annotation",
                 "Assay component-level annotation",
                 "Assay endpoint-level annotation",
                 "Assay component source names and their corresponding assay component ids",
                 "Assay reagent information",
                 "Map of citations to assay",
                 "Assay source-level annotation",
                 "List of chemicals and associated identifiers",
                 "Map of chemicals to different chemical libraries",
                 "List of citations",
                 "Gene** identifers and descriptions",
                 "Intended assay target at the assay endpoint level",
                 "The level 5 fit categories",
                "Organism identifiers and descriptions",
                "Sample ID information and chemical ID mapping",
                "Technological assay target at the assay component level")

output <- 
  data.frame(Field, Description)

library(htmlTable)
htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        caption="Table 18: List of annotation tables.",
        tfoot = "** indicates tables not currently used by the *tcpl* package",
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em '
)


## ----warning = FALSE, echo = FALSE---------------------------------------
Field <- c("aid", "asid", "assay_name", "assay_desc", "timepoint_hr", "assay_footprint")

Description <- c("Assay ID",
                 "Assay source ID",
                 "Assay name (abbreviated \"anm\" within the package)",
                 "Assay description",
                 "Treatment duration in hours",
                 "Microtiter plate size&dagger;")

output <- 
  data.frame(Field, Description)

library(htmlTable)
htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        caption="Table 19: Fields in assay.",
        tfoot = "&dagger; discussed further in the \"Register and Upload New Data\" section",
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em '
)


## ----warning = FALSE, echo = FALSE---------------------------------------
Field <- c("acid", "aid", "assay_component_name", "assay_component_desc")

Description <- c("Assay component ID",
                 "Assay ID",
                 "Assay component name (abbreviated \"acnm\" within the package)",
                 "Assay component description"
                 )

output <- 
  data.frame(Field, Description)

library(htmlTable)
htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        caption="Table 20: Fields in assay_component."
)


## ----warning = FALSE, echo = FALSE---------------------------------------
Field <- c("asid", "assay_source_name", "assay_source_long_name", "assay_source_description")

Description <- c("Assay source ID",
                 "Assay source name (typically an abbreviation of the
assay_source_long_name, abbreviated \"asnm\" within the package)",
                 "The full assay source name", 
                 "Assay source description"
                 )

output <- 
  data.frame(Field, Description)

library(htmlTable)
htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        caption="Table 21: Fields in assay_source."
)


## ----warning = FALSE, echo = FALSE---------------------------------------
Field <- c("aeid", "acid", "assay_component_endpoint_name", "assay_component_endpoint_desc", "export_ready", "normalized_data_type", "burst_assay", "fit_all")

Description <- c("Assay component endpoint ID",
                 "Assay component ID",
                 "Assay component endpoint name (abbreviated \"aenm\" within the
package)", 
                 "Assay component endpoint description",
"0 or 1, used to flag data as \"done\"",
                 "The units of the normalized data&dagger;",
                 "0 or 1, 1 indicates the assay results should be used in calculating the burst z-score",
                 "0 or 1, 1 indicates all results should be fit, regardless of whether the *max_med* surpasses *3bmad*"
                 )

output <- 
  data.frame(Field, Description)

library(htmlTable)
htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        caption="Table 22: Fields in assay_component_endpoint.",
        tfoot = "&dagger; discussed further in the \"Register and Upload New Data\" section"
)


## ----warning = FALSE, echo = FALSE---------------------------------------
Field <- c("acid", "acsn")

Description <- c("Assay component ID",
                 "Assay component source name"
                 )

output <- 
  data.frame(Field, Description)

library(htmlTable)
htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        caption="Table 23: Fields in assay_component_map table."
)


## ----warning = FALSE, echo = FALSE---------------------------------------
Field <- c("chid", "casn", "chnm")

Description <- c("Chemical ID&dagger;",
                 "CAS Registry Number",
                 "Chemical name"
                 )

output <- 
  data.frame(Field, Description)

library(htmlTable)
htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        caption="Table 24: Fields in chemical table." ,
        tfoot = "&dagger; this is the DSSTox GSID within the ToxCast data, but can be any integer and will be auto-generated (if not explicitly defined) for newly registered
chemicals"
)


## ----warning = FALSE, echo = FALSE---------------------------------------
Field <- c("chid", "clib")

Description <- c("Chemical ID",
                 "Chemical library"
                 )

output <- 
  data.frame(Field, Description)

library(htmlTable)
htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        caption="Table 25: Fields in chemical_library table." 
     
)


## ----warning = FALSE, echo = FALSE---------------------------------------
Field <- c("fitc", "parent_fitc", "name", "xloc", "yloc")

Description <- c("Fit category",
                 "Parent fit category",
                 "Fit category name" ,
                 "x-axis location for plotting purposes",
                 "y-axis location for plotting purposes"
                 )

output <- 
  data.frame(Field, Description)

library(htmlTable)
htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        caption="Table 26: Fields in mc5_fit_categories." 
     
)


## ----warning = FALSE, echo = FALSE---------------------------------------
Field <- c("spid", "chid", "stkc", "stkc_unit", "tested_conc_unit", 
           "spid_legacy")

Description <- c("Sample ID",
                 "Chemical ID",
                 "Stock concentration" ,
                 "Stock concentration unit",
                 "The concentration unit for the concentration values in the data-containing tables",
                 "A place-holder for previous sample ID strings"
                 )

output <- 
  data.frame(Field, Description)

library(htmlTable)
htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        caption="Table 27: Fields in sample table." 
)


## ----warning = FALSE, echo = FALSE---------------------------------------
Field <- c("acsn", "spid", "cpid", "apid", "rowi", 
           "coli", "wllt", "wllq", "conc", "rval", "srcf")

Description <- c("Assay component source name",
                 "Sample ID",
                 "Chemical plate ID" ,
                 "Assay plate ID",
                 "Assay plate row index, as an integer",
                 "Assay plate column index, as an integer",
                 "Well type",
                 "1 if the well quality was good, else 0",
                 "Concentration in micromolar",
                 "Raw assay component value/readout from vendor",
                 "Filename of the source file containing the data"
                 )
`N/A` <- c("No", "No", "Yes", "Yes","Yes","Yes", "No", "No", "No&dagger;", "Yes&ddagger;", "No")

output <- 
  data.frame(Field, Description, `N/A`)

library(htmlTable)
htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        
          caption="Table 28: Required felds in level 0 pre-processing." ,
          tfoot = "The N/A column indicates whether the field can be N/A in the pre-processed data.
 &dagger;Concentration can be N/A for control values only tested at a single
concentration. Concentration cannot be N/A for any test compound (well
type of \"t\") data.
 &ddagger;If the raw value is N/A, well type has to be 0."
)


## ----warning = FALSE, echo = FALSE---------------------------------------
`Well Type` <- c("t", "c", "p", "n", "m", 
           "o", "b", "v")

Description <- c("Test compound",
                 "Gain-of-signal control in multiple concentrations",
                 "Gain-of-signal control in single concentration" ,
                 "Neutral/negative control",
                 "Loss-of-signal control in multiple concentrations",
                 "Loss-of-signal control in single concentration",
                 "Blank well",
                 "Viability control"
                 )


output <- 
  data.frame(`Well Type`, Description)

library(htmlTable)
htmlTable(output,
        align = 'l',
        align.header = 'l',
        rnames = FALSE  ,
        css.cell =  ' padding-bottom: 5px;  vertical-align:top; padding-right: 10px;min-width: 5em ',
        caption="Table 29: Well types" 
)


