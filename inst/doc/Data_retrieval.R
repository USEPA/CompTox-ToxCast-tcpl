## ----eval = FALSE--------------------------------------------------------
#  ## Load mc tables from levels of interest
#  mc4 <- tcplLoadData(lvl = 4, type = "mc")
#  mc5 <- tcplLoadData(lvl = 5, type = "mc")
#  mc6 <- tcplLoadData(lvl = 6, type = "mc")

## ----eval = FALSE--------------------------------------------------------
#  ## Find all sample IDs (spids) associated with Bisphenol A
#  chnm <- 'Bisphenol A'
#  ch <- fread(system.file("/csv/chemical.csv",
#                     package = "tcpl"),
#                     sep = ",",
#                     header = TRUE)
#  chem <- tcplLoadChem('chnm', chnm)
#  dat4spid <- mc4[mc4$spid %in% chem$spid]

## ----eval = FALSE--------------------------------------------------------
#  dat5spid <- mc5[mc5$m4id %in% dat4spid$m4id]
#  dat5hit <- dat5spid[hitc == 1]

## ----eval = FALSE--------------------------------------------------------
#  mc6_flags <- mc6[ , .( flag = paste(flag, collapse=";")),
#                      by = m5id]
#  dat5dat6 <- merge(x = mc6_flags,
#                    y = dat5hit,
#                    by = "m5id", all.y = TRUE)

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

