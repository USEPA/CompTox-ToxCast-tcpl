#-------------------------------------------------------------------------------
# old tests using drvr = "example"
#-------------------------------------------------------------------------------

tcplConf(drvr="example")
if (!interactive()) pdf(NULL)

test_that("round_n function works", {
  expect_equal(
    round_n(c(1234.5678, NA_real_, 0.3333),3),
    c("1.2e+03",NA,"0.333"))
  })


test_that("one m4id tcplPlot works", {
  skip()
  lvl = 5
  verbose = TRUE
  l4 <- tcplLoadData(lvl = 4, type = "mc", add.fld = TRUE)
  if (lvl >= 5L) {
    l5 <- mc_vignette[["mc5"]]
    dat <- l4[l5, on = c("m4id","aeid")]
    dat <- dat[,!c("tp","ga","q","la","ac50_loss")]
  }
  agg <- tcplLoadData(lvl = "agg", type = "mc")
  conc_resp_table <- agg %>% 
    group_by(m4id) %>% 
    summarise(conc = list(conc), resp = list(resp)) %>% 
    as.data.table()
  dat <- dat[conc_resp_table, on = "m4id"]
  dat <- dat[,normalized_data_type:="log2_fold_induction"]
  dat <- dat[spid == "01504209"]
  mc5_tcplplot <- suppressWarnings(tcplggplot(dat,verbose = verbose))
  expect_no_warning(mc5_tcplplot)
  vdiffr::expect_doppelganger("test_output_482273", mc5_tcplplot)
})

# needs updates or removal; new spid is not in downward direction
test_that("negative direction plot has negative bmr and cutoff lines", {
  skip()
  lvl = 5
  verbose = FALSE
  l4 <- tcplLoadData(lvl = 4, type = "mc", add.fld = TRUE)
  if (lvl >= 5L) {
    l5 <- mc_vignette[["mc5"]]
    dat <- l4[l5, on = c("m4id","aeid")]
    dat <- dat[,!c("tp","ga","q","la","ac50_loss")]
  }
  agg <- tcplLoadData(lvl = "agg", type = "mc")
  conc_resp_table <- agg %>% 
    group_by(m4id) %>% 
    summarise(conc = list(conc), resp = list(resp)) %>% 
    as.data.table()
  dat <- dat[conc_resp_table, on = "m4id"]
  dat <- dat[,normalized_data_type:="log2_fold_induction"]
  dat <- dat[spid == "01504209"]  
  mc5_tcplplot <- suppressWarnings(tcplggplot(dat,verbose = verbose))
  vdiffr::expect_doppelganger("negative_cutoff_bmr", mc5_tcplplot)
})


#-------------------------------------------------------------------------------
# new tests using drvr = "MySQL" and mocking
#-------------------------------------------------------------------------------
# Format
# test_that("something works", {
  # data("mc_test")
  # mocked <- mc_test$mc0_by_m0id # change this
  # local_mocked_bindings(
  #   tcplQuery = function(query, db, tbl) {
  #     if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
  #     else mocked[query][[1]]
  #   }
  # )
  # tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
# })

# Test tcplPlotLoadData
test_that("tcplPlotLoadData loads all necessary mc data", {
  data("mc_test")
  mocked <- mc_test$plot_single_m4id
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  dat <- tcplPlotLoadData(val = mocked$m4id, flags = TRUE)
  expect_true(is.data.table(dat))
  expect_true(nrow(dat) > 0)
  expect_true(all(c("dsstox_substance_id", "aenm", "m4id", "bmad", "hitc", "modl", 
                    "cnst_aic", "flag", "ac50", "order", "conc", "resp", "conc_unit", 
                    "resp_unit", "normalized_data_type") %in% colnames(dat)))
  expect_true(mocked$m4id %in% dat$m4id)
})

test_that("tcplPlotLoadData loads all necessary sc data", {
  data("sc_test")
  mocked <- sc_test$plot_single_s2id
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") sc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  dat <- tcplPlotLoadData(type = "sc", fld = "s2id", val = mocked$s2id)
  expect_true(is.data.table(dat))
  expect_true(nrow(dat) > 0)
  expect_true(all(c("dsstox_substance_id", "aenm", "s2id", "bmad", "hitc", "max_med", 
                    "order", "conc", "resp", "conc_unit", "coff",
                    "resp_unit", "normalized_data_type") %in% colnames(dat)))
  expect_true(mocked$s2id %in% dat$s2id)
})

test_that("tcplPlotLoadData errors if type is not 'mc' or 'sc'", {
  data("mc_test")
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  expect_error(tcplPlotLoadData(type = "ms", val = 123, flags = TRUE),
               "Invalid 'lvl' and 'type' combination.")
})

# Test tcplPlotValidate
test_that("tcplPlotValidate warns user if flags = TRUE and type = 'sc'", {
  expect_warning(validated_params <- tcplPlotValidate(dat = NULL,type = "sc",compare = "m4id",by = NULL,flags = TRUE,output = "pdf",multi = NULL,verbose = FALSE))
  expect_equal(validated_params, list(dat = NULL, lvl = 2, type = "sc", compare = "s2id", by = NULL, flags = FALSE, output = "pdf", multi = TRUE, verbose = FALSE))
})

test_that("tcplPlotValidate force assigns multi = FALSE and verbose = FALSE if output != pdf", {
  validated_params <- tcplPlotValidate(dat = NULL,type = "mc",compare = "m4id",by = NULL,flags = TRUE,output = "console",multi = TRUE,verbose = TRUE)
  expect_equal(validated_params, list(dat = NULL,lvl = 5,type = "mc",compare = "m4id",by = NULL, flags = TRUE, output = "console", multi = FALSE, verbose = FALSE))
})

test_that("tcplPlotValidate doesn't change correctly assigned parameters", {
  validated_params <- tcplPlotValidate(dat = NULL,type = "mc",compare = "m4id",by = NULL,flags = TRUE,output = "pdf",multi = TRUE,verbose = TRUE)
  expect_equal(validated_params, list(dat = NULL,lvl = 5,type = "mc",compare = "m4id",by = NULL, flags = TRUE, output = "pdf", multi = TRUE, verbose = TRUE))
})

# Test tcplPlotSetYRange
test_that("tcplPlotSetYRange validates range", {
  # error cases
  expect_error(tcplPlotSetYRange(dat=NULL,yuniform=FALSE,yrange=c(1,2,3),type="mc"), "'yrange' must be of length 2")
  expect_error(tcplPlotSetYRange(dat=NULL,yuniform=FALSE,yrange=1,type="mc"), "'yrange' must be of length 2")
  expect_error(tcplPlotSetYRange(dat=NULL,yuniform=FALSE,yrange=NULL,type="mc"), "'yrange' must be of length 2")
  # validate existing set yrange and doesn't overwrite it regardless if yuniform is TRUE
  expect_equal(tcplPlotSetYRange(dat=NULL,yuniform=FALSE,yrange=c(-100,100),type="mc"),c(-100,100))
  expect_equal(tcplPlotSetYRange(dat=NULL,yuniform=TRUE,yrange=c(-100,100),type="mc"),c(-100,100))
})

test_that("tcplPlotSetYRange correctly sets yrange for mc data", {
  data("mc_test")
  mocked <- mc_test$plot_multiple_aeid
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  dat <- tcplPlotLoadData(fld = "aeid", val = mocked$aeid, flags = TRUE)
  expect_no_error(yrange <- tcplPlotSetYRange(dat=dat,yuniform=TRUE,yrange=c(NA,NA),type="mc"))
  expect_length(yrange,2)
  min <- min(dat$resp_min, unlist(dat$resp))
  max <- max(dat$resp_max, unlist(dat$resp))
  if (2 %in% dat$model_type) {
    cutoffs <- dat[model_type == 2]$coff
    min <- min(min, cutoffs, cutoffs * -1)
    max <- max(max, cutoffs, cutoffs * -1)
  }
  # any gain models contained in dat, cutoff only positive
  if (3 %in% dat$model_type) {
    cutoffs <- dat[model_type == 3]$coff
    min <- min(min, cutoffs)
    max <- max(max, cutoffs)
  }
  # any loss models contained in dat, cutoff only negative
  if (4 %in% dat$model_type) {
    cutoffs <- dat[model_type == 4]$coff
    min <- min(min, cutoffs * -1)
    max <- max(max, cutoffs * -1)
  }
  expect_equal(yrange, c(min,max))
})

test_that("tcplPlotSetYRange correctly sets yrange for sc data", {
  data("sc_test")
  mocked <- sc_test$plot_multiple_aeid
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") sc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  dat <- tcplPlotLoadData(type = "sc", fld = "aeid", val = mocked$aeid)
  expect_no_error(yrange <- tcplPlotSetYRange(dat=dat,yuniform=TRUE,yrange=c(NA,NA),type="sc"))
  expect_length(yrange,2)
  min <- min(dat$resp_min, unlist(dat$resp))
  max <- max(dat$resp_max, unlist(dat$resp))
  min <- min(min, dat$coff, dat$coff * -1)
  max <- max(max, dat$coff, dat$coff * -1)
  expect_equal(yrange, c(min,max))
})

# Test tcplPlot - mc
test_that("tcplPlot works for single m4id", {
  data("mc_test")
  mocked <- mc_test$plot_single_m4id
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  expect_no_error(suppressWarnings(tcplPlot(type = "mc", fld = "m4id", val = mocked$m4id, output = "pdf", verbose = TRUE, flags = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot")))
  expect_no_error(suppressWarnings(tcplPlot(type = "mc", fld = "m4id", val = mocked$m4id, output = "pdf", verbose = FALSE, multi = TRUE, fileprefix = "temp_tcplPlot")))
  expect_no_error(suppressWarnings(tcplPlot(type = "mc", fld = "m4id", val = mocked$m4id, output = "console", verbose = TRUE, flags = TRUE, multi = TRUE)))
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 1) # exactly one file created
  file.remove(fn) # clean up
})

test_that("tcplPlot works for multiple m4id", {
  data("mc_test")
  mocked <- mc_test$plot_multiple_m4id
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  expect_no_error(suppressWarnings(tcplPlot(type = "mc", fld = "m4id", val = mocked$m4id, output = "pdf", verbose = TRUE, flags = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot")))
  expect_no_error(suppressWarnings(tcplPlot(type = "mc", fld = "m4id", val = mocked$m4id, output = "pdf", verbose = FALSE, multi = TRUE, fileprefix = "temp_tcplPlot")))
  # console does not work with length(val) > 1
  expect_error(tcplPlot(type = "mc", fld = "m4id", val = mocked$m4id, output = "console", verbose = TRUE, flags = TRUE, multi = TRUE), 
               "More than 1 concentration series returned for given field/val combination. Set output to pdf or reduce the number of plots to 1. Current number of individual plots: 2")
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 1) # exactly one file created
  file.remove(fn) # clean up
})

test_that("tcplPlot works for single aeid", {
  data("mc_test")
  mocked <- mc_test$plot_single_aeid
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  expect_no_error(suppressWarnings(tcplPlot(type = "mc", fld = "aeid", val = mocked$aeid, output = "pdf", verbose = TRUE, flags = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot")))
  expect_no_error(suppressWarnings(tcplPlot(type = "mc", fld = "aeid", val = mocked$aeid, output = "pdf", verbose = FALSE, multi = TRUE, fileprefix = "temp_tcplPlot")))
  # console does not work with length(val) > 1
  expect_error(tcplPlot(type = "mc", fld = "aeid", val = mocked$aeid, output = "console", verbose = TRUE, flags = TRUE, multi = TRUE), 
               "More than 1 concentration series returned for given field/val combination. Set output to pdf or reduce the number of plots to 1. Current number of individual plots: 2")  
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 1) # exactly one file created
  file.remove(fn) # clean up
})

test_that("tcplPlot works for multiple aeid", {
  data("mc_test")
  mocked <- mc_test$plot_multiple_aeid
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  expect_no_error(suppressWarnings(tcplPlot(type = "mc", fld = "aeid", val = mocked$aeid, output = "pdf", verbose = TRUE, flags = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot")))
  expect_no_error(suppressWarnings(tcplPlot(type = "mc", fld = "aeid", val = mocked$aeid, output = "pdf", by = "aeid", verbose = TRUE, flags = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot")))
  expect_no_error(suppressWarnings(tcplPlot(type = "mc", fld = "aeid", val = mocked$aeid, output = "pdf", verbose = FALSE, flags = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot")))
  expect_no_error(suppressWarnings(tcplPlot(type = "mc", fld = "aeid", val = mocked$aeid, output = "pdf", verbose = FALSE, flags = TRUE, by = "aeid", multi = TRUE, fileprefix = "temp_tcplPlot")))
  # console does not work with length(val) > 1
  expect_error(tcplPlot(type = "mc", fld = "aeid", val = mocked$aeid, output = "console", verbose = TRUE, flags = TRUE, multi = TRUE), 
               "More than 1 concentration series returned for given field/val combination. Set output to pdf or reduce the number of plots to 1. Current number of individual plots: 4")
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 3) # exactly three files created (2 from `by = "aeid"` output)
  file.remove(fn) # clean up
})

test_that("tcplPlot works for single spid/aeid", {
  data("mc_test")
  mocked <- mc_test$plot_single_spid
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  expect_no_error(suppressWarnings(tcplPlot(type = "mc", fld = c("spid", "aeid"), val = list(mocked$spid, mocked$aeid), output = "pdf", verbose = TRUE, flags = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot")))
  expect_no_error(suppressWarnings(tcplPlot(type = "mc", fld = c("spid", "aeid"), val = list(mocked$spid, mocked$aeid), output = "pdf", verbose = FALSE, multi = TRUE, fileprefix = "temp_tcplPlot")))
  expect_no_error(suppressWarnings(tcplPlot(type = "mc", fld = c("spid", "aeid"), val = list(mocked$spid, mocked$aeid), output = "console", verbose = TRUE, flags = TRUE, multi = TRUE)))  
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 1) # exactly one file created
  file.remove(fn) # clean up
})

test_that("tcplPlot works for multiple spid/aeid", {
  data("mc_test")
  mocked <- mc_test$plot_multiple_spid
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  expect_no_error(suppressWarnings(tcplPlot(type = "mc", fld = c("spid", "aeid"), val = list(mocked$spid, mocked$aeid), output = "pdf", verbose = TRUE, flags = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot")))
  expect_no_error(suppressWarnings(tcplPlot(type = "mc", fld = c("spid", "aeid"), val = list(mocked$spid, mocked$aeid), output = "pdf", verbose = FALSE, multi = TRUE, fileprefix = "temp_tcplPlot")))
  # console does not work with length(val) > 1
  expect_error(tcplPlot(type = "mc", fld = c("spid", "aeid"), val = list(mocked$spid, mocked$aeid), output = "console", verbose = TRUE, flags = TRUE, multi = TRUE), 
               "More than 1 concentration series returned for given field/val combination. Set output to pdf or reduce the number of plots to 1. Current number of individual plots: 2")
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 1) # exactly one file created
  file.remove(fn) # clean up
})

test_that("tcplPlot works for single m4id compared", {
  data("mc_test")
  mocked <- mc_test$plot_single_m4id_compare
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  expect_no_error(suppressWarnings(tcplPlot(type = "mc", fld = "m4id", val = c(mocked$m4id, mocked$compare.m4id), compare = "chnm", output = "pdf", verbose = TRUE, flags = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot")))
  expect_no_error(suppressWarnings(tcplPlot(type = "mc", fld = "m4id", val = c(mocked$m4id, mocked$compare.m4id), compare = "chnm", output = "pdf", verbose = FALSE, multi = TRUE, fileprefix = "temp_tcplPlot")))
  expect_no_error(suppressWarnings(tcplPlot(type = "mc", fld = "m4id", val = c(mocked$m4id, mocked$compare.m4id), compare = "chnm", output = "console", verbose = TRUE, flags = TRUE, multi = TRUE)))
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 1) # exactly one file created
  file.remove(fn) # clean up
})

test_that("tcplPlot works for multiple m4id compared", {
  data("mc_test")
  mocked <- mc_test$plot_multiple_m4id_compare
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  expect_no_error(suppressWarnings(tcplPlot(type = "mc", fld = "m4id", val = c(mocked$m4id, mocked$compare.m4id), compare = "chnm", output = "pdf", verbose = TRUE, flags = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot")))
  expect_no_error(suppressWarnings(tcplPlot(type = "mc", fld = "m4id", val = c(mocked$m4id, mocked$compare.m4id), compare = "chnm", output = "pdf", verbose = FALSE, flags = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot")))
  # console does not work with length(val) > 1
  expect_error(tcplPlot(type = "mc", fld = "m4id", val = c(mocked$m4id, mocked$compare.m4id), compare = "chnm", output = "console", verbose = TRUE, flags = TRUE, multi = TRUE), 
               "More than 1 concentration series returned for given field/val combination. Set output to pdf or reduce the number of plots to 1. Current number of individual plots: 2")
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 1) # exactly one file created
  file.remove(fn) # clean up
})

test_that("tcplPlot works for single aeid compared", {
  data("mc_test")
  mocked <- mc_test$plot_single_aeid_compare
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  expect_no_error(suppressWarnings(tcplPlot(type = "mc", fld = "aeid", val = c(mocked$aeid, mocked$compare.aeid), compare = "chnm", output = "pdf", verbose = TRUE, flags = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot")))
  expect_no_error(suppressWarnings(tcplPlot(type = "mc", fld = "aeid", val = c(mocked$aeid, mocked$compare.aeid), compare = "chnm", output = "pdf", verbose = FALSE, multi = TRUE, fileprefix = "temp_tcplPlot")))
  # console does not work with length(val) > 1
  expect_error(tcplPlot(type = "mc", fld = "aeid", val = c(mocked$aeid, mocked$compare.aeid), compare = "chnm", output = "console", verbose = TRUE, flags = TRUE, multi = TRUE), 
               "More than 1 concentration series returned for given field/val combination. Set output to pdf or reduce the number of plots to 1. Current number of individual plots: 2")  
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 1) # exactly one file created
  file.remove(fn) # clean up
})

test_that("tcplPlot works for multiple aeid compared", {
  data("mc_test")
  mocked <- mc_test$plot_multiple_aeid_compare
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  expect_no_error(suppressWarnings(tcplPlot(type = "mc", fld = "aeid", val = c(mocked$aeid, mocked$compare.aeid), compare = "chnm", output = "pdf", verbose = TRUE, flags = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot")))
  expect_no_error(suppressWarnings(tcplPlot(type = "mc", fld = "aeid", val = c(mocked$aeid, mocked$compare.aeid), compare = "chnm", output = "pdf", verbose = FALSE, multi = TRUE, flags = TRUE, fileprefix = "temp_tcplPlot")))
  # console does not work with length(val) > 1
  expect_error(tcplPlot(type = "mc", fld = "aeid", val = c(mocked$aeid, mocked$compare.aeid), compare = "chnm", output = "console", verbose = TRUE, flags = TRUE, multi = TRUE), 
               "More than 1 concentration series returned for given field/val combination. Set output to pdf or reduce the number of plots to 1. Current number of individual plots: 2")
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 1) # exactly one file created
  file.remove(fn) # clean up
})

test_that("tcplPlot works for single spid/aeid compared", {
  data("mc_test")
  mocked <- mc_test$plot_single_spid_compare
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  expect_no_error(suppressWarnings(tcplPlot(type = "mc", fld = c("spid", "aeid"), val = list(c(mocked$spid, mocked$compare.spid), c(mocked$aeid, mocked$compare.aeid)), compare = "chnm", output = "pdf", verbose = TRUE, flags = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot")))
  expect_no_error(suppressWarnings(tcplPlot(type = "mc", fld = c("spid", "aeid"), val = list(c(mocked$spid, mocked$compare.spid), c(mocked$aeid, mocked$compare.aeid)), compare = "chnm", output = "pdf", verbose = FALSE, multi = TRUE, fileprefix = "temp_tcplPlot")))
  expect_no_error(suppressWarnings(tcplPlot(type = "mc", fld = c("spid", "aeid"), val = list(c(mocked$spid, mocked$compare.spid), c(mocked$aeid, mocked$compare.aeid)), compare = "chnm", output = "console", verbose = TRUE, flags = TRUE, multi = TRUE)))  
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 1) # exactly one file created
  file.remove(fn) # clean up
})

test_that("tcplPlot works for multiple spid/aeid compared", {
  data("mc_test")
  mocked <- mc_test$plot_multiple_spid_compare
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  expect_no_error(suppressWarnings(tcplPlot(type = "mc", fld = c("spid", "aeid"), val = list(c(mocked$spid, mocked$compare.spid), c(mocked$aeid, mocked$compare.aeid)), compare = "chnm", output = "pdf", verbose = TRUE, flags = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot")))
  expect_no_error(suppressWarnings(tcplPlot(type = "mc", fld = c("spid", "aeid"), val = list(c(mocked$spid, mocked$compare.spid), c(mocked$aeid, mocked$compare.aeid)), compare = "chnm", output = "pdf", verbose = FALSE, flags = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot")))
  # console does not work with length(val) > 1
  expect_error(tcplPlot(type = "mc", fld = c("spid", "aeid"), val = list(c(mocked$spid, mocked$compare.spid), c(mocked$aeid, mocked$compare.aeid)), compare = "chnm", output = "console", verbose = TRUE, flags = TRUE, multi = TRUE), 
               "More than 1 concentration series returned for given field/val combination. Set output to pdf or reduce the number of plots to 1. Current number of individual plots: 2")
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 1) # exactly one file created
  file.remove(fn) # clean up
})

# Test tcplPlot - sc
test_that("tcplPlot works for single s2id", {
  data("sc_test")
  mocked <- sc_test$plot_single_s2id
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") sc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  expect_no_error(suppressWarnings(tcplPlot(type = "sc", fld = "s2id", val = mocked$s2id, output = "pdf", verbose = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot")))
  expect_no_error(suppressWarnings(tcplPlot(type = "sc", fld = "s2id", val = mocked$s2id, output = "pdf", verbose = FALSE, multi = TRUE, fileprefix = "temp_tcplPlot")))
  expect_no_error(suppressWarnings(tcplPlot(type = "sc", fld = "s2id", val = mocked$s2id, output = "console", verbose = TRUE, multi = TRUE)))
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 1) # exactly one file created
  file.remove(fn) # clean up
})

test_that("tcplPlot works for multiple s2id", {
  data("sc_test")
  mocked <- sc_test$plot_multiple_s2id
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") sc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  expect_no_error(suppressWarnings(tcplPlot(type = "sc", fld = "s2id", val = mocked$s2id, output = "pdf", verbose = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot")))
  expect_no_error(suppressWarnings(tcplPlot(type = "sc", fld = "s2id", val = mocked$s2id, output = "pdf", verbose = FALSE, multi = TRUE, fileprefix = "temp_tcplPlot")))
  # console does not work with length(val) > 1
  expect_error(tcplPlot(type = "sc", fld = "s2id", val = mocked$s2id, output = "console", verbose = TRUE, multi = TRUE), 
               "More than 1 concentration series returned for given field/val combination. Set output to pdf or reduce the number of plots to 1. Current number of individual plots: 2")
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 1) # exactly one file created
  file.remove(fn) # clean up
})

test_that("tcplPlot works for single aeid", {
  data("sc_test")
  mocked <- sc_test$plot_single_aeid
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") sc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  expect_no_error(suppressWarnings(tcplPlot(type = "sc", fld = "aeid", val = mocked$aeid, output = "pdf", verbose = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot")))
  expect_no_error(suppressWarnings(tcplPlot(type = "sc", fld = "aeid", val = mocked$aeid, output = "pdf", verbose = FALSE, multi = TRUE, fileprefix = "temp_tcplPlot")))
  # console does not work with length(val) > 1
  expect_error(tcplPlot(type = "sc", fld = "aeid", val = mocked$aeid, output = "console", verbose = TRUE, multi = TRUE), 
               "More than 1 concentration series returned for given field/val combination. Set output to pdf or reduce the number of plots to 1. Current number of individual plots: 7")  
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 1) # exactly one file created
  file.remove(fn) # clean up
})

test_that("tcplPlot works for multiple aeid", {
  data("sc_test")
  mocked <- sc_test$plot_multiple_aeid
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") sc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  expect_no_error(suppressWarnings(tcplPlot(type = "sc", fld = "aeid", val = mocked$aeid, output = "pdf", verbose = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot")))
  expect_no_error(suppressWarnings(tcplPlot(type = "sc", fld = "aeid", val = mocked$aeid, output = "pdf", verbose = FALSE, multi = TRUE, fileprefix = "temp_tcplPlot")))
  expect_no_error(suppressWarnings(tcplPlot(type = "sc", fld = "aeid", val = mocked$aeid, by = "aeid", output = "pdf", verbose = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot")))
  # console does not work with length(val) > 1
  expect_error(tcplPlot(type = "sc", fld = "aeid", val = mocked$aeid, output = "console", verbose = TRUE, multi = TRUE), 
               "More than 1 concentration series returned for given field/val combination. Set output to pdf or reduce the number of plots to 1. Current number of individual plots: 14")
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 3) # exactly one file created
  file.remove(fn) # clean up
})

test_that("tcplPlot works for single spid/aeid", {
  data("sc_test")
  mocked <- sc_test$plot_single_spid
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") sc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  expect_no_error(suppressWarnings(tcplPlot(type = "sc", fld = c("spid", "aeid"), val = list(mocked$spid, mocked$aeid), output = "pdf", verbose = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot")))
  expect_no_error(suppressWarnings(tcplPlot(type = "sc", fld = c("spid", "aeid"), val = list(mocked$spid, mocked$aeid), output = "pdf", verbose = FALSE, multi = TRUE, fileprefix = "temp_tcplPlot")))
  expect_no_error(suppressWarnings(tcplPlot(type = "sc", fld = c("spid", "aeid"), val = list(mocked$spid, mocked$aeid), output = "console", verbose = TRUE, multi = TRUE)))  
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 1) # exactly one file created
  file.remove(fn) # clean up
})

test_that("tcplPlot works for multiple spid/aeid", {
  data("sc_test")
  mocked <- sc_test$plot_multiple_spid
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") sc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  expect_no_error(suppressWarnings(tcplPlot(type = "sc", fld = c("spid", "aeid"), val = list(mocked$spid, mocked$aeid), output = "pdf", verbose = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot")))
  expect_no_error(suppressWarnings(tcplPlot(type = "sc", fld = c("spid", "aeid"), val = list(mocked$spid, mocked$aeid), output = "pdf", verbose = FALSE, multi = TRUE, fileprefix = "temp_tcplPlot")))
  # console does not work with length(val) > 1
  expect_error(tcplPlot(type = "sc", fld = c("spid", "aeid"), val = list(mocked$spid, mocked$aeid), output = "console", verbose = TRUE, multi = TRUE), 
               "More than 1 concentration series returned for given field/val combination. Set output to pdf or reduce the number of plots to 1. Current number of individual plots: 2")
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 1) # exactly one file created
  file.remove(fn) # clean up
})

test_that("tcplPlot works for single s2id compared", {
  data("sc_test")
  mocked <- sc_test$plot_single_s2id_compare
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") sc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  expect_no_error(suppressWarnings(tcplPlot(type = "sc", fld = "s2id", val = c(mocked$s2id, mocked$compare.s2id), compare = "spid", output = "pdf", verbose = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot")))
  expect_no_error(suppressWarnings(tcplPlot(type = "sc", fld = "s2id", val = c(mocked$s2id, mocked$compare.s2id), compare = "spid", output = "pdf", verbose = FALSE, multi = TRUE, fileprefix = "temp_tcplPlot")))
  expect_no_error(suppressWarnings(tcplPlot(type = "sc", fld = "s2id", val = c(mocked$s2id, mocked$compare.s2id), compare = "spid", output = "console", verbose = TRUE, multi = TRUE)))
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 1) # exactly one file created
  file.remove(fn) # clean up
})

test_that("tcplPlot works for multiple s2id compared", {
  data("sc_test")
  mocked <- sc_test$plot_multiple_s2id_compare
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") sc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  expect_no_error(suppressWarnings(tcplPlot(type = "sc", fld = "s2id", val = c(mocked$s2id, mocked$compare.s2id), compare = "spid", output = "pdf", verbose = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot")))
  expect_no_error(suppressWarnings(tcplPlot(type = "sc", fld = "s2id", val = c(mocked$s2id, mocked$compare.s2id), compare = "spid", output = "pdf", verbose = FALSE, multi = TRUE, fileprefix = "temp_tcplPlot")))
  # console does not work with length(val) > 1
  expect_error(tcplPlot(type = "sc", fld = "s2id", val = c(mocked$s2id, mocked$compare.s2id), compare = "spid", output = "console", verbose = TRUE, multi = TRUE), 
               "More than 1 concentration series returned for given field/val combination. Set output to pdf or reduce the number of plots to 1. Current number of individual plots: 2")
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 1) # exactly one file created
  file.remove(fn) # clean up
})

test_that("tcplPlot works for single aeid compared", {
  data("sc_test")
  mocked <- sc_test$plot_single_aeid_compare
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") sc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  expect_no_error(suppressWarnings(tcplPlot(type = "sc", fld = "aeid", val = c(mocked$aeid, mocked$compare.aeid), compare = "spid", output = "pdf", verbose = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot")))
  expect_no_error(suppressWarnings(tcplPlot(type = "sc", fld = "aeid", val = c(mocked$aeid, mocked$compare.aeid), compare = "spid", output = "pdf", verbose = FALSE, multi = TRUE, fileprefix = "temp_tcplPlot")))
  # console does not work with length(val) > 1
  expect_error(tcplPlot(type = "sc", fld = "aeid", val = c(mocked$aeid, mocked$compare.aeid), compare = "spid", output = "console", verbose = TRUE, multi = TRUE), 
               "More than 1 concentration series returned for given field/val combination. Set output to pdf or reduce the number of plots to 1. Current number of individual plots: 7")  
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 1) # exactly one file created
  file.remove(fn) # clean up
})

test_that("tcplPlot works for multiple aeid compared", {
  data("sc_test")
  mocked <- sc_test$plot_multiple_aeid_compare
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") sc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  expect_no_error(suppressWarnings(tcplPlot(type = "sc", fld = "aeid", val = c(mocked$aeid, mocked$compare.aeid), compare = "spid", output = "pdf", verbose = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot")))
  expect_no_error(suppressWarnings(tcplPlot(type = "sc", fld = "aeid", val = c(mocked$aeid, mocked$compare.aeid), compare = "spid", output = "pdf", verbose = FALSE, multi = TRUE, fileprefix = "temp_tcplPlot")))
  # console does not work with length(val) > 1
  expect_error(tcplPlot(type = "sc", fld = "aeid", val = c(mocked$aeid, mocked$compare.aeid), compare = "spid", output = "console", verbose = TRUE, multi = TRUE), 
               "More than 1 concentration series returned for given field/val combination. Set output to pdf or reduce the number of plots to 1. Current number of individual plots: 7")
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 1) # exactly one file created
  file.remove(fn) # clean up
})

test_that("tcplPlot works for single spid/aeid compared", {
  data("sc_test")
  mocked <- sc_test$plot_single_spid_compare
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") sc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  expect_no_error(suppressWarnings(tcplPlot(type = "sc", fld = c("spid", "aeid"), val = list(c(mocked$spid, mocked$compare.spid), c(mocked$aeid, mocked$compare.aeid)), compare = "spid", output = "pdf", verbose = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot")))
  expect_no_error(suppressWarnings(tcplPlot(type = "sc", fld = c("spid", "aeid"), val = list(c(mocked$spid, mocked$compare.spid), c(mocked$aeid, mocked$compare.aeid)), compare = "spid", output = "pdf", verbose = FALSE, multi = TRUE, fileprefix = "temp_tcplPlot")))
  expect_no_error(suppressWarnings(tcplPlot(type = "sc", fld = c("spid", "aeid"), val = list(c(mocked$spid, mocked$compare.spid), c(mocked$aeid, mocked$compare.aeid)), compare = "spid", output = "console", verbose = TRUE, multi = TRUE)))  
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 1) # exactly one file created
  file.remove(fn) # clean up
})

test_that("tcplPlot works for multiple spid/aeid compared", {
  data("sc_test")
  mocked <- sc_test$plot_multiple_spid_compare
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") sc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  expect_no_error(suppressWarnings(tcplPlot(type = "sc", fld = c("spid", "aeid"), val = list(c(mocked$spid, mocked$compare.spid), c(mocked$aeid, mocked$compare.aeid)), compare = "spid", output = "pdf", verbose = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot")))
  expect_no_error(suppressWarnings(tcplPlot(type = "sc", fld = c("spid", "aeid"), val = list(c(mocked$spid, mocked$compare.spid), c(mocked$aeid, mocked$compare.aeid)), compare = "spid", output = "pdf", verbose = FALSE, multi = TRUE, fileprefix = "temp_tcplPlot")))
  # console does not work with length(val) > 1
  expect_error(tcplPlot(type = "sc", fld = c("spid", "aeid"), val = list(c(mocked$spid, mocked$compare.spid), c(mocked$aeid, mocked$compare.aeid)), compare = "spid", output = "console", verbose = TRUE, multi = TRUE), 
               "More than 1 concentration series returned for given field/val combination. Set output to pdf or reduce the number of plots to 1. Current number of individual plots: 2")
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 1) # exactly one file created
  file.remove(fn) # clean up
})

# Stand-alone plotting
test_that("standalone plotting works in mc", {
  data("mc_test")
  mocked <- mc_test$plot_single_m4id
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  dat <- tcplPlotLoadData(val = mocked$m4id, flags = TRUE)
  expect_no_error(suppressWarnings(tcplPlot(dat = dat, type = "mc", fld = "m4id", val = mocked$m4id, output = "pdf", verbose = TRUE, flags = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot")))
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 1) # exactly one file created
  file.remove(fn) # clean up
})

test_that("standalone advanced comparison plotting works in mc", {
  data("mc_test")
  mocked <- mc_test$plot_single_m4id_compare
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  dat <- tcplPlotLoadData(val = mocked$m4id, flags = TRUE)
  expect_no_error(suppressWarnings(tcplPlot(dat = dat, type = "mc", fld = "m4id", val = c(mocked$m4id, mocked$compare.m4id), compare = "spid", output = "pdf", verbose = TRUE, flags = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot")))
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 1) # exactly one file created
  file.remove(fn) # clean up
})

test_that("standalone plotting works in sc", {
  data("sc_test")
  mocked <- sc_test$plot_single_s2id
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") sc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  dat <- tcplPlotLoadData(type = "sc", fld = "s2id", val = mocked$s2id)
  expect_no_error(suppressWarnings(tcplPlot(dat = dat, type = "sc", fld = "s2id", val = mocked$s2id, output = "pdf", verbose = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot")))
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 1) # exactly one file created
  file.remove(fn) # clean up
})

test_that("standalone advanced comparison plotting works in sc", {
  data("sc_test")
  mocked <- sc_test$plot_single_s2id_compare
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") sc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  dat <- tcplPlotLoadData(type = "sc", fld = "s2id", val = mocked$s2id)
  expect_no_error(suppressWarnings(tcplPlot(dat = dat, type = "sc", fld = "s2id", val = c(mocked$s2id, mocked$compare.s2id), compare = "spid", output = "pdf", verbose = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot")))
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 1) # exactly one file created
  file.remove(fn) # clean up
})

# "ggplot" output tests
test_that("ggplot output works in mc", {
  data("mc_test")
  mocked <- mc_test$plot_single_m4id
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  expect_no_error(suppressWarnings(otpt <- tcplPlot(type = "mc", fld = "m4id", val = mocked$m4id, verbose = FALSE, output = "ggplot", fileprefix = "temp_tcplPlot")))
  expect_true(ggplot2::is.ggplot(otpt))
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 0) # no files should be generated
})

test_that("ggplot output comparison plotting works in mc", {
  data("mc_test")
  mocked <- mc_test$plot_single_m4id_compare
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  expect_no_error(suppressWarnings(otpt <- tcplPlot(type = "mc", fld = "m4id", val = c(mocked$m4id, mocked$compare.m4id), compare = "chnm", verbose = FALSE, output = "ggplot", fileprefix = "temp_tcplPlot")))
  expect_true(ggplot2::is.ggplot(otpt))
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 0) # no files should be generated
})

test_that("ggplot output works in sc", {
  data("sc_test")
  mocked <- sc_test$plot_single_s2id
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") sc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  expect_no_error(suppressWarnings(otpt <- tcplPlot(type = "sc", fld = "s2id", val = mocked$s2id, verbose = FALSE, output = "ggplot", fileprefix = "temp_tcplPlot")))
  expect_true(ggplot2::is.ggplot(otpt))
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 0) # no files should be generated
})

test_that("ggplot output comparison plotting works in sc", {
  data("sc_test")
  mocked <- sc_test$plot_single_s2id_compare
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") sc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  expect_no_error(suppressWarnings(otpt <- tcplPlot(type = "sc", fld = "s2id", val = c(mocked$s2id, mocked$compare.s2id), compare = "spid", verbose = FALSE, output = "ggplot", fileprefix = "temp_tcplPlot")))
  expect_true(ggplot2::is.ggplot(otpt))
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 0) # no files should be generated
})

test_that("ggplot output returns specific warnings and errors", {
  data("mc_test")
  mocked <- mc_test$plot_multiple_m4id
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  expect_error(tcplPlot(type = "mc", fld = "m4id", val = mocked$m4id, output = "ggplot", flags = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot"),
               "More than 1 concentration series returned for given field/val combination. Set output to pdf or reduce the number of plots to 1. Current number of individual plots: 2")
  mocked <- mc_test$plot_single_m4id
  expect_message(suppressWarnings(tcplPlot(type = "mc", fld = "m4id", val = mocked$m4id, output = "ggplot", verbose = TRUE, flags = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot")),
                 "ggplot and verbose table arranged into one grob. To work with a simple ggplot object, set `verbose = FALSE` and 'flags = FALSE'.")
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 0) # no files should be generated
})


#-------------------------------------------------------------------------------
# Covers testing tcplLoadData with "API" driver
# Using httptest mocking to automatically save json responses from http requests
# NOTE -- updates to the CTX API may mean stored json files are out of date. In 
# this case, delete the 'ctx' folder and rerun this ENTIRE test file (temporarily
# replacing the 'apikey' string with a valid key) to repopulate the stored 
# .jsons. These will likely be huge and will need to be edited by hand to reduce
# their sizes. To do this, open the file(s) and remove all but one element of
# the outer array -- we don't need more than one endpoint-sample.
#-------------------------------------------------------------------------------
httptest::with_mock_dir("ctx", {
  apikey <- "apikey"
  tcplConf(pass = apikey,
           drvr = "API")
  data(test_api)
  test_that("tcplPlot works with API data by m4id", {
    expect_no_error(suppressWarnings(tcplPlot(val = test_api$m4id, output = "pdf", verbose = TRUE, flags = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot")))
    fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
    expect_length(fn, 1) # exactly one file created
    file.remove(fn) # clean up
  })
  test_that("tcplPlot works with API data by aeid", {
    expect_no_error(suppressWarnings(tcplPlot(fld = "aeid", val = test_api$aeid, output = "pdf", verbose = TRUE, flags = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot")))
    fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
    expect_length(fn, 1) # exactly one file created
    file.remove(fn) # clean up
  })
  test_that("tcplPlot works with API data by spid", {
    expect_no_error(suppressWarnings(tcplPlot(fld = "spid", val = test_api$spid, output = "pdf", verbose = TRUE, flags = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot")))
    fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
    expect_length(fn, 1) # exactly one file created
    file.remove(fn) # clean up
  })
  test_that("tcplPlot works with API data by dtxsid", {
    expect_no_error(suppressWarnings(tcplPlot(fld = "dtxsid", val = test_api$dtxsid, output = "pdf", verbose = TRUE, flags = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot")))
    fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
    expect_length(fn, 1) # exactly one file created
    file.remove(fn) # clean up
  })
})