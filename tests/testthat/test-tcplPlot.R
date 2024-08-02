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
  skip_on_ci()
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
  mc5_tcplplot <- tcplggplot(dat,verbose = verbose)
  expect_no_warning(mc5_tcplplot)
  vdiffr::expect_doppelganger("test_output_482273", mc5_tcplplot)
})

test_that("negative direction plot has negative bmr and cutoff lines", {
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
  dat <- dat[spid == "1210314466"]  
  mc5_tcplplot <- tcplggplot(dat,verbose = verbose)
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
  skip()
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

test_that("tcplPlotValidate warns user if flags = TRUE and type = 'sc'", {
  expect_warning(validated_params <- tcplPlotValidate(type = "sc",flags = TRUE,output = "pdf",multi = NULL,verbose = FALSE))
  expect_equal(validated_params, list(lvl = 2, type = "sc", flags = FALSE, output = "pdf", multi = TRUE, verbose = FALSE))
})

test_that("tcplPlotValidate force assigns multi = FALSE and verbose = FALSE if output != pdf", {
  validated_params <- tcplPlotValidate(type = "mc",flags = TRUE,output = "console",multi = TRUE,verbose = TRUE)
  expect_equal(validated_params, list(lvl = 5, type = "mc", flags = TRUE, output = "console", multi = FALSE, verbose = FALSE))
})

test_that("tcplPlotValidate doesn't change correctly assigned parameters", {
  validated_params <- tcplPlotValidate(type = "mc",flags = TRUE,output = "pdf",multi = TRUE,verbose = TRUE)
  expect_equal(validated_params, list(lvl = 5, type = "mc", flags = TRUE, output = "pdf", multi = TRUE, verbose = TRUE))
})

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
  expect_equal(yrange, c(-53.6184,19.1))
})

test_that("tcplPlotSetYRange correctly sets yrange for sc data", {
  skip()
})

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
  expect_no_error(suppressWarnings(tcplPlot(type = "mc", fld = "m4id", val = mocked$m4id, output = "console", verbose = TRUE, flags = TRUE, multi = TRUE)))
  file.remove(stringr::str_subset(list.files(), "^temp_tcplPlot")) # clean up
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
  # console does not work with length(val) > 1
  expect_error(tcplPlot(type = "mc", fld = "m4id", val = mocked$m4id, output = "console", verbose = TRUE, flags = TRUE, multi = TRUE), 
               "More than 1 concentration series returned for given field/val combination.  Set output to pdf or reduce the number of curves to 1. Current number of curves: 2")
  file.remove(stringr::str_subset(list.files(), "^temp_tcplPlot")) # clean up
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
  # console does not work with length(val) > 1
  expect_error(tcplPlot(type = "mc", fld = "aeid", val = mocked$aeid, output = "console", verbose = TRUE, flags = TRUE, multi = TRUE), 
               "More than 1 concentration series returned for given field/val combination.  Set output to pdf or reduce the number of curves to 1. Current number of curves: 2")  
  file.remove(stringr::str_subset(list.files(), "^temp_tcplPlot")) # clean up
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
  # console does not work with length(val) > 1
  expect_error(tcplPlot(type = "mc", fld = "aeid", val = mocked$aeid, output = "console", verbose = TRUE, flags = TRUE, multi = TRUE), 
               "More than 1 concentration series returned for given field/val combination.  Set output to pdf or reduce the number of curves to 1. Current number of curves: 4")
  file.remove(stringr::str_subset(list.files(), "^temp_tcplPlot")) # clean up
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
  # console does not work with length(val) > 1
  expect_no_error(suppressWarnings(tcplPlot(type = "mc", fld = c("spid", "aeid"), val = list(mocked$spid, mocked$aeid), output = "console", verbose = TRUE, flags = TRUE, multi = TRUE)))  
  file.remove(stringr::str_subset(list.files(), "^temp_tcplPlot")) # clean up
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
  # console does not work with length(val) > 1
  expect_error(tcplPlot(type = "mc", fld = c("spid", "aeid"), val = list(mocked$spid, mocked$aeid), output = "console", verbose = TRUE, flags = TRUE, multi = TRUE), 
               "More than 1 concentration series returned for given field/val combination.  Set output to pdf or reduce the number of curves to 1. Current number of curves: 2")
  file.remove(stringr::str_subset(list.files(), "^temp_tcplPlot")) # clean up
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
  expect_no_error(suppressWarnings(tcplPlot(type = "mc", fld = "m4id", val = mocked$m4id, compare.val = mocked$compare.m4id, output = "pdf", verbose = TRUE, flags = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot")))
  expect_no_error(suppressWarnings(tcplPlot(type = "mc", fld = "m4id", val = mocked$m4id, compare.val = mocked$compare.m4id, output = "console", verbose = TRUE, flags = TRUE, multi = TRUE)))
  file.remove(stringr::str_subset(list.files(), "^temp_tcplPlot")) # clean up
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
  expect_no_error(suppressWarnings(tcplPlot(type = "mc", fld = "m4id", val = mocked$m4id, compare.val = mocked$compare.m4id, output = "pdf", verbose = TRUE, flags = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot")))
  # console does not work with length(val) > 1
  expect_error(tcplPlot(type = "mc", fld = "m4id", val = mocked$m4id, compare.val = mocked$compare.m4id, output = "console", verbose = TRUE, flags = TRUE, multi = TRUE), 
               "More than 1 concentration series returned for given field/val combination.  Set output to pdf or reduce the number of curves to 1. Current number of curves: 4")
  file.remove(stringr::str_subset(list.files(), "^temp_tcplPlot")) # clean up
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
  expect_no_error(suppressWarnings(tcplPlot(type = "mc", fld = "aeid", val = mocked$aeid, compare.val = mocked$compare.aeid, output = "pdf", verbose = TRUE, flags = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot")))
  # console does not work with length(val) > 1
  expect_error(tcplPlot(type = "mc", fld = "aeid", val = mocked$aeid, compare.val = mocked$compare.aeid, output = "console", verbose = TRUE, flags = TRUE, multi = TRUE), 
               "More than 1 concentration series returned for given field/val combination.  Set output to pdf or reduce the number of curves to 1. Current number of curves: 4")  
  file.remove(stringr::str_subset(list.files(), "^temp_tcplPlot")) # clean up
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
  expect_no_error(suppressWarnings(tcplPlot(type = "mc", fld = "aeid", val = mocked$aeid, compare.val = mocked$compare.aeid, output = "pdf", verbose = TRUE, flags = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot")))
  # console does not work with length(val) > 1
  expect_error(tcplPlot(type = "mc", fld = "aeid", val = mocked$aeid, compare.val = mocked$compare.aeid, output = "console", verbose = TRUE, flags = TRUE, multi = TRUE), 
               "More than 1 concentration series returned for given field/val combination.  Set output to pdf or reduce the number of curves to 1. Current number of curves: 8")
  file.remove(stringr::str_subset(list.files(), "^temp_tcplPlot")) # clean up
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
  expect_no_error(suppressWarnings(tcplPlot(type = "mc", fld = c("spid", "aeid"), val = list(mocked$spid, mocked$aeid), compare.val = list(mocked$compare.spid, mocked$compare.aeid), output = "pdf", verbose = TRUE, flags = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot")))
  # console does not work with length(val) > 1
  expect_no_error(suppressWarnings(tcplPlot(type = "mc", fld = c("spid", "aeid"), val = list(mocked$spid, mocked$aeid), compare.val = list(mocked$compare.spid, mocked$compare.aeid), output = "console", verbose = TRUE, flags = TRUE, multi = TRUE)))  
  file.remove(stringr::str_subset(list.files(), "^temp_tcplPlot")) # clean up
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
  expect_no_error(suppressWarnings(tcplPlot(type = "mc", fld = c("spid", "aeid"), val = list(mocked$spid, mocked$aeid), compare.val = list(mocked$compare.spid, mocked$compare.aeid), output = "pdf", verbose = TRUE, flags = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot")))
  # console does not work with length(val) > 1
  expect_error(tcplPlot(type = "mc", fld = c("spid", "aeid"), val = list(mocked$spid, mocked$aeid), compare.val = list(mocked$compare.spid, mocked$compare.aeid), output = "console", verbose = TRUE, flags = TRUE, multi = TRUE), 
               "More than 1 concentration series returned for given field/val combination.  Set output to pdf or reduce the number of curves to 1. Current number of curves: 4")
  file.remove(stringr::str_subset(list.files(), "^temp_tcplPlot")) # clean up
})