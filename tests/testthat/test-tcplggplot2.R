#-------------------------------------------------------------------------------
# tests using drvr = "MySQL" and mocking
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

pdf(NULL) # prevent automatic RPlots.pdf output due to not storing tcplggplot2 in object
#-------------------------------------------------------------------------------
# Test tcplggplot2 function - mc
#-------------------------------------------------------------------------------
test_that("tcplggplot2 works for mc single plot", {
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
  expect_true(is.ggplot(tcplggplot2(dat = dat, compare = "m4id", verbose = FALSE)))
  expect_true(is.ggplot(tcplggplot2(dat = dat, compare = "m4id", verbose = FALSE, flags = TRUE)))
  expect_true(is.ggplot(tcplggplot2(dat = dat, compare = "m4id", verbose = FALSE, yrange = c(-100,100))))
})

test_that("tcplggplot2 works for verbose mc single plot", {
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
  expect_false(is.ggplot(tcplggplot2(dat = dat, compare = "m4id", verbose = TRUE)))
  expect_false(is.ggplot(tcplggplot2(dat = dat, compare = "m4id", verbose = TRUE, flags = TRUE)))
  expect_false(is.ggplot(tcplggplot2(dat = dat, compare = "m4id", verbose = TRUE, yrange = c(-100,100))))
})

test_that("tcplggplot2 works for mc small comparison plot", {
  data("mc_test")
  mocked <- mc_test$plot_single_aeid
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  dat <- tcplPlotLoadData(fld = "aeid", val = mocked$aeid, flags = TRUE)
  expect_true(is.ggplot(tcplggplot2(dat = dat, compare = "conc_unit", verbose = FALSE)))
  expect_warning(expect_true(is.ggplot(tcplggplot2(dat = dat, compare = "conc_unit", verbose = FALSE, flags = TRUE))), 
                 "'verbose' = FALSE and 'flags' = TRUE. Flags will not be included in comparison plots unless 'verbose' = TRUE.")
  expect_true(is.ggplot(tcplggplot2(dat = dat, compare = "conc_unit", verbose = FALSE, yrange = c(-100,100))))
})

test_that("tcplggplot2 works for verbose mc small comparison plot", {
  data("mc_test")
  mocked <- mc_test$plot_single_aeid
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  dat <- tcplPlotLoadData(fld = "aeid", val = mocked$aeid, flags = TRUE)
  expect_false(is.ggplot(tcplggplot2(dat = dat, compare = "conc_unit", verbose = TRUE)))
  expect_false(is.ggplot(tcplggplot2(dat = dat, compare = "conc_unit", verbose = TRUE, flags = TRUE)))
  expect_false(is.ggplot(tcplggplot2(dat = dat, compare = "conc_unit", verbose = TRUE, yrange = c(-100,100))))
})

test_that("tcplggplot2 works for mc large comparison plot", {
  data("mc_test")
  mocked <- mc_test$plot_multiple_aeid_compare
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  dat <- tcplPlotLoadData(fld = "aeid", val = mocked$aeid, flags = TRUE)
  expect_true(is.ggplot(tcplggplot2(dat = dat, compare = "conc_unit", verbose = FALSE, group.threshold = 8)))
  expect_true(is.ggplot(tcplggplot2(dat = dat, compare = "conc_unit", verbose = FALSE, yrange = c(-100,100), group.threshold = 8)))
  expect_true(is.ggplot(tcplggplot2(dat = dat, compare = "conc_unit", verbose = FALSE, group.threshold = 8)))
  expect_true(is.ggplot(tcplggplot2(dat = dat, compare = "conc_unit", verbose = FALSE, group.fld = "fitc", group.threshold = 8)))
})

test_that("tcplggplot2 works for verbose (flags table) mc large comparison plot", {
  data("mc_test")
  mocked <- mc_test$plot_multiple_aeid_compare
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  dat <- tcplPlotLoadData(fld = "aeid", val = mocked$aeid, flags = TRUE)
  expect_false(is.ggplot(tcplggplot2(dat = dat, compare = "conc_unit", flags = TRUE, group.threshold = 8)))
  expect_false(is.ggplot(tcplggplot2(dat = dat, compare = "conc_unit", verbose = FALSE, flags = TRUE, group.threshold = 8)))
  expect_false(is.ggplot(tcplggplot2(dat = dat, compare = "conc_unit", flags = TRUE, yrange = c(-100,100), group.threshold = 8)))
  expect_false(is.ggplot(tcplggplot2(dat = dat, compare = "conc_unit", flags = TRUE, group.threshold = 8)))
  expect_false(is.ggplot(tcplggplot2(dat = dat, compare = "conc_unit", flags = TRUE, group.fld = "fitc", group.threshold = 8)))
})


#-------------------------------------------------------------------------------
# Test tcplggplot2 function - mc
#-------------------------------------------------------------------------------
test_that("tcplggplot2 works for sc single plot", {
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
  expect_true(is.ggplot(tcplggplot2(dat = dat, type = "sc", compare = "s2id", verbose = FALSE)))
  expect_true(is.ggplot(tcplggplot2(dat = dat, type = "sc", compare = "s2id", verbose = FALSE, flags = TRUE)))
  expect_true(is.ggplot(tcplggplot2(dat = dat, type = "sc", compare = "s2id", verbose = FALSE, yrange = c(-100,100))))
})

test_that("tcplggplot2 works for verbose sc single plot", {
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
  expect_false(is.ggplot(tcplggplot2(dat = dat, type = "sc", compare = "s2id", verbose = TRUE)))
  expect_false(is.ggplot(tcplggplot2(dat = dat, type = "sc", compare = "s2id", verbose = TRUE, flags = TRUE)))
  expect_false(is.ggplot(tcplggplot2(dat = dat, type = "sc", compare = "s2id", verbose = TRUE, yrange = c(-100,100))))
})

test_that("tcplggplot2 works for sc small comparison plot", {
  data("sc_test")
  mocked <- sc_test$plot_single_aeid
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") sc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  dat <- tcplPlotLoadData(type = "sc", fld = "aeid", val = mocked$aeid)
  expect_true(is.ggplot(tcplggplot2(dat = dat, type = "sc", compare = "conc_unit", verbose = FALSE)))
  expect_true(is.ggplot(tcplggplot2(dat = dat, type = "sc", compare = "conc_unit", verbose = FALSE, flags = TRUE)))
  expect_true(is.ggplot(tcplggplot2(dat = dat, type = "sc", compare = "conc_unit", verbose = FALSE, yrange = c(-100,100))))
})

test_that("tcplggplot2 works for verbose sc small comparison plot", {
  data("sc_test")
  mocked <- sc_test$plot_single_aeid
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") sc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  dat <- tcplPlotLoadData(type = "sc", fld = "aeid", val = mocked$aeid)
  expect_false(is.ggplot(tcplggplot2(dat = dat, type = "sc", compare = "conc_unit", verbose = TRUE)))
  expect_false(is.ggplot(tcplggplot2(dat = dat, type = "sc", compare = "conc_unit", verbose = TRUE, flags = TRUE)))
  expect_false(is.ggplot(tcplggplot2(dat = dat, type = "sc", compare = "conc_unit", verbose = TRUE, yrange = c(-100,100))))
})

test_that("tcplggplot2 works for sc large comparison plot", {
  data("sc_test")
  mocked <- sc_test$plot_multiple_aeid_compare
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") sc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  dat <- tcplPlotLoadData(type = "sc", fld = "aeid", val = mocked$aeid)
  expect_true(is.ggplot(tcplggplot2(dat = dat, type = "sc", compare = "conc_unit", group.threshold = 8)))
  expect_true(is.ggplot(tcplggplot2(dat = dat, type = "sc", compare = "conc_unit", yrange = c(-100,100), group.threshold = 8)))
  expect_true(is.ggplot(tcplggplot2(dat = dat, type = "sc", compare = "conc_unit", group.threshold = 8)))
  expect_true(is.ggplot(tcplggplot2(dat = dat, type = "sc", compare = "conc_unit", group.fld = "coff", group.threshold = 8)))
})

test_that("tcplggplot2 correctly performs checks and validates parameters", {
  data("mc_test")
  mocked <- mc_test$plot_multiple_aeid_compare
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  dat <- tcplPlotLoadData(fld = "aeid", val = mocked$aeid, flags = TRUE)
  expect_error(tcplggplot2(dat=NULL), "'dat' must not be NULL.")
  expect_error(tcplggplot2(dat=dat[0]), "'dat' contains 0 rows.")
  expect_error(tcplggplot2(dat=dat, type = "mv"), "'type' must be 'mc' or 'sc'.")
  expect_warning(expect_true(is.ggplot(tcplggplot2(dat=dat, verbose = FALSE, flags = TRUE))), 
                 "'verbose' = FALSE and 'flags' = TRUE. Flags will not be included in comparison plots unless 'verbose' = TRUE.")
  expect_error(tcplggplot2(dat=dat, group.fld = c("fitc", "model_type")), "'group.fld' must be of length 1.")
  dat$conc_unit[1] <- "mg/L"
  expect_error(tcplggplot2(dat=dat), "Concentration or normalized data type units do not match.")
})