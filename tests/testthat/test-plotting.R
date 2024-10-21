test_that("tcplPlot handles compare.val with multiple plots", {
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
  expect_no_error(suppressWarnings(tcplPlot(type = "mc", fld = "m4id", val = mocked$m4id, compare.val = mocked$compare.m4id, output = "pdf", flags = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot")))
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 1) # exactly one file created
  file.remove(fn) # clean up
})

test_that("tcplPlot handles compare.val with multiple plots and type = 'sc'", {
  data("sc_test")
  mocked <- sc_test$plot_multiple_s2id_compare
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") sc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  expect_no_error(suppressWarnings(tcplPlot(type = "sc", fld = "s2id", val = mocked$s2id, compare.val = mocked$compare.s2id, output = "pdf", verbose = FALSE, flags = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot_sc")))
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot_sc")
  expect_length(fn, 1) # exactly one file created
  file.remove(fn) # clean up
})
