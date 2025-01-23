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


#-------------------------------------------------------------------------------
# Test MORELETTERS
#-------------------------------------------------------------------------------
test_that("MORELETTERS works", {
  expect_error(MORELETTERS(0))
  expect_equal(MORELETTERS(1), "A")
  expect_equal(MORELETTERS(10), "J")
  expect_equal(MORELETTERS(27), "AA")
  expect_equal(MORELETTERS(100), "CV")
  expect_equal(MORELETTERS(1000), "ALL")
  expect_equal(MORELETTERS(1:26), LETTERS)
})


#-------------------------------------------------------------------------------
# Test tcplfit2_fun
#-------------------------------------------------------------------------------
test_that("tcplfit2_fun works", {
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
  model_vals <- tcplfit2_fun(dat, dat$modl, 1:10)
  expect_true(is.numeric(model_vals))
  expect_equal(length(model_vals[!is.na(model_vals)]), 10)
})


#-------------------------------------------------------------------------------
# Test round_n
#-------------------------------------------------------------------------------
test_that("round_n works", {
  expect_equal(round_n(0.00001),"1.0e-05")
  expect_equal(round_n(0.0001),"1.0e-04")
  expect_equal(round_n(0.001),"0.001")
  expect_equal(round_n(0.01),"0.010")
  expect_equal(round_n(0.1),"0.100")
  expect_equal(round_n(1),"1.000")
  expect_equal(round_n(10),"10.000")
  expect_equal(round_n(100),"100.000")
  expect_equal(round_n(1000),"1.0e+03")
  expect_equal(round_n(10000),"1.0e+04")
  expect_equal(round_n(c(.0001,.0003,.001,.003,.01,.03,.1,.3,1,3,10,30,100,300,1000,3000,10000)),
               c('1.0e-04', '3.0e-04', '0.001', '0.003', '0.010', '0.030', '0.100', '0.300', '1.000', 
                 '3.000', '10.000', '30.000', '100.000', '300.000', '1.0e+03', '3.0e+03', '1.0e+04'))
})
