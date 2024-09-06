#-------------------------------------------------------------------------------
# Covers testing tcplLoadUnit with "API" driver
# Using httptest mocking to automatically save json responses from http requests
# NOTE -- updates to the CTX API may mean stored json files are out of date. In 
# this case, delete the 'ctx' folder and follow the instructions in the test-
# tcplLoadData test file. 
#-------------------------------------------------------------------------------
httptest::with_mock_dir("ctx", {
  apikey <- "apikey"
  tcplConf(pass = apikey,
           drvr = "API")
  data(test_api)
  test_that("tcplLoadUnit works by aeid", {
    expect_no_error(dat <- tcplLoadUnit(aeid = test_api$aeid))
    expect_true(is.data.table(dat))
    expect_equal(nrow(dat), 1)
    expect_true(all(c("aeid", "resp_unit") %in% colnames(dat)))
    expect_equal(dat$aeid, test_api$aeid)
  })
  #error cases
  test_that("data not found results in null data.table", {
    expect_no_error(dat <- tcplLoadUnit(aeid = 0))
    expect_equal(nrow(dat), 0)
  })
})