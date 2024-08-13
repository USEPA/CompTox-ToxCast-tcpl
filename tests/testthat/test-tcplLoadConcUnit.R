#-------------------------------------------------------------------------------
# Covers testing tcplLoadConcUnit with "API" driver
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
  test_that("tcplLoadConcUnit works by spid", {
    expect_no_error(dat <- tcplLoadConcUnit(spid = test_api$spid))
    expect_true(is.data.table(dat))
    expect_equal(nrow(dat), 1)
    expect_true(all(c("spid", "conc_unit") %in% colnames(dat)))
    expect_equal(dat$spid, test_api$spid)
  })
  #error cases
  test_that("data not found results in null data.table", {
    expect_warning(dat <- tcplLoadConcUnit(spid = "fakespid"), "Data not found for the following 'fld' and 'val' combos: 
SPID: fakespid")
    expect_equal(nrow(dat), 0)
  })
})