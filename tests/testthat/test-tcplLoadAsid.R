#-------------------------------------------------------------------------------
# Covers testing tcplLoadAsid with "API" driver
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
  assays <- tcplQueryAPI(resource = "assay")
  test_that("tcplLoadAsid works by aeid", {
    expect_no_error(dat <- tcplLoadAsid(fld = "aeid", val = test_api$aeid))
    expect_true(is.data.table(dat))
    expect_equal(nrow(dat), 1)
    expect_true(all(c("aeid", "asid", "asnm") %in% colnames(dat)))
    expect_equal(dat$asid, test_api$asid)
  })
  test_that("tcplLoadAsid works by non-id", {
    expect_no_error(dat <- tcplLoadAsid(fld = c("intended_target_type", "detection_technology_type"), val = list(assays$intended_target_type[1], assays$detection_technology_type[1])))
    expect_true(is.data.table(dat))
    expect_true(nrow(dat) > 0)
    expect_true(all(c("intended_target_type", "detection_technology_type", "asid", "asnm") %in% colnames(dat)))
    expect_equal(unique(dat$intended_target_type), assays$intended_target_type[1])
    expect_equal(unique(dat$detection_technology_type), assays$detection_technology_type[1])
  })
  #error cases
  test_that("data not found results in null data.table", {
    expect_no_error(dat <- tcplLoadAsid(fld = "aeid", val = 0))
    expect_equal(nrow(dat), 0)
  })
  test_that("invalid field results in error", {
    expect_error(dat <- tcplLoadAsid(fld = "awid", val = 0), "Query field\\(s\\) 'awid' not available. Try using from the following:*")
  })
})