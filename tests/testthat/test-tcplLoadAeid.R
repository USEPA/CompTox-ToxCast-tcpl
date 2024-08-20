#-------------------------------------------------------------------------------
# Covers testing tcplLoadAeid with "API" driver
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
  test_that("tcplLoadAeid works by acid", {
    expect_no_error(dat <- tcplLoadAeid(fld = "acid", val = test_api$acid))
    expect_true(is.data.table(dat))
    expect_equal(nrow(dat), 1)
    expect_true(all(c("acid", "aeid", "aenm") %in% colnames(dat)))
    expect_equal(dat$aeid, test_api$aeid)
  })
  test_that("tcplLoadAeid works by non-id", {
    expect_no_error(dat <- tcplLoadAeid(fld = c("intended_target_type", "detection_technology_type"), val = list(assays$intended_target_type[1], assays$detection_technology_type[1])))
    expect_true(is.data.table(dat))
    expect_true(nrow(dat) > 0)
    expect_true(all(c("intended_target_type", "detection_technology_type", "aeid", "aenm") %in% colnames(dat)))
    expect_equal(unique(dat$intended_target_type), assays$intended_target_type[1])
    expect_equal(unique(dat$detection_technology_type), assays$detection_technology_type[1])
  })
  #error cases
  test_that("data not found results in null data.table", {
    expect_no_error(dat <- tcplLoadAeid(fld = "acid", val = 0))
    expect_equal(nrow(dat), 0)
  })
  test_that("invalid field results in error", {
    expect_error(dat <- tcplLoadAeid(fld = "awid", val = 0), "Query field\\(s\\) 'awid' not available. Try using from the following:*")
  })
})