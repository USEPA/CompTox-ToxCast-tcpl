#-------------------------------------------------------------------------------
# Covers testing tcplLoadChem with "API" driver
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
  test_that("tcplLoadChem works by spid", {
    expect_no_error(dat <- tcplLoadChem(field = "spid", val = test_api$spid))
    expect_true(is.data.table(dat))
    expect_equal(nrow(dat), 1)
    expect_true(all(c("spid", "chid", "casn", "chnm", "dsstox_substance_id", "code") %in% colnames(dat)))
    expect_equal(dat$spid, test_api$spid)
  })
  test_that("include.spid = FALSE works", {
    expect_no_error(dat <- tcplLoadChem(field = "spid", val = test_api$spid, include.spid = FALSE))
    expect_true(is.data.table(dat))
    expect_equal(nrow(dat), 1)
    expect_true(all(c("chid", "casn", "chnm", "dsstox_substance_id", "code") %in% colnames(dat)))
  })
  test_that("exact is silently changed to TRUE if driver is API", {
    expect_no_error(dat <- tcplLoadChem(field = "spid", val = test_api$spid, exact = FALSE))
    expect_true(is.data.table(dat))
    expect_equal(nrow(dat), 1)
    expect_true(all(c("spid", "chid", "casn", "chnm", "dsstox_substance_id", "code") %in% colnames(dat)))
    expect_equal(dat$spid, test_api$spid)
  })
  #error cases
  test_that("field can only be spid if driver is API", {
    expect_error(tcplLoadChem(), "When drvr option is set to 'API', only 'spid' is a valid 'field' value.")
    expect_error(tcplLoadChem(field = "chnm", val = "fakechnm"), "When drvr option is set to 'API', only 'spid' is a valid 'field' value.")
  })
  test_that("data not found results in null data.table", {
    expect_warning(dat <- tcplLoadChem(field = "spid", val = "fakespid"), "Data not found for the following 'fld' and 'val' combos: 
SPID: fakespid")
    expect_equal(nrow(dat), 0)
  })
})