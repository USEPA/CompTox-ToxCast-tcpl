test_that("example=driver updates option TCPL_DRVR", {
  drvr <- "example"
  tcplConf(drvr)
  expect_equal(getOption("TCPL_DRVR"),
               drvr)
})

test_that("unsupported driver error message", {
  drvr  <-  "test"
  expect_error(
    tcplConf(drvr),
    paste0(drvr," is not a supported database driver. Must be 'MySQL', 'API', or 'example'.")
)
})

test_that("API driver no key error message", {
  drvr  <-  "API"
  expect_error(
    tcplConf(drvr),
    "'API' driver requires an API-key, supply it to the 'pass' parameter. To request a key, send an email to ccte_api@epa.gov."
  )
})

test_that("API driver properly sets URL", {
  tcplConf(drvr = "API", pass = "api_key")
  expect_equal(getOption("TCPL_HOST"), 
               "https://api-ccte.epa.gov/bioactivity"
  )
})