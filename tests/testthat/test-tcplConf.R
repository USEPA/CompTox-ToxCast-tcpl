test_that("example=driver updates option TCPL_DRVR", {
  tcplConf(drvr = "example")
  expect_equal("example",
                    getOption("TCPL_DRVR"))
})

test_that("unsupported driver error message", {
  drvr  <-  "test"
  expect_error(
    tcplConf(drvr),
    paste0(drvr," is not a supported database driver. Must be 'MySQL', 'tcplLite', 'API' or 'example'.")
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
  expect_equal("https://api-ccte.epa.gov/bioactivity",
               getOption("TCPL_HOST")
  )
})