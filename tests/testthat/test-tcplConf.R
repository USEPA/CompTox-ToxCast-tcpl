test_that("example=driver updates option TCPL_DRVR", {
  tcplConf(drvr = "example")
  expect_equal("example",
                    getOption("TCPL_DRVR"))
})

test_that("unsupported driver error message", {
  drvr  <-  "test"
  expect_error(
    tcplConf(drvr),
    paste0(drvr," is not a supported database driver. Must be 'MySQL', 'tcplLite', or 'example'.")
)
})