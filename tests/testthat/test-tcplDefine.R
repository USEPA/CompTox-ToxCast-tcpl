test_that("empty returns full dictionary", {
  tcplConf(drvr = "example")
  expect_snapshot(tcplDefine())
})

test_that("multiple values returns all necessary definitions", {
  tcplConf(drvr = "example")
  # just tables
  expect_snapshot(tcplDefine(c("chemical", "sample")))
  # just fields
  expect_snapshot(tcplDefine(c("spid", "rval", "wllt")))
  # mix of tables and fields
  expect_snapshot(tcplDefine(c("sc0", "chid")))
})

test_that("one value returns all necessary definitions", {
  tcplConf(drvr = "example")
  # just a table
  expect_snapshot(tcplDefine("chemical"))
  # just a field
  expect_snapshot(tcplDefine("spid"))
})

test_that("no values found shows a warning", {
  tcplConf(drvr = "example")
  # just a table
  expect_warning(tcplDefine("sc4"), "The given table and/or field names were not found.")
})