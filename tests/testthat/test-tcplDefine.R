test_that("empty returns full dictionary", {
  tcplConf(drvr = "example")
  expect_snapshot_value(tcplDefine(), style = "serialize")
})

test_that("multiple values returns all necessary definitions", {
  tcplConf(drvr = "example")
  # just tables
  expect_snapshot_value(tcplDefine(c("chemical", "sample")), style = "serialize")
  # just fields
  expect_snapshot_value(tcplDefine(c("spid", "rval", "wllt")), style = "serialize")
  # mix of tables and fields
  expect_snapshot_value(tcplDefine(c("sc0", "chid")), style = "serialize")
})

test_that("one value returns all necessary definitions", {
  tcplConf(drvr = "example")
  # just a table
  expect_snapshot_value(tcplDefine("chemical"), style = "serialize")
  # just a field
  expect_snapshot_value(tcplDefine("spid"), style = "serialize")
})

test_that("no values found shows a warning", {
  tcplConf(drvr = "example")
  # just a table
  expect_warning(tcplDefine("sc4"), "The given table and/or field names were not found.")
})