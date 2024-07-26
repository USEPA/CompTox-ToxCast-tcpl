tcplConf(drvr = "example")

test_that("sc0 loads sc0 data table", {
  sc0 <- tcplLoadData(lvl = 0, type = "sc")
  expect_true(
    is.data.table(sc0)
  )
  sc0 <- tcplLoadData(lvl = 0L, type = "sc")
  expect_true(
    is.data.table(sc0)
  )
})

test_that("sc1 loads sc1 data table", {
  sc1 <- tcplLoadData(lvl = 1, type = "sc")
  expect_true(
    is.data.table(sc1)
  )
  sc1 <- tcplLoadData(lvl = 1L, type = "sc")
  expect_true(
    is.data.table(sc1)
  )
})

test_that("sc2 loads sc2 data table", {
  sc2 <- tcplLoadData(lvl = 2, type = "sc")
  expect_true(
    is.data.table(sc2)
  )
  sc2 <- tcplLoadData(lvl = 2L, type = "sc")
  expect_true(
    is.data.table(sc2)
  )
})

test_that("lvl = 'agg' loads sc agg data table", {
  agg <- tcplLoadData(lvl = "agg", type = "sc")
  expect_true(
    is.data.table(agg)
  )
})

test_that("mc0 loads mc0 data table", {
  mc0 <- tcplLoadData(lvl = 0, type = "mc")
  expect_true(
    is.data.table(mc0)
  )
  mc0 <- tcplLoadData(lvl = 0L, type = "mc")
  expect_true(
    is.data.table(mc0)
  )
})

test_that("mc1 loads mc1 data table", {
  mc1 <- tcplLoadData(lvl = 1, type = "mc")
  expect_true(
    is.data.table(mc1)
  )
  mc1 <- tcplLoadData(lvl = 1L, type = "mc")
  expect_true(
    is.data.table(mc1)
  )
})

test_that("mc2 loads mc2 data table", {
  mc2 <- tcplLoadData(lvl = 2, type = "mc")
  expect_true(
    is.data.table(mc2)
  )
  mc2 <- tcplLoadData(lvl = 2L, type = "mc")
  expect_true(
    is.data.table(mc2)
  )
})

test_that("mc3 loads mc3 data table", {
  mc3 <- tcplLoadData(lvl = 3, type = "mc")
  expect_true(
    is.data.table(mc3)
  )
  mc3 <- tcplLoadData(lvl = 3L, type = "mc")
  expect_true(
    is.data.table(mc3)
  )
})

test_that("mc4 loads mc4 data table", {
  mc4 <- tcplLoadData(lvl = 4, type = "mc")
  expect_true(
    is.data.table(mc4)
  )
  mc4 <- tcplLoadData(lvl = 4L, type = "mc")
  expect_true(
    is.data.table(mc4)
  )
})

test_that("mc4 !add.fld loads mc4 data table", {
  mc4 <- tcplLoadData(lvl = 4, type = "mc", add.fld=FALSE)
  expect_true(
    is.data.table(mc4)
  )
  mc4 <- tcplLoadData(lvl = 4L, type = "mc", add.fld=FALSE)
  expect_true(
    is.data.table(mc4)
  )
})

test_that("mc5 loads mc5 data table", {
  mc5 <- tcplLoadData(lvl = 5, type = "mc")
  expect_true(
    is.data.table(mc5)
  )
  mc5 <- tcplLoadData(lvl = 5L, type = "mc")
  expect_true(
    is.data.table(mc5)
  )
})

test_that("mc5 !add.fld loads mc5 data table", {
  mc5 <- tcplLoadData(lvl = 5, type = "mc", add.fld=FALSE)
  expect_true(
    is.data.table(mc5)
  )
  mc5 <- tcplLoadData(lvl = 5L, type = "mc", add.fld=FALSE)
  expect_true(
    is.data.table(mc5)
  )
})

test_that("lvl = 'agg' loads mc agg data table", {
  agg <- tcplLoadData(lvl = "agg", type = "mc")
  expect_true(
    is.data.table(agg)
  )
})

test_that("invalid input error message", {
  expect_error(
    tcplLoadData(lvl = "sc", type = 0),
    "Invalid 'lvl' and 'type' combination."
  )
})

test_that("sc example error message appears", {
  expect_error(
    tcplLoadData(lvl = 4, type = "sc"),
    "example tables for sc0, sc1, sc2, agg available."
  )
})

test_that("mc example error message appears", {
  expect_error(
    tcplLoadData(lvl = 6, type = "mc"),
    "example tables for mc0, mc1, mc2, mc3, mc4, mc5, agg available."
  )
})

# test_that("description", {
#   expect_*(code)
# })



## this stores simple string responses for .prepField
# test_that("tcplLoadData loads mc4 data for one m4id", {
#   data("mc_test")
#   mocked <- mc_test$mc4_m4id_12699347
#   local_mocked_bindings(
#     tcplQuery = function(query, db, tbl) {
#       if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
#       else mocked$tcplQuery
#     },
#     .prepField = function(fld, tbl, db) mocked$.prepField
#   )
#   tcplConf(drvr = "MySQL")
#   mc4 <- tcplLoadData(lvl = 4, fld = "m4id", val = 12699347)
#   expect_true(
#     is.data.table(mc4)
#   )
# })



## this way avoids storing a .prepField and allows getting coverage there
test_that("tcplLoadData loads mc4 data for one m4id", {
  data("mc_test")
  mocked <- mc_test$mc4_m4id_12699347
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL")
  mc4 <- tcplLoadData(lvl = 4, fld = "m4id", val = 12699347)
  expect_true(
    is.data.table(mc4)
  )
})

test_that("tcplLoadData loads mc5 data for one m4id", {
  data("mc_test")
  mocked <- mc_test$mc5_m4id_12699347
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL")
  mc5 <- tcplLoadData(lvl = 5, fld = "m4id", val = 12699347)
  expect_true(
    is.data.table(mc5)
  )
})