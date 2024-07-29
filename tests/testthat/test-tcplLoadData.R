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


#-------------------------------------------------------------------------------
# new method using mocking for tcplQuery function
# TODO: add more expectations to check for columns/rows of the data table, etc
#-------------------------------------------------------------------------------

# MC0
test_that("tcplLoadData loads mc0 data for one m0id", {
  data("mc_test")
  mocked <- mc_test$mc0_by_m0id
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  dat <- tcplLoadData(lvl = 0, fld = "m0id", val = mocked$m0id)
  expect_true(
    is.data.table(dat)
  )
})

test_that("tcplLoadData loads mc0 data for one acid", {
  data("mc_test")
  mocked <- mc_test$mc0_by_acid
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  dat <- tcplLoadData(lvl = 0, fld = "acid", val = mocked$acid)
  expect_true(
    is.data.table(dat)
  )
})

# MC1
test_that("tcplLoadData loads mc1 data for one m1id", {
  data("mc_test")
  mocked <- mc_test$mc1_by_m1id
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  dat <- tcplLoadData(lvl = 1, fld = "m1id", val = mocked$m1id)
  expect_true(
    is.data.table(dat)
  )
})

test_that("tcplLoadData loads mc1 data for one acid", {
  data("mc_test")
  mocked <- mc_test$mc1_by_acid
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  dat <- tcplLoadData(lvl = 1, fld = "acid", val = mocked$acid)
  expect_true(
    is.data.table(dat)
  )
})

# MC2
test_that("tcplLoadData loads mc2 data for one m2id", {
  data("mc_test")
  mocked <- mc_test$mc2_by_m2id
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  dat <- tcplLoadData(lvl = 2, fld = "m2id", val = mocked$m2id)
  expect_true(
    is.data.table(dat)
  )
})

test_that("tcplLoadData loads mc2 data for one acid", {
  data("mc_test")
  mocked <- mc_test$mc2_by_acid
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  dat <- tcplLoadData(lvl = 2, fld = "acid", val = mocked$acid)
  expect_true(
    is.data.table(dat)
  )
})

# MC3
test_that("tcplLoadData loads mc3 data for one m3id", {
  data("mc_test")
  mocked <- mc_test$mc3_by_m3id
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  dat <- tcplLoadData(lvl = 3, fld = "m3id", val = mocked$m3id)
  expect_true(
    is.data.table(dat)
  )
})

test_that("tcplLoadData loads mc3 data for one aeid", {
  data("mc_test")
  mocked <- mc_test$mc3_by_aeid
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  dat <- tcplLoadData(lvl = 3, fld = "aeid", val = mocked$aeid)
  expect_true(
    is.data.table(dat)
  )
})

# MC4
test_that("tcplLoadData loads mc4 data for one m4id", {
  data("mc_test")
  mocked <- mc_test$mc4_by_m4id
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  dat <- tcplLoadData(lvl = 4, fld = "m4id", val = mocked$m4id)
  expect_true(
    is.data.table(dat)
  )
})

test_that("tcplLoadData loads mc4 data for one aeid with add.fld = FALSE", {
  data("mc_test")
  mocked <- mc_test$mc4_by_aeid
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  dat <- tcplLoadData(lvl = 4, fld = "aeid", val = mocked$aeid, add.fld = FALSE)
  expect_true(
    is.data.table(dat)
  )
})

# MC5
test_that("tcplLoadData loads mc5 data for one m5id", {
  data("mc_test")
  mocked <- mc_test$mc5_by_m5id
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  dat <- tcplLoadData(lvl = 5, fld = "m5id", val = mocked$m5id)
  expect_true(
    is.data.table(dat)
  )
})

test_that("tcplLoadData loads mc5 data for one aeid with add.fld = FALSE", {
  data("mc_test")
  mocked <- mc_test$mc5_by_aeid
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  dat <- tcplLoadData(lvl = 5, fld = "aeid", val = mocked$aeid, add.fld = FALSE)
  expect_true(
    is.data.table(dat)
  )
})

# MC6
test_that("tcplLoadData loads mc6 data for one m6id", {
  data("mc_test")
  mocked <- mc_test$mc6_by_m6id
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  dat <- tcplLoadData(lvl = 6, fld = "m6id", val = mocked$m6id)
  expect_true(
    is.data.table(dat)
  )
})

test_that("tcplLoadData loads mc6 data for one aeid", {
  data("mc_test")
  mocked <- mc_test$mc6_by_aeid
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  dat <- tcplLoadData(lvl = 6, fld = "aeid", val = mocked$aeid)
  expect_true(
    is.data.table(dat)
  )
})

# MC7
test_that("tcplLoadData loads mc7 data for one m7id", {
  data("mc_test")
  mocked <- mc_test$mc7_by_m7id
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  dat <- tcplLoadData(lvl = 7, fld = "m7id", val = mocked$m7id)
  expect_true(
    is.data.table(dat)
  )
})

test_that("tcplLoadData loads mc7 data for one aeid", {
  data("mc_test")
  mocked <- mc_test$mc7_by_aeid
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  dat <- tcplLoadData(lvl = 7, fld = "aeid", val = mocked$aeid)
  expect_true(
    is.data.table(dat)
  )
})

# MCagg
test_that("tcplLoadData loads mc 'agg' data for one aeid", {
  data("mc_test")
  mocked <- mc_test$mcagg_by_aeid
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  dat <- tcplLoadData(lvl = "agg", fld = "aeid", val = mocked$aeid)
  expect_true(
    is.data.table(dat)
  )
})
