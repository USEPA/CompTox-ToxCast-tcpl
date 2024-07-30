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
# Covers testing tcplLoadData with "MySQL" driver
# new method using mocking for tcplQuery function
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
  #expectations
  expect_true(is.data.table(dat))
  expect_true(nrow(dat) > 0)
  expect_true(all(c("m0id", "acid", "spid", "wllt", "wllq", "conc", "rval") %in% colnames(dat)))
  expect_true(mocked$m0id %in% dat$m0id)
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
  #expectations
  expect_true(is.data.table(dat))
  expect_true(nrow(dat) > 0)
  expect_true(all(c("m0id", "acid", "spid", "wllt", "wllq", "conc", "rval") %in% colnames(dat)))
  expect_true(mocked$acid %in% dat$acid)
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
  #expectations
  expect_true(is.data.table(dat))
  expect_true(nrow(dat) > 0)
  expect_true(all(c("m0id", "m1id", "acid", "spid", "wllt", "wllq", "conc", "rval") %in% colnames(dat)))
  expect_true(mocked$m1id %in% dat$m1id)
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
  #expectations
  expect_true(is.data.table(dat))
  expect_true(nrow(dat) > 0)
  expect_true(all(c("m0id", "m1id", "acid", "spid", "wllt", "wllq", "conc", "rval") %in% colnames(dat)))
  expect_true(mocked$acid %in% dat$acid)
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
  #expectations
  expect_true(is.data.table(dat))
  expect_true(nrow(dat) > 0)
  expect_true(all(c("m0id", "m1id", "m2id", "acid", "spid", "wllt", "wllq", "conc", "rval", "cval") %in% colnames(dat)))
  expect_true(mocked$m2id %in% dat$m2id)
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
  #expectations
  expect_true(is.data.table(dat))
  expect_true(nrow(dat) > 0)
  expect_true(all(c("m0id", "m1id", "m2id", "acid", "spid", "wllt", "wllq", "conc", "rval", "cval") %in% colnames(dat)))
  expect_true(mocked$acid %in% dat$acid)
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
  #expectations
  expect_true(is.data.table(dat))
  expect_true(nrow(dat) > 0)
  expect_true(all(c("m0id", "m1id", "m2id", "m3id", "acid", "aeid", "spid", "wllt", "wllq", "conc", "rval", "resp") %in% colnames(dat)))
  expect_true(mocked$m3id %in% dat$m3id)
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
  #expectations
  expect_true(is.data.table(dat))
  expect_true(nrow(dat) > 0)
  expect_true(all(c("m0id", "m1id", "m2id", "m3id", "acid", "aeid", "spid", "wllt", "wllq", "conc", "rval", "resp") %in% colnames(dat)))
  expect_true(mocked$aeid %in% dat$aeid)
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
  #expectations
  expect_true(is.data.table(dat))
  expect_true(nrow(dat) > 0)
  expect_true(all(c("m4id", "aeid", "spid", "bmad", "nconc", "nrep", "cnst_aic") %in% colnames(dat)))
  expect_true(mocked$m4id %in% dat$m4id)
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
  #expectations
  expect_true(is.data.table(dat))
  expect_true(nrow(dat) > 0)
  expect_true(all(c("m4id", "aeid", "spid", "bmad", "nconc", "nrep") %in% colnames(dat)))
  expect_true(mocked$aeid %in% dat$aeid)
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
  #expectations
  expect_true(is.data.table(dat))
  expect_true(nrow(dat) > 0)
  expect_true(all(c("m4id", "m5id", "aeid", "spid", "bmad", "nconc", "nrep", "modl", "hitc", "coff", "top") %in% colnames(dat)))
  expect_true(mocked$m5id %in% dat$m5id)
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
  #expectations
  expect_true(is.data.table(dat))
  expect_true(nrow(dat) > 0)
  expect_true(all(c("m4id", "m5id", "aeid", "spid", "bmad", "nconc", "nrep", "modl", "hitc", "coff") %in% colnames(dat)))
  expect_true(mocked$aeid %in% dat$aeid)
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
  #expectations
  expect_true(is.data.table(dat))
  expect_true(nrow(dat) > 0)
  expect_true(all(c("m4id", "m5id", "m6id", "aeid", "spid", "bmad", "nconc", "nrep", "flag") %in% colnames(dat)))
  expect_true(mocked$m6id %in% dat$m6id)
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
  #expectations
  expect_true(is.data.table(dat))
  expect_true(nrow(dat) > 0)
  expect_true(all(c("m4id", "m5id", "m6id", "aeid", "spid", "bmad", "nconc", "nrep", "flag") %in% colnames(dat)))
  expect_true(mocked$aeid %in% dat$aeid)
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
  #expectations
  expect_true(is.data.table(dat))
  expect_true(nrow(dat) > 0)
  expect_true(all(c("m4id", "m7id", "aeid", "spid", "bmad", "nconc", "nrep", "aed_val") %in% colnames(dat)))
  expect_true(mocked$m7id %in% dat$m7id)
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
  #expectations
  expect_true(is.data.table(dat))
  expect_true(nrow(dat) > 0)
  expect_true(all(c("m4id", "m7id", "aeid", "spid", "bmad", "nconc", "nrep", "aed_val") %in% colnames(dat)))
  expect_true(mocked$aeid %in% dat$aeid)
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
  #expectations
  expect_true(is.data.table(dat))
  expect_true(nrow(dat) > 0)
  expect_true(all(c("m0id", "m1id", "m2id", "m3id", "m4id", "acid", "aeid", "spid", 
                    "conc", "resp", "bmad", "nconc", "nrep") %in% colnames(dat)))
  expect_true(mocked$aeid %in% dat$aeid)
})

# error cases
test_that("invalid lvl and type error message", {
  data("mc_test")
  mocked <- mc_test$mcagg_by_aeid
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  expect_error(
    tcplLoadData(lvl = 5, type = "sc"),
    "Invalid 'lvl' and 'type' combination."
  )
})

test_that("missing val error message", {
  data("mc_test")
  mocked <- mc_test$mcagg_by_aeid
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  expect_error(
    tcplLoadData(lvl = 5, type = "mc", fld = "aeid"),
    "'val' cannot be NULL check that a valid value was provided for the specified field"
  )
})