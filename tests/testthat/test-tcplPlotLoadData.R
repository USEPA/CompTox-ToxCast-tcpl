#-------------------------------------------------------------------------------
# tests using drvr = "MySQL" and mocking
#-------------------------------------------------------------------------------
# Format
# test_that("something works", {
# data("mc_test")
# mocked <- mc_test$mc0_by_m0id # change this
# local_mocked_bindings(
#   tcplQuery = function(query, db, tbl) {
#     if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
#     else mocked[query][[1]]
#   }
# )
# tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
# })

#-------------------------------------------------------------------------------
# Test tcplPlotLoadData
#-------------------------------------------------------------------------------
test_that("tcplPlotLoadData loads all necessary mc data", {
  data("mc_test")
  mocked <- mc_test$plot_multiple_aeid_compare
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  dat <- tcplPlotLoadData(fld = "aeid", val = mocked$aeid, flags = TRUE)
  expect_true(is.data.table(dat))
  expect_true(nrow(dat) > 0)
  expect_true(all(c("spid", "chnm","dsstox_substance_id", "aeid", "aenm", "m4id", 
                    "resp_max", "resp_min", "conc_max", "conc_min", "hitc", "modl", 
                    "cnst_aic", "flag", "flag_count", "ac50", "conc", "resp", 
                    "coff", "bmr", "bmd", "top", "model_type", "conc_unit", 
                    "normalized_data_type") %in% colnames(dat)))
  expect_true(all(mocked$aeid %in% dat$aeid))
  expect_equal(nrow(dat[is.null(top) | is.na(top)]), 0) #top is never null/na
  expect_true(all(dat[top < 0, bmr] < 0)) # bmr negative if top negative
  expect_true(all(dat[model_type == 4, coff] < 0)) # coff negative if loss
  expect_true(all(dat[model_type == 3, coff] >= 0)) # coff positive if gain
  expect_true(all(dat[model_type == 2 & top < 0, coff] < 0)) # coff negative if bidirectional loss
  expect_true(all(dat[model_type == 2 & top > 0, coff] >= 0)) # coff positive if bidirectional gain
})

test_that("tcplPlotLoadData loads all necessary sc data", {
  data("sc_test")
  mocked <- sc_test$plot_multiple_aeid_compare
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") sc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  dat <- tcplPlotLoadData(type = "sc", fld = "aeid", val = mocked$aeid)
  expect_true(is.data.table(dat))
  expect_true(nrow(dat) > 0)
  expect_true(all(c("spid", "chnm","dsstox_substance_id", "aeid", "aenm", "s2id", 
                    "max_med", "hitc", "conc", "resp", "coff", "bmd", "top", 
                    "conc_unit", "normalized_data_type") %in% colnames(dat)))
  expect_true(all(mocked$aeid %in% dat$aeid))
  expect_equal(nrow(dat[is.null(top) | is.na(top)]), 0) #top is never null/na
  expect_true(all(dat[xor(max_med < 0, hitc < 0), coff] < 0)) # coff negative if exactly one of max_med/hitc are negative
  expect_true(all(dat[max_med < 0 & hitc < 0, coff] >= 0)) # coff positive both max_med/hitc are negative
  expect_true(all(dat[max_med >= 0 & hitc >= 0, coff] >= 0)) # coff positive both max_med/hitc are positive
})

test_that("tcplPlotLoadData errors if type is not 'mc' or 'sc'", {
  data("mc_test")
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  expect_error(tcplPlotLoadData(type = "ms", val = 123, flags = TRUE),
               "Invalid 'lvl' and 'type' combination.")
})