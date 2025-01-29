#-------------------------------------------------------------------------------
# old tests using drvr = "example"
#-------------------------------------------------------------------------------

tcplConf(drvr="example")
if (!interactive()) pdf(NULL)

test_that("round_n function works", {
  expect_equal(
    round_n(c(1234.5678, NA_real_, 0.3333),3),
    c("1.2e+03",NA,"0.333"))
  })


test_that("one m4id tcplPlot works", {
  skip()
  lvl = 5
  verbose = TRUE
  l4 <- tcplLoadData(lvl = 4, type = "mc", add.fld = TRUE)
  if (lvl >= 5L) {
    l5 <- mc_vignette[["mc5"]]
    dat <- l4[l5, on = c("m4id","aeid")]
    dat <- dat[,!c("tp","ga","q","la","ac50_loss")]
  }
  agg <- tcplLoadData(lvl = "agg", type = "mc")
  conc_resp_table <- agg %>% 
    group_by(m4id) %>% 
    summarise(conc = list(conc), resp = list(resp)) %>% 
    as.data.table()
  dat <- dat[conc_resp_table, on = "m4id"]
  dat <- dat[,normalized_data_type:="log2_fold_induction"]
  dat <- dat[spid == "01504209"]
  mc5_tcplplot <- tcplggplot2(dat,verbose = verbose)
  expect_no_warning(mc5_tcplplot)
  vdiffr::expect_doppelganger("test_output_482273", mc5_tcplplot)
})

# needs updates or removal; new spid is not in downward direction
test_that("negative direction plot has negative bmr and cutoff lines", {
  skip()
  lvl = 5
  verbose = FALSE
  l4 <- tcplLoadData(lvl = 4, type = "mc", add.fld = TRUE)
  if (lvl >= 5L) {
    l5 <- mc_vignette[["mc5"]]
    dat <- l4[l5, on = c("m4id","aeid")]
    dat <- dat[,!c("tp","ga","q","la","ac50_loss")]
  }
  agg <- tcplLoadData(lvl = "agg", type = "mc")
  conc_resp_table <- agg %>% 
    group_by(m4id) %>% 
    summarise(conc = list(conc), resp = list(resp)) %>% 
    as.data.table()
  dat <- dat[conc_resp_table, on = "m4id"]
  dat <- dat[,normalized_data_type:="log2_fold_induction"]
  dat <- dat[spid == "01504209"]  
  mc5_tcplplot <- tcplggplot2(dat,verbose = verbose)
  vdiffr::expect_doppelganger("negative_cutoff_bmr", mc5_tcplplot)
})


#-------------------------------------------------------------------------------
# new tests using drvr = "MySQL" and mocking
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
# Test tcplPlot - mc
#-------------------------------------------------------------------------------
test_that("tcplPlot works for single m4id", {
  data("mc_test")
  mocked <- mc_test$plot_single_m4id
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  expect_no_error(tcplPlot(type = "mc", fld = "m4id", val = mocked$m4id, output = "pdf", verbose = TRUE, flags = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot"))
  expect_no_error(tcplPlot(type = "mc", fld = "m4id", val = mocked$m4id, output = "pdf", verbose = FALSE, multi = TRUE, fileprefix = "temp_tcplPlot"))
  expect_no_error(tcplPlot(type = "mc", fld = "m4id", val = mocked$m4id, output = "console", verbose = TRUE, flags = TRUE, multi = TRUE))
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 1) # exactly one file created
  file.remove(fn) # clean up
})

test_that("tcplPlot works for multiple m4id", {
  data("mc_test")
  mocked <- mc_test$plot_multiple_m4id
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  expect_no_error(tcplPlot(type = "mc", fld = "m4id", val = mocked$m4id, output = "pdf", verbose = TRUE, flags = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot"))
  expect_no_error(tcplPlot(type = "mc", fld = "m4id", val = mocked$m4id, output = "pdf", verbose = FALSE, multi = TRUE, fileprefix = "temp_tcplPlot"))
  # console does not work with length(val) > 1
  expect_error(tcplPlot(type = "mc", fld = "m4id", val = mocked$m4id, output = "console", verbose = TRUE, flags = TRUE, multi = TRUE), 
               "More than 1 concentration series returned for given field/val combination. Set output to pdf or reduce the number of plots to 1. Current number of individual plots: 2")
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 1) # exactly one file created
  file.remove(fn) # clean up
})

test_that("tcplPlot works for single aeid mc", {
  data("mc_test")
  mocked <- mc_test$plot_single_aeid
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  expect_no_error(tcplPlot(type = "mc", fld = "aeid", val = mocked$aeid, output = "pdf", verbose = TRUE, flags = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot"))
  expect_no_error(tcplPlot(type = "mc", fld = "aeid", val = mocked$aeid, output = "pdf", verbose = FALSE, multi = TRUE, fileprefix = "temp_tcplPlot"))
  # console does not work with length(val) > 1
  expect_error(tcplPlot(type = "mc", fld = "aeid", val = mocked$aeid, output = "console", verbose = TRUE, flags = TRUE, multi = TRUE), 
               "More than 1 concentration series returned for given field/val combination. Set output to pdf or reduce the number of plots to 1. Current number of individual plots: 2")  
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 1) # exactly one file created
  file.remove(fn) # clean up
})

test_that("tcplPlot works for multiple aeid mc", {
  data("mc_test")
  mocked <- mc_test$plot_multiple_aeid
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  expect_no_error(tcplPlot(type = "mc", fld = "aeid", val = mocked$aeid, output = "pdf", verbose = TRUE, flags = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot"))
  expect_no_error(tcplPlot(type = "mc", fld = "aeid", val = mocked$aeid, output = "pdf", by = "aeid", verbose = TRUE, flags = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot"))
  expect_no_error(tcplPlot(type = "mc", fld = "aeid", val = mocked$aeid, output = "pdf", verbose = FALSE, flags = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot"))
  expect_no_error(tcplPlot(type = "mc", fld = "aeid", val = mocked$aeid, output = "pdf", verbose = FALSE, flags = TRUE, by = "aeid", multi = TRUE, fileprefix = "temp_tcplPlot"))
  # console does not work with length(val) > 1
  expect_error(tcplPlot(type = "mc", fld = "aeid", val = mocked$aeid, output = "console", verbose = TRUE, flags = TRUE, multi = TRUE), 
               "More than 1 concentration series returned for given field/val combination. Set output to pdf or reduce the number of plots to 1. Current number of individual plots: 4")
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 3) # exactly three files created (2 from `by = "aeid"` output)
  file.remove(fn) # clean up
})

test_that("tcplPlot works for single spid/aeid mc", {
  data("mc_test")
  mocked <- mc_test$plot_single_spid
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  expect_no_error(tcplPlot(type = "mc", fld = c("spid", "aeid"), val = list(mocked$spid, mocked$aeid), output = "pdf", verbose = TRUE, flags = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot"))
  expect_no_error(tcplPlot(type = "mc", fld = c("spid", "aeid"), val = list(mocked$spid, mocked$aeid), output = "pdf", verbose = FALSE, multi = TRUE, fileprefix = "temp_tcplPlot"))
  expect_no_error(tcplPlot(type = "mc", fld = c("spid", "aeid"), val = list(mocked$spid, mocked$aeid), output = "console", verbose = TRUE, flags = TRUE, multi = TRUE))
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 1) # exactly one file created
  file.remove(fn) # clean up
})

test_that("tcplPlot works for multiple spid/aeid mc", {
  data("mc_test")
  mocked <- mc_test$plot_multiple_spid
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  expect_no_error(tcplPlot(type = "mc", fld = c("spid", "aeid"), val = list(mocked$spid, mocked$aeid), output = "pdf", verbose = TRUE, flags = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot"))
  expect_no_error(tcplPlot(type = "mc", fld = c("spid", "aeid"), val = list(mocked$spid, mocked$aeid), output = "pdf", verbose = FALSE, multi = TRUE, fileprefix = "temp_tcplPlot"))
  # console does not work with length(val) > 1
  expect_error(tcplPlot(type = "mc", fld = c("spid", "aeid"), val = list(mocked$spid, mocked$aeid), output = "console", verbose = TRUE, flags = TRUE, multi = TRUE), 
               "More than 1 concentration series returned for given field/val combination. Set output to pdf or reduce the number of plots to 1. Current number of individual plots: 2")
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 1) # exactly one file created
  file.remove(fn) # clean up
})

test_that("tcplPlot works for single m4id compared", {
  data("mc_test")
  mocked <- mc_test$plot_single_m4id_compare
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  expect_no_error(tcplPlot(type = "mc", fld = "m4id", val = mocked$m4id, compare = "chnm", output = "pdf", verbose = TRUE, flags = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot"))
  expect_no_error(tcplPlot(type = "mc", fld = "m4id", val = mocked$m4id, compare = "chnm", output = "pdf", verbose = FALSE, multi = TRUE, fileprefix = "temp_tcplPlot"))
  expect_no_error(tcplPlot(type = "mc", fld = "m4id", val = mocked$m4id, compare = "chnm", output = "console", verbose = TRUE, flags = TRUE, multi = TRUE))
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 1) # exactly one file created
  file.remove(fn) # clean up
})

test_that("tcplPlot works for multiple m4id compared", {
  data("mc_test")
  mocked <- mc_test$plot_multiple_m4id_compare
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  expect_no_error(tcplPlot(type = "mc", fld = "m4id", val = mocked$m4id, compare = "chnm", output = "pdf", verbose = TRUE, flags = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot"))
  expect_no_error(suppressWarnings(tcplPlot(type = "mc", fld = "m4id", val = mocked$m4id, compare = "chnm", output = "pdf", verbose = FALSE, flags = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot")))
  # console does not work with length(val) > 1
  expect_error(tcplPlot(type = "mc", fld = "m4id", val = mocked$m4id, compare = "chnm", output = "console", verbose = TRUE, flags = TRUE, multi = TRUE), 
               "More than 1 concentration series returned for given field/val combination. Set output to pdf or reduce the number of plots to 1. Current number of individual plots: 2")
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 1) # exactly one file created
  file.remove(fn) # clean up
})

test_that("tcplPlot works for single aeid compared mc", {
  data("mc_test")
  mocked <- mc_test$plot_single_aeid_compare
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  expect_no_error(tcplPlot(type = "mc", fld = "aeid", val = mocked$aeid, compare = "chnm", output = "pdf", verbose = TRUE, flags = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot"))
  expect_no_error(tcplPlot(type = "mc", fld = "aeid", val = mocked$aeid, compare = "chnm", output = "pdf", verbose = FALSE, multi = TRUE, fileprefix = "temp_tcplPlot"))
  # console does not work with length(val) > 1
  expect_error(tcplPlot(type = "mc", fld = "aeid", val = mocked$aeid, compare = "chnm", output = "console", verbose = TRUE, flags = TRUE, multi = TRUE), 
               "More than 1 concentration series returned for given field/val combination. Set output to pdf or reduce the number of plots to 1. Current number of individual plots: 2")  
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 1) # exactly one file created
  file.remove(fn) # clean up
})

test_that("tcplPlot works for multiple aeid compared mc", {
  data("mc_test")
  mocked <- mc_test$plot_multiple_aeid_compare
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  expect_no_error(tcplPlot(type = "mc", fld = "aeid", val = mocked$aeid, compare = c("chnm", "model_type"), output = "pdf", verbose = TRUE, flags = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot"))
  expect_no_error(suppressWarnings(tcplPlot(type = "mc", fld = "aeid", val = mocked$aeid, compare = c("chnm", "model_type"), output = "pdf", verbose = FALSE, multi = TRUE, flags = TRUE, fileprefix = "temp_tcplPlot")))
  # console does not work with length(val) > 1
  expect_error(tcplPlot(type = "mc", fld = "aeid", val = mocked$aeid, compare = c("chnm", "model_type"), output = "console", verbose = TRUE, flags = TRUE, multi = TRUE), 
               "More than 1 concentration series returned for given field/val combination. Set output to pdf or reduce the number of plots to 1. Current number of individual plots: 4")
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 1) # exactly one file created
  file.remove(fn) # clean up
})

test_that("tcplPlot works for single spid/aeid compared mc", {
  data("mc_test")
  mocked <- mc_test$plot_single_spid_compare
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  expect_no_error(tcplPlot(type = "mc", fld = c("spid", "aeid"), val = list(mocked$spid, mocked$aeid), compare = "chnm", output = "pdf", verbose = TRUE, flags = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot"))
  expect_no_error(tcplPlot(type = "mc", fld = c("spid", "aeid"), val = list(mocked$spid, mocked$aeid), compare = "chnm", output = "pdf", verbose = FALSE, multi = TRUE, fileprefix = "temp_tcplPlot"))
  expect_no_error(tcplPlot(type = "mc", fld = c("spid", "aeid"), val = list(mocked$spid, mocked$aeid), compare = "chnm", output = "console", verbose = TRUE, flags = TRUE, multi = TRUE))  
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 1) # exactly one file created
  file.remove(fn) # clean up
})

test_that("tcplPlot works for multiple spid/aeid compared mc", {
  data("mc_test")
  mocked <- mc_test$plot_multiple_spid_compare
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  expect_no_error(tcplPlot(type = "mc", fld = c("spid", "aeid"), val = list(mocked$spid, mocked$aeid), compare = "chnm", output = "pdf", verbose = TRUE, flags = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot"))
  expect_no_error(suppressWarnings(tcplPlot(type = "mc", fld = c("spid", "aeid"), val = list(mocked$spid, mocked$aeid), compare = "chnm", output = "pdf", verbose = FALSE, flags = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot")))
  # console does not work with length(val) > 1
  expect_error(tcplPlot(type = "mc", fld = c("spid", "aeid"), val = list(mocked$spid, mocked$aeid), compare = "chnm", output = "console", verbose = TRUE, flags = TRUE, multi = TRUE), 
               "More than 1 concentration series returned for given field/val combination. Set output to pdf or reduce the number of plots to 1. Current number of individual plots: 2")
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 1) # exactly one file created
  file.remove(fn) # clean up
})

test_that("tcplPlot works for large comparisons greater than group.threshold mc", {
  data("mc_test")
  mocked <- mc_test$plot_multiple_aeid_compare
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  expect_no_error(tcplPlot(type = "mc", fld = "aeid", val = mocked$aeid, compare = "normalized_data_type", output = "pdf", verbose = TRUE, flags = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot", group.threshold = 8))
  expect_no_error(tcplPlot(type = "mc", fld = "aeid", val = mocked$aeid, compare = "normalized_data_type", output = "pdf", verbose = FALSE, multi = TRUE, flags = TRUE, fileprefix = "temp_tcplPlot", group.threshold = 8, group.fld = "model_type"))
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 1) # exactly one file created
  file.remove(fn) # clean up
})

test_that("tcplPlot works for large comparisons greater than group.threshold sc", {
  data("sc_test")
  mocked <- sc_test$plot_multiple_aeid_compare
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  expect_no_error(tcplPlot(type = "sc", fld = "aeid", val = mocked$aeid, compare = "normalized_data_type", output = "pdf", verbose = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot", group.threshold = 12))
  expect_no_error(tcplPlot(type = "sc", fld = "aeid", val = mocked$aeid, compare = "normalized_data_type", output = "pdf", verbose = FALSE, multi = TRUE, fileprefix = "temp_tcplPlot", group.threshold = 12, group.fld = "coff"))
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 1) # exactly one file created
  file.remove(fn) # clean up
})

test_that("tcplPlot errors if 'compare' or 'by' are not fields in 'dat'", {
  data("mc_test")
  mocked <- mc_test$plot_single_m4id_compare
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  expect_error(tcplPlot(val = mocked$m4id, compare = "chnm", by = "aid", output = "pdf", fileprefix = "temp_tcplPlot"),
               "'by' must be a valid field contained in dat.")
  expect_error(tcplPlot(val = mocked$m4id, compare = "dtxsid", by = "aeid", output = "pdf", fileprefix = "temp_tcplPlot"),
               "'compare' must all be valid field\\(s\\) contained in dat.")
})

#-------------------------------------------------------------------------------
# Test tcplPlot - sc
#-------------------------------------------------------------------------------
test_that("tcplPlot works for single s2id", {
  data("sc_test")
  mocked <- sc_test$plot_single_s2id
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") sc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  expect_no_error(tcplPlot(type = "sc", fld = "s2id", val = mocked$s2id, output = "pdf", verbose = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot"))
  expect_no_error(tcplPlot(type = "sc", fld = "s2id", val = mocked$s2id, output = "pdf", verbose = FALSE, multi = TRUE, fileprefix = "temp_tcplPlot"))
  expect_no_error(tcplPlot(type = "sc", fld = "s2id", val = mocked$s2id, output = "console", verbose = TRUE, multi = TRUE))
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 1) # exactly one file created
  file.remove(fn) # clean up
})

test_that("tcplPlot works for multiple s2id", {
  data("sc_test")
  mocked <- sc_test$plot_multiple_s2id
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") sc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  expect_no_error(tcplPlot(type = "sc", fld = "s2id", val = mocked$s2id, output = "pdf", verbose = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot"))
  expect_no_error(tcplPlot(type = "sc", fld = "s2id", val = mocked$s2id, output = "pdf", verbose = FALSE, multi = TRUE, fileprefix = "temp_tcplPlot"))
  # console does not work with length(val) > 1
  expect_error(tcplPlot(type = "sc", fld = "s2id", val = mocked$s2id, output = "console", verbose = TRUE, multi = TRUE), 
               "More than 1 concentration series returned for given field/val combination. Set output to pdf or reduce the number of plots to 1. Current number of individual plots: 2")
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 1) # exactly one file created
  file.remove(fn) # clean up
})

test_that("tcplPlot works for single aeid sc", {
  data("sc_test")
  mocked <- sc_test$plot_single_aeid
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") sc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  expect_no_error(tcplPlot(type = "sc", fld = "aeid", val = mocked$aeid, output = "pdf", verbose = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot"))
  expect_no_error(tcplPlot(type = "sc", fld = "aeid", val = mocked$aeid, output = "pdf", verbose = FALSE, multi = TRUE, fileprefix = "temp_tcplPlot"))
  # console does not work with length(val) > 1
  expect_error(tcplPlot(type = "sc", fld = "aeid", val = mocked$aeid, output = "console", verbose = TRUE, multi = TRUE), 
               "More than 1 concentration series returned for given field/val combination. Set output to pdf or reduce the number of plots to 1. Current number of individual plots: 7")  
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 1) # exactly one file created
  file.remove(fn) # clean up
})

test_that("tcplPlot works for multiple aeid sc", {
  data("sc_test")
  mocked <- sc_test$plot_multiple_aeid
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") sc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  expect_no_error(tcplPlot(type = "sc", fld = "aeid", val = mocked$aeid, output = "pdf", verbose = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot"))
  expect_no_error(tcplPlot(type = "sc", fld = "aeid", val = mocked$aeid, output = "pdf", verbose = FALSE, multi = TRUE, fileprefix = "temp_tcplPlot"))
  expect_no_error(tcplPlot(type = "sc", fld = "aeid", val = mocked$aeid, by = "aeid", output = "pdf", verbose = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot"))
  # console does not work with length(val) > 1
  expect_error(tcplPlot(type = "sc", fld = "aeid", val = mocked$aeid, output = "console", verbose = TRUE, multi = TRUE), 
               "More than 1 concentration series returned for given field/val combination. Set output to pdf or reduce the number of plots to 1. Current number of individual plots: 14")
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 3) # exactly one file created
  file.remove(fn) # clean up
})

test_that("tcplPlot works for single spid/aeid sc", {
  data("sc_test")
  mocked <- sc_test$plot_single_spid
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") sc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  expect_no_error(tcplPlot(type = "sc", fld = c("spid", "aeid"), val = list(mocked$spid, mocked$aeid), output = "pdf", verbose = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot"))
  expect_no_error(tcplPlot(type = "sc", fld = c("spid", "aeid"), val = list(mocked$spid, mocked$aeid), output = "pdf", verbose = FALSE, multi = TRUE, fileprefix = "temp_tcplPlot"))
  expect_no_error(tcplPlot(type = "sc", fld = c("spid", "aeid"), val = list(mocked$spid, mocked$aeid), output = "console", verbose = TRUE, multi = TRUE))  
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 1) # exactly one file created
  file.remove(fn) # clean up
})

test_that("tcplPlot works for multiple spid/aeid sc", {
  data("sc_test")
  mocked <- sc_test$plot_multiple_spid
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") sc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  expect_no_error(tcplPlot(type = "sc", fld = c("spid", "aeid"), val = list(mocked$spid, mocked$aeid), output = "pdf", verbose = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot"))
  expect_no_error(tcplPlot(type = "sc", fld = c("spid", "aeid"), val = list(mocked$spid, mocked$aeid), output = "pdf", verbose = FALSE, multi = TRUE, fileprefix = "temp_tcplPlot"))
  # console does not work with length(val) > 1
  expect_error(tcplPlot(type = "sc", fld = c("spid", "aeid"), val = list(mocked$spid, mocked$aeid), output = "console", verbose = TRUE, multi = TRUE), 
               "More than 1 concentration series returned for given field/val combination. Set output to pdf or reduce the number of plots to 1. Current number of individual plots: 2")
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 1) # exactly one file created
  file.remove(fn) # clean up
})

test_that("tcplPlot works for single s2id compared", {
  data("sc_test")
  mocked <- sc_test$plot_single_s2id_compare
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") sc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  expect_no_error(tcplPlot(type = "sc", fld = "s2id", val = mocked$s2id, compare = "spid", output = "pdf", verbose = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot"))
  expect_no_error(tcplPlot(type = "sc", fld = "s2id", val = mocked$s2id, compare = "spid", output = "pdf", verbose = FALSE, multi = TRUE, fileprefix = "temp_tcplPlot"))
  expect_no_error(tcplPlot(type = "sc", fld = "s2id", val = mocked$s2id, compare = "spid", output = "console", verbose = TRUE, multi = TRUE))
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 1) # exactly one file created
  file.remove(fn) # clean up
})

test_that("tcplPlot works for multiple s2id compared", {
  data("sc_test")
  mocked <- sc_test$plot_multiple_s2id_compare
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") sc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  expect_no_error(tcplPlot(type = "sc", fld = "s2id", val = mocked$s2id, compare = "spid", output = "pdf", verbose = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot"))
  expect_no_error(tcplPlot(type = "sc", fld = "s2id", val = mocked$s2id, compare = "spid", output = "pdf", verbose = FALSE, multi = TRUE, fileprefix = "temp_tcplPlot"))
  # console does not work with length(val) > 1
  expect_error(tcplPlot(type = "sc", fld = "s2id", val = mocked$s2id, compare = "spid", output = "console", verbose = TRUE, multi = TRUE), 
               "More than 1 concentration series returned for given field/val combination. Set output to pdf or reduce the number of plots to 1. Current number of individual plots: 2")
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 1) # exactly one file created
  file.remove(fn) # clean up
})

test_that("tcplPlot works for single aeid compared sc", {
  data("sc_test")
  mocked <- sc_test$plot_single_aeid_compare
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") sc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  expect_no_error(tcplPlot(type = "sc", fld = "aeid", val = mocked$aeid, compare = "spid", output = "pdf", verbose = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot"))
  expect_no_error(tcplPlot(type = "sc", fld = "aeid", val = mocked$aeid, compare = "spid", output = "pdf", verbose = FALSE, multi = TRUE, fileprefix = "temp_tcplPlot"))
  # console does not work with length(val) > 1
  expect_error(tcplPlot(type = "sc", fld = "aeid", val = mocked$aeid, compare = "spid", output = "console", verbose = TRUE, multi = TRUE), 
               "More than 1 concentration series returned for given field/val combination. Set output to pdf or reduce the number of plots to 1. Current number of individual plots: 7")  
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 1) # exactly one file created
  file.remove(fn) # clean up
})

test_that("tcplPlot works for multiple aeid compared sc", {
  data("sc_test")
  mocked <- sc_test$plot_multiple_aeid_compare
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") sc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  expect_no_error(tcplPlot(type = "sc", fld = "aeid", val = mocked$aeid, compare = "spid", output = "pdf", verbose = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot"))
  expect_no_error(tcplPlot(type = "sc", fld = "aeid", val = mocked$aeid, compare = "spid", output = "pdf", verbose = FALSE, multi = TRUE, fileprefix = "temp_tcplPlot"))
  # console does not work with length(val) > 1
  expect_error(tcplPlot(type = "sc", fld = "aeid", val = mocked$aeid, compare = "spid", output = "console", verbose = TRUE, multi = TRUE), 
               "More than 1 concentration series returned for given field/val combination. Set output to pdf or reduce the number of plots to 1. Current number of individual plots: 7")
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 1) # exactly one file created
  file.remove(fn) # clean up
})

test_that("tcplPlot works for single spid/aeid compared sc", {
  data("sc_test")
  mocked <- sc_test$plot_single_spid_compare
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") sc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  expect_no_error(tcplPlot(type = "sc", fld = c("spid", "aeid"), val = list(mocked$spid, mocked$aeid), compare = "spid", output = "pdf", verbose = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot"))
  expect_no_error(tcplPlot(type = "sc", fld = c("spid", "aeid"), val = list(mocked$spid, mocked$aeid), compare = "spid", output = "pdf", verbose = FALSE, multi = TRUE, fileprefix = "temp_tcplPlot"))
  expect_no_error(tcplPlot(type = "sc", fld = c("spid", "aeid"), val = list(mocked$spid, mocked$aeid), compare = "spid", output = "console", verbose = TRUE, multi = TRUE))  
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 1) # exactly one file created
  file.remove(fn) # clean up
})

test_that("tcplPlot works for multiple spid/aeid compared sc", {
  data("sc_test")
  mocked <- sc_test$plot_multiple_spid_compare
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") sc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  expect_no_error(tcplPlot(type = "sc", fld = c("spid", "aeid"), val = list(mocked$spid, mocked$aeid), compare = "spid", output = "pdf", verbose = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot"))
  expect_no_error(tcplPlot(type = "sc", fld = c("spid", "aeid"), val = list(mocked$spid, mocked$aeid), compare = "spid", output = "pdf", verbose = FALSE, multi = TRUE, fileprefix = "temp_tcplPlot"))
  # console does not work with length(val) > 1
  expect_error(tcplPlot(type = "sc", fld = c("spid", "aeid"), val = list(mocked$spid, mocked$aeid), compare = "spid", output = "console", verbose = TRUE, multi = TRUE), 
               "More than 1 concentration series returned for given field/val combination. Set output to pdf or reduce the number of plots to 1. Current number of individual plots: 2")
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 1) # exactly one file created
  file.remove(fn) # clean up
})

#-------------------------------------------------------------------------------
# Stand-alone plotting
#-------------------------------------------------------------------------------
test_that("standalone plotting works in mc", {
  data("mc_test")
  mocked <- mc_test$plot_single_m4id
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  dat <- tcplPlotLoadData(val = mocked$m4id, flags = TRUE)
  expect_no_error(tcplPlot(dat = dat, output = "pdf", verbose = TRUE, flags = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot"))
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 1) # exactly one file created
  file.remove(fn) # clean up
})

test_that("standalone advanced comparison plotting works in mc", {
  data("mc_test")
  mocked <- mc_test$plot_single_m4id_compare
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  dat <- tcplPlotLoadData(val = mocked$m4id, flags = TRUE)
  expect_no_error(tcplPlot(dat = dat, compare = "chnm", output = "pdf", verbose = TRUE, flags = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot"))
  dat <- split(dat, by = "chnm")
  expect_no_error(tcplPlot(dat = dat, output = "pdf", verbose = TRUE, flags = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot"))
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 1) # exactly one file created
  file.remove(fn) # clean up
})

test_that("standalone plotting works in sc", {
  data("sc_test")
  mocked <- sc_test$plot_single_s2id
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") sc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  dat <- tcplPlotLoadData(type = "sc", fld = "s2id", val = mocked$s2id)
  expect_no_error(tcplPlot(dat = dat, type = "sc", fld = "s2id", val = mocked$s2id, output = "pdf", verbose = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot"))
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 1) # exactly one file created
  file.remove(fn) # clean up
})

test_that("standalone advanced comparison plotting works in sc", {
  data("sc_test")
  mocked <- sc_test$plot_single_s2id_compare
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") sc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  dat <- tcplPlotLoadData(type = "sc", fld = "s2id", val = mocked$s2id)
  expect_no_error(tcplPlot(dat = dat, type = "sc", compare = "spid", output = "pdf", verbose = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot"))
  dat <- split(dat, by = "spid")
  expect_no_error(tcplPlot(dat = dat, type = "sc", output = "pdf", verbose = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot"))
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 1) # exactly one file created
  file.remove(fn) # clean up
})

#-------------------------------------------------------------------------------
# "ggplot" output tests
#-------------------------------------------------------------------------------
test_that("ggplot output works in mc", {
  data("mc_test")
  mocked <- mc_test$plot_single_m4id
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  expect_no_error(otpt <- tcplPlot(type = "mc", fld = "m4id", val = mocked$m4id, verbose = FALSE, output = "ggplot", fileprefix = "temp_tcplPlot"))
  expect_true(ggplot2::is.ggplot(otpt))
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 0) # no files should be generated
})

test_that("ggplot output comparison plotting works in mc", {
  data("mc_test")
  mocked <- mc_test$plot_single_m4id_compare
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  expect_no_error(otpt <- tcplPlot(type = "mc", fld = "m4id", val = mocked$m4id, compare = "chnm", verbose = FALSE, output = "ggplot", fileprefix = "temp_tcplPlot"))
  expect_true(ggplot2::is.ggplot(otpt))
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 0) # no files should be generated
})

test_that("ggplot output works in sc", {
  data("sc_test")
  mocked <- sc_test$plot_single_s2id
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") sc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  expect_no_error(otpt <- tcplPlot(type = "sc", fld = "s2id", val = mocked$s2id, verbose = FALSE, output = "ggplot", fileprefix = "temp_tcplPlot"))
  expect_true(ggplot2::is.ggplot(otpt))
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 0) # no files should be generated
})

test_that("ggplot output comparison plotting works in sc", {
  data("sc_test")
  mocked <- sc_test$plot_single_s2id_compare
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") sc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  expect_no_error(otpt <- tcplPlot(type = "sc", fld = "s2id", val = mocked$s2id, compare = "spid", verbose = FALSE, output = "ggplot", fileprefix = "temp_tcplPlot"))
  expect_true(ggplot2::is.ggplot(otpt))
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 0) # no files should be generated
})

test_that("ggplot output returns specific warnings and errors", {
  data("mc_test")
  mocked <- mc_test$plot_multiple_m4id
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  expect_error(tcplPlot(type = "mc", fld = "m4id", val = mocked$m4id, output = "ggplot", flags = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot"),
               "More than 1 concentration series returned for given field/val combination. Set output to pdf or reduce the number of plots to 1. Current number of individual plots: 2")
  mocked <- mc_test$plot_single_m4id
  expect_message(tcplPlot(type = "mc", fld = "m4id", val = mocked$m4id, output = "ggplot", verbose = TRUE, flags = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot"),
                 "ggplot and verbose table arranged into one grob. To work with a simple ggplot object, set `verbose = FALSE` and 'flags = FALSE'.")
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 0) # no files should be generated
})


#-------------------------------------------------------------------------------
# loec plotting tests
#-------------------------------------------------------------------------------
test_that("loec plotting works for single plot", {
  data("mc_test")
  mocked <- mc_test$plot_single_m4id
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  dat <- tcplPlotLoadData(val = mocked$m4id, flags = TRUE)
  dat[, c("modl", "fitc", "model_type", "hitc", "bmr", "bmd", "ac50") := list("loec", 100L, 1, 1, NA, NA, NA)]
  dat$loec <- sort(unlist(dat$conc), decreasing = TRUE)[1] # set loec to highest tested conc
  expect_no_error(tcplPlot(dat = dat, output = "pdf", verbose = TRUE, flags = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot"))
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 1) # exactly one file created
  file.remove(fn) # clean up
})

test_that("loec plotting works for small comparison plot", {
  data("mc_test")
  mocked <- mc_test$plot_single_m4id_compare
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  dat <- tcplPlotLoadData(val = mocked$m4id, flags = TRUE)
  dat[, c("modl", "fitc", "model_type", "hitc", "bmr", "bmd", "ac50") := list("loec", 100L, 1, 1, NA, NA, NA)]
  dat$loec <- sort(unlist(dat$conc), decreasing = TRUE)[1:2] # set loec to highest tested concs
  expect_no_error(tcplPlot(dat = dat, compare = "chnm", output = "pdf", verbose = TRUE, flags = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot"))
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 1) # exactly one file created
  file.remove(fn) # clean up
})

test_that("loec plotting works for large comparison plot", {
  data("mc_test")
  mocked <- mc_test$plot_multiple_aeid_compare
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  dat_copy <- dat <- tcplPlotLoadData(fld = "aeid", val = mocked$aeid, flags = TRUE)
  dat <- dat[1:4]
  dat[, c("modl", "fitc", "model_type", "hitc", "bmr", "bmd", "ac50") := list("loec", 100L, 1, 1, NA, NA, NA)]
  dat$loec <- c(sort(unlist(dat$conc), decreasing = TRUE)[1:4], rep(NA, nrow(dat) - 4)) 
  dat <- rbind(dat, dat_copy[5:nrow(dat_copy)], fill = TRUE)
  expect_no_error(tcplPlot(dat = dat, compare = "normalized_data_type", output = "pdf", verbose = TRUE, flags = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot", group.threshold = 8))
  expect_no_error(tcplPlot(dat = dat, compare = "normalized_data_type", output = "pdf", verbose = TRUE, flags = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot"))
  dat <- dat_copy
  dat[, c("modl", "fitc", "model_type", "hitc", "bmr", "bmd", "ac50") := list("loec", 100L, 1, 1, NA, NA, NA)]
  dat$loec <- c(sort(unlist(dat$conc), decreasing = TRUE)[1:4], rep(NA, nrow(dat) - 4)) 
  expect_no_error(tcplPlot(dat = dat, compare = "normalized_data_type", output = "pdf", verbose = TRUE, flags = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot", group.threshold = 8))
  fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
  expect_length(fn, 1) # exactly one file created
  file.remove(fn) # clean up
})


#-------------------------------------------------------------------------------
# Covers testing tcplLoadData with "API" driver
# Using httptest mocking to automatically save json responses from http requests
# NOTE -- updates to the CTX API may mean stored json files are out of date. In 
# this case, delete the 'ctx' folder and rerun this ENTIRE test file (temporarily
# replacing the 'apikey' string with a valid key) to repopulate the stored 
# .jsons. These will likely be huge and will need to be edited by hand to reduce
# their sizes. To do this, open the file(s) and remove all but one element of
# the outer array -- we don't need more than one endpoint-sample.
#-------------------------------------------------------------------------------
httptest::with_mock_dir("ctx", {
  apikey <- "apikey"
  tcplConf(pass = apikey,
           drvr = "API")
  data(test_api)
  test_that("tcplPlot works with API data by m4id", {
    expect_no_error(tcplPlot(val = test_api$m4id, output = "pdf", verbose = TRUE, flags = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot"))
    fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
    expect_length(fn, 1) # exactly one file created
    file.remove(fn) # clean up
  })
  test_that("tcplPlot works with API data by aeid", {
    expect_no_error(tcplPlot(fld = "aeid", val = test_api$aeid, output = "pdf", verbose = TRUE, flags = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot"))
    fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
    expect_length(fn, 1) # exactly one file created
    file.remove(fn) # clean up
  })
  test_that("tcplPlot works with API data by spid", {
    expect_no_error(tcplPlot(fld = "spid", val = test_api$spid, output = "pdf", verbose = TRUE, flags = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot"))
    fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
    expect_length(fn, 1) # exactly one file created
    file.remove(fn) # clean up
  })
  test_that("tcplPlot works with API data by dtxsid", {
    expect_no_error(tcplPlot(fld = "dtxsid", val = test_api$dtxsid, output = "pdf", verbose = TRUE, flags = TRUE, multi = TRUE, fileprefix = "temp_tcplPlot"))
    fn <- stringr::str_subset(list.files(), "^temp_tcplPlot")
    expect_length(fn, 1) # exactly one file created
    file.remove(fn) # clean up
  })
})