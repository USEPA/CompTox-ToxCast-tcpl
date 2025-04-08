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

pdf(NULL) # prevent automatic RPlots.pdf output due to not storing gtable in object

#-------------------------------------------------------------------------------
# Test MORELETTERS
#-------------------------------------------------------------------------------
test_that("MORELETTERS works", {
  expect_error(MORELETTERS(0))
  expect_equal(MORELETTERS(1), "A")
  expect_equal(MORELETTERS(10), "J")
  expect_equal(MORELETTERS(27), "AA")
  expect_equal(MORELETTERS(100), "CV")
  expect_equal(MORELETTERS(1000), "ALL")
  expect_equal(MORELETTERS(1:26), LETTERS)
})


#-------------------------------------------------------------------------------
# Test tcplfit2_fun
#-------------------------------------------------------------------------------
test_that("tcplfit2_fun works", {
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
  model_vals <- tcplfit2_fun(dat, dat$modl, 1:10)
  expect_true(is.numeric(model_vals))
  expect_equal(length(model_vals[!is.na(model_vals)]), 10)
})


#-------------------------------------------------------------------------------
# Test round_n
#-------------------------------------------------------------------------------
test_that("round_n works", {
  expect_equal(round_n(0.00001),"1.0e-05")
  expect_equal(round_n(0.0001),"1.0e-04")
  expect_equal(round_n(0.001),"0.001")
  expect_equal(round_n(0.01),"0.010")
  expect_equal(round_n(0.1),"0.100")
  expect_equal(round_n(1),"1.000")
  expect_equal(round_n(10),"10.000")
  expect_equal(round_n(100),"100.000")
  expect_equal(round_n(1000),"1.0e+03")
  expect_equal(round_n(10000),"1.0e+04")
  expect_equal(round_n(c(.0001,.0003,.001,.003,.01,.03,.1,.3,1,3,10,30,100,300,1000,3000,10000)),
               c('1.0e-04', '3.0e-04', '0.001', '0.003', '0.010', '0.030', '0.100', '0.300', '1.000', 
                 '3.000', '10.000', '30.000', '100.000', '300.000', '1.0e+03', '3.0e+03', '1.0e+04'))
})


#-------------------------------------------------------------------------------
# Test get_plot_title
#-------------------------------------------------------------------------------
test_that("get_plot_title creates full title for single plot mc", {
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
  title <- get_plot_title(dat = dat, verbose = FALSE)
  expected_title <- trimws(paste0(dat$dsstox_substance_id, " ", dat$chnm, "\nSPID:", dat$spid, 
                           "  AEID:", dat$aeid, "  AENM:", dat$aenm, "\nM4ID:", dat$m4id, 
                           "\nHITC:", format(round(dat$hitc, 3), nsmall = 3)))
  expect_equal(title, expected_title)
  title <- get_plot_title(dat = dat, verbose = TRUE)
  expected_title <- trimws(paste0(dat$dsstox_substance_id, " ", dat$chnm, "\nSPID:", dat$spid, 
                           "  AEID:", dat$aeid, "  AENM:", dat$aenm, "\nM4ID:", dat$m4id))
  expect_equal(title, expected_title)
})

test_that("get_plot_title creates full title for aeid compare plot mc", {
  data("mc_test")
  mocked <- mc_test$plot_multiple_m4id
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  dat <- tcplPlotLoadData(val = mocked$m4id, flags = TRUE)
  title <- get_plot_title(dat = dat, compare = "aeid", verbose = FALSE)
  expected_title <- trimws(paste0("AEID:", unique(dat$aeid), "  AENM:", unique(dat$aenm)))
  expect_equal(title, expected_title)
  title <- get_plot_title(dat = dat, compare = "aeid", verbose = TRUE)
  expect_equal(title, expected_title)
})

test_that("get_plot_title creates full title for chem compare plot mc", {
  data("mc_test")
  mocked <- mc_test$plot_multiple_aeid
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  dat <- tcplPlotLoadData(fld = "aeid", val = mocked$aeid, flags = TRUE)
  dat <- dat[!is.na(chid)]
  dat <- dat[chnm == dat$chnm[1]]
  title <- get_plot_title(dat = dat, compare = "chnm", verbose = FALSE)
  expected_title <- trimws(paste(unique(dat$dsstox_substance_id), unique(dat$chnm)))
  if (lu(dat$spid) == 1) {
    expected_title <- paste0(expected_title, "\nSPID:", unique(dat$spid))
  }
  expect_equal(title, expected_title)
  title <- get_plot_title(dat = dat, compare = "chnm", verbose = TRUE)
  expect_equal(title, expected_title)
})

test_that("get_plot_title creates full title for other compare plot mc", {
  data("mc_test")
  mocked <- mc_test$plot_multiple_aeid
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  dat <- tcplPlotLoadData(fld = "aeid", val = mocked$aeid, flags = TRUE)
  title <- get_plot_title(dat = dat, compare = "normalized_data_type", verbose = FALSE)
  expected_title <- trimws(paste0("normalized_data_type: ", unique(dat$normalized_data_type)))
  expect_equal(title, expected_title)
  title <- get_plot_title(dat = dat, compare = "normalized_data_type", verbose = TRUE)
  expect_equal(title, expected_title)
  # length of 2 compare
  title <- get_plot_title(dat = dat, compare = c("normalized_data_type", "conc_unit"), verbose = FALSE)
  expected_title <- trimws(paste0("normalized_data_type: ", unique(dat$normalized_data_type),
                                  "; conc_unit: ", unique(dat$conc_unit)))
  expect_equal(title, expected_title)
})

test_that("get_plot_title creates full title for single plot sc", {
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
  title <- get_plot_title(dat = dat, type = "sc", verbose = FALSE)
  expected_title <- trimws(paste0(dat$dsstox_substance_id, " ", dat$chnm, "\nSPID:", dat$spid, 
                                  "  AEID:", dat$aeid, "  AENM:", dat$aenm, "\nS2ID:", dat$s2id, 
                                  "\nHITC:", format(round(dat$hitc, 3), nsmall = 3)))
  expect_equal(title, expected_title)
  title <- get_plot_title(dat = dat, type = "sc", verbose = TRUE)
  expected_title <- trimws(paste0(dat$dsstox_substance_id, " ", dat$chnm, "\nSPID:", dat$spid, 
                                  "  AEID:", dat$aeid, "  AENM:", dat$aenm, "\nS2ID:", dat$s2id))
  expect_equal(title, expected_title)
})

test_that("get_plot_title creates full title for aeid compare plot sc", {
  data("sc_test")
  mocked <- sc_test$plot_multiple_s2id
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") sc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  dat <- tcplPlotLoadData(type = "sc", fld = "s2id", val = mocked$s2id)
  title <- get_plot_title(dat = dat, type = "sc", compare = "aeid", verbose = FALSE)
  expected_title <- trimws(paste0("AEID:", unique(dat$aeid), "  AENM:", unique(dat$aenm)))
  expect_equal(title, expected_title)
  title <- get_plot_title(dat = dat, type = "sc", compare = "aeid", verbose = TRUE)
  expect_equal(title, expected_title)
})

test_that("get_plot_title creates full title for chem compare plot sc", {
  data("sc_test")
  mocked <- sc_test$plot_multiple_aeid
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") sc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  dat <- tcplPlotLoadData(type = "sc", fld = "aeid", val = mocked$aeid)
  dat <- dat[!is.na(chid)]
  dat <- dat[chnm == dat$chnm[1]]
  title <- get_plot_title(dat = dat, type = "sc", compare = "chnm", verbose = FALSE)
  expected_title <- trimws(paste(unique(dat$dsstox_substance_id), unique(dat$chnm)))
  if (lu(dat$spid) == 1) {
    expected_title <- paste0(expected_title, "\nSPID:", unique(dat$spid))
  }
  expect_equal(title, expected_title)
  title <- get_plot_title(dat = dat, type = "sc", compare = "chnm", verbose = TRUE)
  expect_equal(title, expected_title)
})

test_that("get_plot_title creates full title for other compare plot sc", {
  data("sc_test")
  mocked <- sc_test$plot_multiple_aeid
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") sc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  dat <- tcplPlotLoadData(type = "sc", fld = "aeid", val = mocked$aeid)
  title <- get_plot_title(dat = dat, type = "sc", compare = "normalized_data_type", verbose = FALSE)
  expected_title <- trimws(paste0("normalized_data_type: ", unique(dat$normalized_data_type)))
  expect_equal(title, expected_title)
  title <- get_plot_title(dat = dat, type = "sc", compare = "normalized_data_type", verbose = TRUE)
  expect_equal(title, expected_title)
  # length of 2 compare
  title <- get_plot_title(dat = dat, compare = c("normalized_data_type", "conc_unit"), verbose = FALSE)
  expected_title <- trimws(paste0("normalized_data_type: ", unique(dat$normalized_data_type),
                                  "; conc_unit: ", unique(dat$conc_unit)))
  expect_equal(title, expected_title)
})


#-------------------------------------------------------------------------------
# Test get_plot_caption
#-------------------------------------------------------------------------------
test_that("get_plot_caption returns list of flags for a single plot", {
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
  caption <- get_plot_caption(dat)
  expected_caption <- paste0("\nFlags(", dat$flag_count, "): ", dat$flag)
  expect_equal(caption, expected_caption)
})

test_that("get_plot_caption returns list of flags for a compare plot", {
  data("mc_test")
  mocked <- mc_test$plot_multiple_aeid
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  dat <- tcplPlotLoadData(fld = "aeid", val = mocked$aeid, flags = TRUE)
  dat$index <- MORELETTERS(1:nrow(dat))
  caption <- get_plot_caption(dat)
  expect_true(stringr::str_detect(caption, "^Flags:\n.*\\|.*"))
  expect_true(all(stringr::str_detect(caption, as.character(dat$flag_count))))
  expect_true(all(stringr::str_detect(caption, dat$flag)))
})


#-------------------------------------------------------------------------------
# Test dynamic_table_trunc
#-------------------------------------------------------------------------------
test_that("dynamic_table_trunc works", {
  all_cols <- c("index","m4id","dsstox_substance_id","chnm","spid","aeid","aenm","color")
  tbl <- data.table(index = LETTERS[1:4], 
                    m4id = c(8353147, 8353148, 8353576, 8353577),
                    dsstox_substance_id = c('DTXSID8031077', 'DTXSID00872663', 'DTXSID8031077', 'DTXSID00872663'),
                    chnm = c("A Chemical Name", "A Longer Chemical Name", "A Much Longer Chemical Name That is Too Long", 
                             "An Even Longer Chemical Name Which Will Not Survive Truncation No Matter What"),
                    spid = c("A Sample Id", "A Longer Sample Id", "A Slightly Long Sample Id", 
                             "A Sample Id So Long It Can't Survive Truncation"),
                    aeid = c(2649, 2649, 2652, 2652),
                    aenm = c("An Assay Endpoint Name", "A Longer Assay Endpoint Name", "A Significantly Longer Assay Endpoint Name", 
                             "An Assay Endpoint Name So Long It Exceeds The 40 Character Limit But Fits When Chem/Spid Are Removed"),
                    color = c('#455BCDFF', '#39A4FBFF', '#19E3BAFF', '#71FE5FFF'))
  truncated <- dynamic_table_trunc(copy(tbl), all_cols)
  expect_true(is.data.table(truncated))
  expect_equal(str_detect(truncated$chnm,"[...]"), c(F,F,T,T))
  expect_equal(str_detect(truncated$spid,"[...]"), c(F,F,T,T))
  expect_equal(str_detect(truncated$aenm,"[...]"), c(F,F,T,T))
  truncated <- dynamic_table_trunc(tbl[,-c("aeid","aenm")], all_cols) # removed when all are the same endpoint
  expect_true(is.data.table(truncated))
  expect_equal(str_detect(truncated$chnm,"[...]"), c(F,F,F,T))
  expect_equal(str_detect(truncated$spid,"[...]"), c(F,F,F,T))
  truncated <- dynamic_table_trunc(tbl[,-c("dsstox_substance_id","chnm")], all_cols) # removed when all are the same chemical
  expect_true(is.data.table(truncated))
  expect_equal(str_detect(truncated$spid,"[...]"), c(F,F,F,T))
  expect_equal(str_detect(truncated$aenm,"[...]"), c(F,F,F,T))
  truncated <- dynamic_table_trunc(tbl[,-c("dsstox_substance_id","chnm","spid")], all_cols) # removed when all are the same chemical/spid
  expect_true(is.data.table(truncated))
  expect_equal(str_detect(truncated$aenm,"[...]"), c(F,F,F,F))
  colnames(tbl)[2] <- all_cols[2] <- "s2id" # sc test
  truncated <- dynamic_table_trunc(copy(tbl), all_cols)
  expect_true(is.data.table(truncated))
  expect_equal(str_detect(truncated$chnm,"[...]"), c(F,F,T,T))
  expect_equal(str_detect(truncated$spid,"[...]"), c(F,F,T,T))
  expect_equal(str_detect(truncated$aenm,"[...]"), c(F,F,T,T))
})


#-------------------------------------------------------------------------------
# Test get_verbose_tables
#-------------------------------------------------------------------------------
test_that("get_verbose_tables returns two tables small compare mc", {
  data("mc_test")
  mocked <- mc_test$plot_multiple_aeid
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") mc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  dat <- tcplPlotLoadData(fld = "aeid", val = mocked$aeid, flags = TRUE)
  dat$index <- MORELETTERS(1:nrow(dat))
  dat$color <- viridis::viridis(nrow(dat), begin = 0.1, end = 0.9, option = "turbo")
  expect_no_error(tbls <- get_verbose_tables(dat = dat, compare = "normalized_data_type", flags = TRUE))
  expect_length(tbls, 2)
  expect_true(gtable::is.gtable(tbls[[1]]))
  expect_true(gtable::is.gtable(tbls[[2]]))
  expect_no_error(tbls <- get_verbose_tables(dat = dat, compare = "normalized_data_type", flags = FALSE))
  expect_length(tbls, 2)
  expect_true(gtable::is.gtable(tbls[[1]]))
  expect_true(gtable::is.gtable(tbls[[2]]))
})

test_that("get_verbose_tables returns two tables large compare mc", {
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
  dat$index <- MORELETTERS(1:nrow(dat))
  dat$color <- viridis::viridis(nrow(dat), begin = 0.1, end = 0.9, option = "turbo")
  expect_no_error(tbls <- get_verbose_tables(dat = dat, compare = "normalized_data_type", flags = TRUE))
  expect_length(tbls, 2)
  expect_true(gtable::is.gtable(tbls[[1]]))
  expect_true(gtable::is.gtable(tbls[[2]]))
  expect_no_error(tbls <- get_verbose_tables(dat = dat, compare = "normalized_data_type", flags = FALSE))
  expect_length(tbls, 2)
  expect_true(gtable::is.gtable(tbls[[1]]))
  expect_true(gtable::is.gtable(tbls[[2]]))
})

test_that("get_verbose_tables returns two tables small compare sc", {
  data("sc_test")
  mocked <- sc_test$plot_multiple_aeid
  local_mocked_bindings(
    tcplQuery = function(query, db, tbl) {
      if (query == "SHOW VARIABLES LIKE 'max_allowed_packet'") sc_test$tcplConfQuery
      else mocked[query][[1]]
    }
  )
  tcplConf(drvr = "MySQL", db = "invitrodb") # must include both
  dat <- tcplPlotLoadData(type = "sc", fld = "aeid", val = mocked$aeid)
  dat$index <- MORELETTERS(1:nrow(dat))
  dat$color <- viridis::viridis(nrow(dat), begin = 0.1, end = 0.9, option = "turbo")
  expect_no_error(tbls <- get_verbose_tables(dat = dat, type = "sc", compare = "normalized_data_type"))
  expect_length(tbls, 2)
  expect_true(gtable::is.gtable(tbls[[1]]))
  expect_true(gtable::is.gtable(tbls[[2]]))
})

test_that("get_verbose_tables returns two tables large compare sc", {
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
  dat$index <- MORELETTERS(1:nrow(dat))
  dat$color <- viridis::viridis(nrow(dat), begin = 0.1, end = 0.9, option = "turbo")
  expect_no_error(tbls <- get_verbose_tables(dat = dat, type = "sc", compare = "normalized_data_type"))
  expect_length(tbls, 2)
  expect_true(gtable::is.gtable(tbls[[1]]))
  expect_true(gtable::is.gtable(tbls[[2]]))
})