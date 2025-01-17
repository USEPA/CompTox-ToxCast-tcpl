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
# Test tcplPlotValidate
#-------------------------------------------------------------------------------
test_that("tcplPlotValidate warns user if flags = TRUE and type = 'sc'", {
  expect_warning(validated_params <- tcplPlotValidate(dat = NULL,type = "sc",compare = "m4id",by = NULL,flags = TRUE,output = "pdf",multi = NULL,verbose = FALSE),
                 "'flags' was set to TRUE - no flags exist for plotting single concentration")
  expect_equal(validated_params, list(dat = NULL, lvl = 2, type = "sc", compare = "s2id", by = NULL, flags = FALSE, output = "pdf", multi = TRUE, verbose = FALSE))
})

test_that("tcplPlotValidate only updates compare to 's2id' when compare = 'm4id' and type = 'sc'", {
  validated_params <- tcplPlotValidate(dat = NULL,type = "sc",compare = "m4id",by = NULL,flags = FALSE,output = "pdf",multi = NULL,verbose = FALSE)
  expect_equal(validated_params, list(dat = NULL, lvl = 2, type = "sc", compare = "s2id", by = NULL, flags = FALSE, output = "pdf", multi = TRUE, verbose = FALSE))
  validated_params <- tcplPlotValidate(dat = NULL,type = "sc",compare = "s2id",by = NULL,flags = FALSE,output = "pdf",multi = NULL,verbose = FALSE)
  expect_equal(validated_params, list(dat = NULL, lvl = 2, type = "sc", compare = "s2id", by = NULL, flags = FALSE, output = "pdf", multi = TRUE, verbose = FALSE))
  validated_params <- tcplPlotValidate(dat = NULL,type = "sc",compare = "chnm",by = NULL,flags = FALSE,output = "pdf",multi = NULL,verbose = FALSE)
  expect_equal(validated_params, list(dat = NULL, lvl = 2, type = "sc", compare = "chnm", by = NULL, flags = FALSE, output = "pdf", multi = TRUE, verbose = FALSE))
})

test_that("tcplPlotValidate force assigns multi = FALSE and verbose = FALSE if output == 'console'", {
  validated_params <- tcplPlotValidate(dat = NULL,type = "mc",compare = "m4id",by = NULL,flags = TRUE,output = "console",multi = TRUE,verbose = TRUE)
  expect_equal(validated_params, list(dat = NULL,lvl = 5,type = "mc",compare = "m4id",by = NULL, flags = TRUE, output = "console", multi = FALSE, verbose = FALSE))
})

test_that("tcplPlotValidate doesn't change correctly assigned parameters", {
  validated_params <- tcplPlotValidate(dat = NULL,type = "mc",compare = "m4id",by = NULL,flags = TRUE,output = "pdf",multi = TRUE,verbose = TRUE)
  expect_equal(validated_params, list(dat = NULL,lvl = 5,type = "mc",compare = "m4id",by = NULL, flags = TRUE, output = "pdf", multi = TRUE, verbose = TRUE))
})

test_that("tcplPlotValidate properly validates 'by'", {
  expect_error(tcplPlotValidate(by = c("aeid", "spid")), "'by' must be of length 1.")
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
  dat <- split(dat, by ="spid")
  expect_warning(validated_params <- tcplPlotValidate(dat = dat, by = "model_type", output = "pdf"), 
                 "Using 'by' can have unintended consequences when 'dat' is provided as a list of data.tables. Instead, consider adding a custom field to group comparison plots, and specify using the 'compare' parameter. Then, use 'by' to split plots into files.")
  expect_equal(validated_params, list(dat = dat, lvl = 5, type = "mc", compare = "m4id", 
                                      by = "model_type", flags = NULL, output = "pdf", 
                                      multi = TRUE, verbose = FALSE))
})

test_that("tcplPlotValidate properly validates 'dat' as a list or a data.table", {
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
  expect_error(tcplPlotValidate(dat = split(dat, by = c("aeid", "chnm"), flatten = FALSE)), 
               "'dat' must be a data.table or a list of data.tables.")
  expect_error(tcplPlotValidate(dat = c(0,1,2,3)), 
               "'dat' must be a data.table or a list of data.tables.")
  expect_error(tcplPlotValidate(dat = NA), 
               "'dat' must be a data.table or a list of data.tables.")
  expect_error(tcplPlotValidate(dat = "dat"), 
               "'dat' must be a data.table or a list of data.tables.")
  dat <- split(dat, by = "aeid")
  expect_warning(validated_params <- tcplPlotValidate(dat = dat, compare = "model_type", output = "pdf"), 
                 "'dat' provided as list of list of data tables, meaning compare plots are already subdivided. 'compare' field will be ignored and have no effect.")
  expect_equal(validated_params, list(dat = dat, lvl = 5, type = "mc", compare = "model_type", 
                                      by = NULL, flags = NULL, output = "pdf", 
                                      multi = TRUE, verbose = FALSE))
  validated_params <- tcplPlotValidate(dat = dat, output = "pdf")
  expect_equal(validated_params, list(dat = dat, lvl = 5, type = "mc", compare = "m4id", 
                                      by = NULL, flags = NULL, output = "pdf", 
                                      multi = TRUE, verbose = FALSE))
})

test_that("tcplPlotValidate validates 'output'", {
  validated_params <- tcplPlotValidate()
  expect_equal(validated_params, list(dat = NULL, lvl = 5, type = "mc", compare = "m4id", 
                                      by = NULL, flags = NULL, output = "ggplot", 
                                      multi = FALSE, verbose = FALSE))
  expect_error(validated_params <- tcplPlotValidate(output = NULL), "'output' cannot be NULL")
})

test_that("tcplPlotValidate defaults to TRUE for 'multi' if output = 'pdf'", {
  validated_params <- tcplPlotValidate(output = "pdf")
  expect_equal(validated_params, list(dat = NULL, lvl = 5, type = "mc", compare = "m4id", 
                                      by = NULL, flags = NULL, output = "pdf", 
                                      multi = TRUE, verbose = FALSE))
  validated_params <- tcplPlotValidate(multi = FALSE, output = "pdf")
  expect_equal(validated_params, list(dat = NULL, lvl = 5, type = "mc", compare = "m4id", 
                                      by = NULL, flags = NULL, output = "pdf", 
                                      multi = FALSE, verbose = FALSE))
})



#-------------------------------------------------------------------------------
# Test tcplPlotSetYRange
#-------------------------------------------------------------------------------
test_that("tcplPlotSetYRange validates range", {
  # error cases
  expect_error(tcplPlotSetYRange(dat=NULL,yuniform=FALSE,yrange=c(1,2,3),type="mc"), "'yrange' must be of length 2")
  expect_error(tcplPlotSetYRange(dat=NULL,yuniform=FALSE,yrange=1,type="mc"), "'yrange' must be of length 2")
  expect_error(tcplPlotSetYRange(dat=NULL,yuniform=FALSE,yrange=NULL,type="mc"), "'yrange' must be of length 2")
  # validate existing set yrange and doesn't overwrite it regardless if yuniform is TRUE
  expect_equal(tcplPlotSetYRange(dat=NULL,yuniform=FALSE,yrange=c(-100,100),type="mc"),c(-100,100))
  expect_equal(tcplPlotSetYRange(dat=NULL,yuniform=TRUE,yrange=c(-100,100),type="mc"),c(-100,100))
})

test_that("tcplPlotSetYRange correctly sets yrange for mc data", {
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
  expect_no_error(yrange <- tcplPlotSetYRange(dat=dat,yuniform=TRUE,yrange=c(NA,NA),type="mc"))
  expect_length(yrange,2)
  min <- min(dat$resp_min, unlist(dat$resp))
  max <- max(dat$resp_max, unlist(dat$resp))
  if (2 %in% dat$model_type) {
    cutoffs <- dat[model_type == 2]$coff
    min <- min(min, cutoffs, cutoffs * -1)
    max <- max(max, cutoffs, cutoffs * -1)
  }
  # any gain models contained in dat, cutoff only positive
  if (3 %in% dat$model_type) {
    cutoffs <- dat[model_type == 3]$coff
    min <- min(min, cutoffs)
    max <- max(max, cutoffs)
  }
  # any loss models contained in dat, cutoff only negative
  if (4 %in% dat$model_type) {
    cutoffs <- dat[model_type == 4]$coff
    min <- min(min, cutoffs * -1)
    max <- max(max, cutoffs * -1)
  }
  expect_equal(yrange, c(min,max))
})

test_that("tcplPlotSetYRange correctly sets yrange for sc data", {
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
  expect_no_error(yrange <- tcplPlotSetYRange(dat=dat,yuniform=TRUE,yrange=c(NA,NA),type="sc"))
  expect_length(yrange,2)
  min <- min(dat$resp_min, unlist(dat$resp))
  max <- max(dat$resp_max, unlist(dat$resp))
  min <- min(min, dat$coff, dat$coff * -1)
  max <- max(max, dat$coff, dat$coff * -1)
  expect_equal(yrange, c(min,max))
})