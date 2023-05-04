tcplConf(drvr="example")
if (!interactive()) pdf(NULL)

test_that("round_n function works", {
  expect_equal(
    round_n(c(1234.5678, NA_real_, 0.3333),3),
    c("1.2e+03",NA,"0.333"))
  })

test_that("check_tcpl_db_schema() loaded", {
  expect_false(
    (check_tcpl_db_schema())
  )
})

test_that("error message works: 'No data for fld/val provided'", {
  input <- NULL  
  expect_error(
    if (nrow(input) == 0) stop("No data for fld/val provided")
  ) 
})

test_that("tcplPlot loads data", {
  input <- tcplLoadData(lvl = 4, type = "mc")
  expect_no_error(
    if (nrow(input) == 0) stop("No data for fld/val provided")
  )
})

test_that("logic check works: assign multi=TRUE for output='pdf'", {
  output <- "pdf"
  multi  <-  NULL
  if (output == "pdf" && is.null(multi)) {
    multi <- TRUE
  }
  expect_true(multi)
})

test_that("logic check works: assign multi=FALSE for output != 'pdf', 
                              verbose=FALSE for output='console'", {
  output = 'console'
  if (output !="pdf") {
    multi <- FALSE
    if(output =="console"){
      verbose <- FALSE
    }
  }
  expect_false(multi)
  expect_false(verbose)
  })

test_that("logic check works: assign nrow = ncol = 1 for output='pdf' and
                              multi=FALSE to plot one plot per page", {
  output = 'pdf'
  multi = FALSE
  input <- tcplLoadData(lvl = 4, type = "mc")
  if(nrow(input) > 1 && output == "pdf" && multi == FALSE) {
    nrow = ncol = 1
  } 
  expect_equal(
    c(nrow,ncol),
    c(1,1)
  )
})

test_that("error message: output = 'console' and multi = FALSE 
                          to avoid multiple plots in console", {
  input <- tcplLoadData(lvl = 4, type = "mc")
  expect_error(
    if(nrow(input) > 1 && output == "console" && multi == FALSE) stop("More than 1 concentration series returned for given field/val combination.  Set output to pdf or reduce the number of curves to 1. Current number of curves: ", nrow(input))
  )
})


test_that("nrow value set based on verbose", {
  nrow = NULL
  verbose = TRUE
  if(is.null(nrow)){
    nrow <- ifelse(verbose,2,2)
  }
  expect_equal(nrow,2)
})

test_that("ncol value set based on verbose", {
  ncol = NULL
  verbose = FALSE
  if(is.null(ncol)){
    ncol <- ifelse(verbose,2,3)
  }
  expect_equal(ncol,3)
})

test_that("m4id filters mc4 input", {
  input <- tcplLoadData(lvl = 4, type = "mc")
  m4id <- input$m4id
  expect_length(m4id,5)
})

test_that("m4id filters mc5 input", {
  input <- tcplLoadData(lvl = 5, type = "mc")
  m4id <- input$m4id
  expect_length(m4id,5)
})

test_that("dat table loads", {
  lvl = 5
  l4 <- tcplLoadData(lvl = 4, type = "mc", add.fld = TRUE)
  if (lvl >= 5L) {
    l5 <- tcplLoadData(lvl = 5, type = "mc", add.fld = TRUE)
    dat <- l4[l5, on = "m4id"]
  }
  expect_length(dat,length(l5)+length(l4)-1)
})

test_that("tcplPrepOtpt loads chemical and units", {
  lvl = 5
  l4 <- tcplLoadData(lvl = 4, type = "mc", add.fld = TRUE)
  if (lvl >= 5L) {
    l5 <- mc_vignette[["mc5"]]
    dat <- l4[l5, on = c("m4id","aeid")]
    dat <- dat[,!c("tp","ga","q","la","ac50_loss")]
  }
  expect_length(dat,188)
  expect_type(dat$dsstox_substance_id,"character")
  expect_type(dat$conc_unit,"character")
})

test_that("unlog conc data table works", {
  agg <- tcplLoadData(lvl = "agg", type = "mc")
  conc_resp_table <- agg %>% 
    group_by(m4id) %>% 
    summarise(conc = list(10^logc), resp = list(resp)) %>% 
    as.data.table()
  expect_true(is.data.table(conc_resp_table))
  expect_length(conc_resp_table,3)
  expect_length(conc_resp_table$conc,5)
})

test_that("conc_resp_table joins dat table correctly", {
  lvl = 5
  l4 <- tcplLoadData(lvl = 4, type = "mc", add.fld = TRUE)
  if (lvl >= 5L) {
    l5 <- tcplLoadData(lvl = 5, type = "mc", add.fld = TRUE)
    dat <- l4[l5, on = "m4id"]
  }
  agg <- tcplLoadData(lvl = "agg", type = "mc")
  conc_resp_table <- agg %>% 
    group_by(m4id) %>% 
    summarise(conc = list(10^logc), resp = list(resp)) %>% 
    as.data.table()
  dat <- dat[conc_resp_table, on = "m4id"]
  expect_length(dat,183)
})

test_that("one m4id tcplPlot works", {
  skip_on_ci()
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
    summarise(conc = list(10^logc), resp = list(resp)) %>% 
    as.data.table()
  dat <- dat[conc_resp_table, on = "m4id"]
  dat <- dat[,normalized_data_type:="log2_fold_induction"]
  dat <- dat[spid == "01504209"]
  mc5_tcplplot <- tcplggplot(dat,verbose = verbose)
  expect_no_warning(mc5_tcplplot)
  vdiffr::expect_doppelganger("test_output_482273", mc5_tcplplot)
})

test_that("negative direction plot has negative bmr and cutoff lines", {
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
    summarise(conc = list(10^logc), resp = list(resp)) %>% 
    as.data.table()
  dat <- dat[conc_resp_table, on = "m4id"]
  dat <- dat[,normalized_data_type:="log2_fold_induction"]
  dat <- dat[spid == "1210314466"]  
  mc5_tcplplot <- tcplggplot(dat,verbose = verbose)
  vdiffr::expect_doppelganger("negative_cutoff_bmr", mc5_tcplplot)
})

test_that("coff,bmr should be negative if winning model has negative top", {
  lvl = 5
  l4 <- tcplLoadData(lvl = 4, type = "mc", add.fld = TRUE)
  if (lvl >= 5L) {
    l5 <- tcplLoadData(lvl = 5, type = "mc", add.fld = TRUE)
    dat <- l4[l5, on = "m4id"]
  }
  dat <- dat[spid == "1210314466"]
  if (!is.null(dat$top) && !is.null(dat$coff) && !is.na(dat$top) && !is.null(dat$bmr)) {
    if (dat$top < 0) {
      dat$coff <- dat$coff * -1
      dat$bmr <- dat$bmr * -1
    }
  }
  expect_lt(dat$coff,0)
  expect_lt(dat$bmr,0)
})

# test_that("description", {
#   expect_*(code)
# })
