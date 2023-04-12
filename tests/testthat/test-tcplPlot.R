tcplConf(drvr="example")
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


# test_that("description", {
#   expect_*(code)
# })
