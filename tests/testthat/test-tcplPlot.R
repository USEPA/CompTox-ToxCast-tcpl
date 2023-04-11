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

test_that("tcplPlot loads data", {
  input <- tcplLoadData(lvl = 4, type = "mc")
  expect_no_error(
    if (nrow(input) == 0) stop("No data for fld/val provided")
  )
  input <- NULL  
  expect_error(
    if (nrow(input) == 0) stop("No data for fld/val provided")
  ) 
})

test_that("logic check works: assign multi=TRUE for output='pdf'", {
  output <- "pdf"
  multi  <-  NULL
  expect_true(
  if (output == "pdf" && is.null(multi)) {
    multi <- TRUE
  }, multi)
})

test_that("logic check works: assign multi=FALSE for output != 'pdf', 
                              verbose=FALSE for output='console'", {
  output = 'console'
  expect_false(
    if (output !="pdf") {
      multi <- FALSE
      if(output =="console"){
        verbose <- FALSE
        }
      }, c(multi,verbose))
    })

# test_that("description", {
#   expect_*(code)
# })
