tcplConf(drvr = "example")

test_that("sc1 loads", {
  tcplLoadData(lvl = 1, type = "sc")
  expect_true(is.data.table(sc1))
})

tcplLoadData(lvl = 2, type = "sc")
tcplLoadData(lvl = 0, type = "mc")
tcplLoadData(lvl = 1, type = "mc")
tcplLoadData(lvl = 2, type = "mc")
tcplLoadData(lvl = 3, type = "mc")
tcplLoadData(lvl = 4, type = "mc", add.fld = FALSE)
tcplLoadData(lvl = 4, type = "mc", add.fld = TRUE)
tcplLoadData(lvl = 5, type = "mc", add.fld = FALSE)
tcplLoadData(lvl = 5, type = "mc", add.fld = TRUE)
tcplLoadData(lvl = 6, type = "mc")
tcplLoadData(lvl=5, type = "sc")
  
test_that("invalid input error message", {
  expect_error(tcplLoadData(lvl = "sc", type = 0),
               "Invalid 'lvl' and 'type' combination.")
  })
  
test_that("sc example error message appears", {
  expect_error(tcplLoadData(lvl = 6, type = "sc"),
               "example tables for sc0, sc1, sc2 available.")
    
  })
  
test_that("mc example error message appears", {
  expect_error(tcplLoadData(lvl = 6, type = "sc"),
               "example tables for sc0, sc1, sc2 available.")
  })
  
# test_that("description", {
#   expect_*(code)
# })