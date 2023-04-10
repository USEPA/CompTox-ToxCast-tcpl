tcplConf(drvr="example")
test_that("round_n function works", {
  expect_equal(
    round_n(c(1234.5678, NA_real_, 0.3333),3),
    c("1.2e+03",NA,"0.333"))
  })


# test_that("description", {
#   expect_*(code)
# })
