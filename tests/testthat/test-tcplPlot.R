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
    summarise(conc = list(conc), resp = list(resp)) %>% 
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
    summarise(conc = list(conc), resp = list(resp)) %>% 
    as.data.table()
  dat <- dat[conc_resp_table, on = "m4id"]
  dat <- dat[,normalized_data_type:="log2_fold_induction"]
  dat <- dat[spid == "1210314466"]  
  mc5_tcplplot <- tcplggplot(dat,verbose = verbose)
  vdiffr::expect_doppelganger("negative_cutoff_bmr", mc5_tcplplot)
})


#-------------------------------------------------------------------------------
# new tests using drvr = "MySQL" and mocking
#-------------------------------------------------------------------------------