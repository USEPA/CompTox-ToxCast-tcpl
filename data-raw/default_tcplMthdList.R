library(tcpl)
tcplConf(user=user, pass=pass, db="invitrodb", drvr="MySQL", host=host)

mc2 <- tcplMthdList(2)
mc3 <- tcplMthdList(3)
mc4 <- tcplMthdList(4)
mc5 <- tcplMthdList(5)
mc6 <- tcplMthdList(6)
sc1 <- tcplMthdList(1, type = "sc")
sc2 <- tcplMthdList(2, type = "sc")

mthd_list_defaults <- list(mc2 = mc2,
                          mc3 = mc3,
                          mc4 = mc4,
                          mc5 = mc5,
                          mc6= mc6,
                          sc1 = sc1,
                          sc2 = sc2
                          )

usethis::use_data(mthd_list_defaults, overwrite = TRUE)
