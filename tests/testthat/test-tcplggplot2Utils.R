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