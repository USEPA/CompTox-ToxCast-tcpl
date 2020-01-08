#' Function that checks if the most recent v3 table schema is used in the database schema
#'
#' @return boolean TRUE if param tables are listed in schema FALSE otherwise
#' 
#'
#' @examples
check_tcpl_db_schema <- function(){
  # get list of fields in the table
  tbls <- unlist(tcplQuery("SHOW TABLES;"))
  # check if _params are in the list of tables
  all(c("mc4_param","mc5_param") %in% tbls)
}