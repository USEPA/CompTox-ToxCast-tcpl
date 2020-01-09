#' Title
#'
#' @param dat 
#'
#' @return
#'
#' @examples
write_lvl_4 <- function(dat){
  mc4_cols <- c("aeid",
                "spid",
                "bmad",
                "resp_max",
                "resp_min",
                "max_mean",
                "max_mean_conc",
                "max_med",
                "max_med_conc",
                "logc_max",
                "logc_min",
                "nconc",
                "npts",
                "nrep",
                "nmed_gtbl",
                "tmpi")
  mc4_agg_cols <- c(paste0("m", 0:4, "id"), "aeid")
  
  tcplAppend(
    dat = copy(dat[, unique(.SD), .SDcols = mc4_cols]),
    tbl = "mc4",
    db = getOption("TCPL_DB"), lvl = lvl
  )

  qformat <- "SELECT m4id, aeid, tmpi FROM mc4 WHERE aeid IN (%s);"
  ids <- dat[, unique(get("aeid"))]
  qstring <- sprintf(qformat, paste0("\"", ids, "\"", collapse = ","))
  
  m4id_map <- tcplQuery(query = qstring, db = getOption("TCPL_DB"), tbl = c("mc4"))
  setkeyv(m4id_map, c("aeid", "tmpi"))
  setkeyv(dat, c("aeid", "tmpi"))
  
  dat <- m4id_map[dat]
  param <- dat[,c("m4id","fitparams")]
  res <- param[, list(model_param = as.character(names(unlist(fitparams))), model_val = as.character(unlist(fitparams))), by = m4id]
  
  # append agg columns
  tcplAppend(
    dat = dat[, .SD, .SDcols = mc4_agg_cols],
    tbl = "mc4_agg",
    db = getOption("TCPL_DB")
  )
  
  # append param dat
  tcpl:::tcplAppend(
    dat = res,
    tbl = "mc4_param",
    db = getOption("TCPL_DB")
  )
  
    
}