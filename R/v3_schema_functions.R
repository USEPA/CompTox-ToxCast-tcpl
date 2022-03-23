#' Write level 4 with updated schema
#'
#' @param dat output of tcplfit2 that has been unnested into a data.table
#'
#'
#' @examples
write_lvl_4 <- function(dat){
  #variable binding
  lvl <- aeid <- m4id <- m3ids <- NULL
  
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
  param <- dat[,c("m4id","aeid","fitparams")]
  #get one standard deviation to save in similar way to fit params
  onesd <- dat[,c("m4id","aeid","osd")]
  setnames(onesd, "osd", "model_val")
  onesd[,"model" := "all"]
  onesd[,"model_param" := "onesd"]

  # get bmed to save in similar way to fit params
  if ("bmed" %in% colnames(dat)) {
    bmed <- dat[, c("m4id", "aeid", "bmed")]
    setnames(bmed, "bmed", "model_val")
    bmed[, "model" := "all"]
    bmed[, "model_param" := "bmed"]
  } else {
    bmed <- 0
  }

  #unnest fit2 params
  unnested_param <- rbindlist(setNames(lapply(param$fitparams,tcplFit2_unnest),param$m4id),idcol = "m4id")
  unnested_param$m4id <- as.numeric(unnested_param$m4id)
  setkey(unnested_param,"m4id")
  setkey(param,"m4id")
  dat1 <- param[unnested_param]
  dat_param <- dat1[,c("m4id","aeid","model","model_param","model_val")]
  
  # get l3 dat for agg columns
  dat_agg <- dat[, .(aeid,m4id,m3id = m3ids)][, lapply(.SD,unlist),m4id]
  l3_dat <- tcplLoadData(lvl = 3L, type = "mc", fld = "m3id", val = dat_agg$m3id)[,c("m0id","m1id","m2id","m3id")]
  setkey(dat_agg,"m3id")
  setkey(l3_dat,"m3id")
  dat_agg <- dat_agg[l3_dat]
  
  
  # append agg columns
  tcplAppend(
    dat = dat_agg[, .SD, .SDcols = mc4_agg_cols],
    tbl = "mc4_agg",
    db = getOption("TCPL_DB")
  )
  
  # append param dat
  tcplAppend(
    dat = dat_param,
    tbl = "mc4_param",
    db = getOption("TCPL_DB")
  )
  
  
  tcplAppend(
    dat = onesd,
    tbl = "mc4_param",
    db = getOption("TCPL_DB")
  )
  
  tcplAppend(
    dat = bmed,
    tbl = "mc4_param",
    db = getOption("TCPL_DB")
  )
  
    
}

#' Function that checks if the most recent v3 table schema is used in the database schema
#'
#' @return boolean TRUE if param tables are listed in schema FALSE otherwise
#' 
#'
#' @examples
check_tcpl_db_schema <- function(){
  drvr <- getOption("TCPL_DRVR")
  if (drvr == "MySQL") {
    # get list of fields in the table
    tbls <- unlist(tcplQuery("SHOW TABLES;"))
    # check if _params are in the list of tables
    return(all(c("mc4_param","mc5_param") %in% tbls))
  }else{
    return(FALSE)
  }
  
}