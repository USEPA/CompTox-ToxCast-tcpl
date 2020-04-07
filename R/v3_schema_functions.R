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
  param <- dat[,c("m4id","aeid","fitparams")]
  #get one standard deviation to save in similar way to fit params
  onesd <- dat[,c("m4id","aeid","osd")]
  setnames(onesd, "osd", "model_val")
  onesd[,"model" := "all"]
  onesd[,"model_param" := "onesd"]

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
  tcpl:::tcplAppend(
    dat = dat_agg[, .SD, .SDcols = mc4_agg_cols],
    tbl = "mc4_agg",
    db = getOption("TCPL_DB")
  )
  
  # append param dat
  tcpl:::tcplAppend(
    dat = dat_param,
    tbl = "mc4_param",
    db = getOption("TCPL_DB")
  )
  
  
  tcpl:::tcplAppend(
    dat = onesd,
    tbl = "mc4_param",
    db = getOption("TCPL_DB")
  )
  
    
}

