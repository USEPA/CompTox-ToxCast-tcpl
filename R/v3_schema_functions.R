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
  
    
}


library(dplyr)
library(tidyr)
test <- param %>%
  unnest_longer(fitparams) %>%
  filter(fitparams_id != "modelnames") %>%
  rename(model = fitparams_id) %>%
  unnest_longer(fitparams) %>% 
  select(m4id,model,model_param = fitparams_id,model_val = fitparams) %>% 
  filter(!model_param %in% c("pars","sds")) %>% 
  unnest_longer(model_val)
# test1 <- test %>% group_by(m4id,model,model_param) %>% chop(model_val)
# test2 <- test1 %>% filter(!length(model_val[[1]])>1)
# rej <- test1 %>% filter(length(model_val[[1]])>1)
# rej1 <- rej %>% group_by(m4id,model) %>% summarise(l = list(as.list(setNames(model_val,unlist(model_param)))))
# test3 <- test2 %>% group_by(m4id) %>% chop(c(model_param,model_val)) %>% group_by(m4id,model) %>% summarise(l = list(as.list(setNames(unlist(model_val),unlist(model_param)))))
# test4 <- bind_rows(rej1,test3) %>% group_by(m4id,model) %>% summarise(l = ifelse(n()>1,list(append(last(l),first(l))),l))
# test5 <- test4 %>% summarise(output = list(as.list(setNames(l,model))))
# test6 <- test5 %>% mutate(output = list(append(output,list(modelnames = names(output)))) )

re_list <-
  bind_rows(
    test %>% group_by(m4id, model, model_param) %>% tidyr::chop(model_val) %>% filter(!length(model_val[[1]]) > 1) %>% group_by(m4id) %>% chop(c(model_param, model_val)) %>% group_by(m4id, model) %>% summarise(l = list(as.list(setNames(unlist(model_val), unlist(model_param))))),
    test %>% group_by(m4id, model, model_param) %>% tidyr::chop(model_val) %>% filter(length(model_val[[1]]) > 1) %>% group_by(m4id, model) %>% summarise(l = list(as.list(setNames(model_val, unlist(model_param)))))
  ) %>%
  group_by(m4id, model) %>%
  summarise(l = ifelse(n() > 1, list(append(last(l), first(l))), l)) %>%
  summarise(output = list(as.list(setNames(l, model)))) %>%
  rowwise() %>%
  mutate(output = list(append(output, list(modelnames = names(output)))))
