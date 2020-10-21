#' tcpl Wrapper for tcplfit2_core including additional calculations to fit into new schema
#'
#' @param dat
#'
#' @return
#' @importFrom tcplfit2 tcplfit2_core
#' @examples
tcplFit2 <- function(dat,fitmodels = c("cnst", "hill", "gnls", "poly1", "poly2", "pow", "exp2", "exp3", "exp4",
                                       "exp5")) {
  # do all the regular fitting things that still need to be done
  res <- dat[, `:=`(c("rmns", "rmds", "nconcs", "med_rmds"), {
    rmns <- mean(resp)
    rmds <- median(resp)
    nconcs <- .N
    med_rmds <- rmds >= (3 * bmad)
    .(rmns, rmds, nconcs, med_rmds)
  }), keyby = .(aeid, spid, logc)][, .(
    bmad = min(bmad), resp_max = max(resp), osd = min(osd), bmed = max(bmed),
    resp_min = min(resp), max_mean = max(rmns), max_mean_conc = logc[which.max(rmns)],
    max_med = max(rmds), max_med_conc = logc[which.max(rmds)],
    logc_max = max(logc), logc_min = min(logc), nconc = length(unique(logc)),
    npts = .N, nrep = median(as.numeric(nconcs)), nmed_gtbl = sum(med_rmds) / first(nconcs),
    concentration_unlogged = list(10^(logc)), response = list(resp), m3ids = list(m3id)
  ),
  keyby = .(aeid, spid)
  ][, `:=`(tmpi = seq_len(.N)), keyby = .(aeid)][,
    `:=`(fitparams = list(tcplfit2::tcplfit2_core(unlist(concentration_unlogged),
      unlist(response),cutoff = bmad,
      verbose = FALSE, force.fit = FALSE,
      fitmodels = fitmodels
    ))),
    keyby = .(spid)
  ]

  # return dat with fitted curves in fitparams
  res
}

#' Title
#'
#' @param mc4 
#'
#' @return
#' @importFrom dplyr %>% filter group_by summarise left_join inner_join select
#' @importFrom tidyr pivot_longer
#' @importFrom tcplfit2 tcplhit2_core
#'
#' @examples
tcplHit2 <- function(mc4,coff){
  nested_mc4 <- mc4 %>% filter(model != "all") %>% group_by(m4id) %>% summarise(params = list(tcplFit2_nest(data.table(model = model,model_param = model_param,model_val = model_val))))
  
  #get lvl 3 conc/resp information
  l4_agg <- tcplLoadData(lvl = "agg", fld = "m4id", val = nested_mc4$m4id)
  l3_dat <- l4_agg %>% left_join(tcplLoadData(lvl = 3, fld = "m3id", val = l4_agg$m3id),by = c("aeid", "m3id", "m2id", "m1id", "m0id", "spid", "logc", "resp"))
  
  #unlog and plug into nested
  nested_mc4 <- nested_mc4 %>% left_join(l3_dat %>% group_by(m4id) %>% summarise(conc = list(10^(logc)), resp = list(resp)), by = "m4id")
  
  #rejoin the onesd for tcplfit2
  nested_mc4 <- nested_mc4 %>% inner_join(mc4 %>% filter(model_param == "onesd") %>% select(m4id,onesd = model_val))
  
  #rejoin for bmed
  nested_mc4 <- nested_mc4 %>% inner_join(mc4 %>% filter(model_param == "bmed") %>% select(m4id,bmed = model_val))
  
  #add the cutoff
  #nested_mc4$cutoff <- coff
  
  test <- nested_mc4 %>% rowwise %>% mutate(df = list(tcplhit2_core(params = params, conc = conc, resp = resp, bmed = bmed, cutoff = coff, onesd = onesd))) %>% select(-conc,-resp)
  
  res <- cbind(test %>% as.data.table(),test %>% as.data.table %>% pull(df) %>% rbindlist())
  
  # mc5 table
  mc5 <- res %>% left_join(mc4 %>% select(m4id,aeid) %>% unique,by = "m4id") %>% select(m4id,aeid,modl = fit_method,hitc = hitcall,coff = cutoff) %>% mutate(model_type = 3)
  
  # mc5 param table
  mc5_param <- res %>% left_join(mc4 %>% select(m4id,aeid) %>% unique,by = "m4id") %>% select(m4id,aeid,top_over_cutoff:bmd)
  mc5_param <- mc5_param %>% tidyr::pivot_longer(cols = top_over_cutoff:bmd, names_to = "hit_param", values_to = "hit_val") %>% filter(!is.na(hit_val))
  
  mc5 %>% inner_join(mc5_param,by = c("m4id","aeid")) %>% as.data.table
}


#' Title
#'
#' @param output 
#'
#' @return
#'
#' @examples
tcplFit2_unnest <- function(output){
  #unnest for use in invitrodb
  modelnames <- output$modelnames
  for(m in modelnames){
    assign(m,output[[m]][!names(output[[m]]) %in% c("pars","sds","modl")])
  }
  res <- mget(modelnames)
  
  test <- NULL
  for(m in modelnames){
    test <- rbind(test,data.frame(model = m,model_param = names(res[[m]]),model_val = unlist(res[[m]]), stringsAsFactors = FALSE, row.names = NULL))
  }
  test
}


#' Title
#'
#' @param test 
#'
#' @return
#'
#' @examples
tcplFit2_nest <- function(test){
  # renest
  modelnames <- unique(test$model)
  for(m in modelnames){
    assign(m,split(test[test$model == m,]$model_val,test[test$model == m,]$model_param))
  }
  
  out1 <- c(
    mget(modelnames),
    list(modelnames = modelnames)
  )
  
}

log_model_params <- function(datc){
  datc %>% mutate(model_val = case_when(model_param == "ac50" ~ log10(model_val),
                                        TRUE ~ model_val)) %>% as.data.table()
}

unlog_model_params <- function(datc){
  datc %>% mutate(model_val = case_when(model_param == "ac50" ~ 10^(model_val),
                                        TRUE ~ model_val)) %>% as.data.table()
}