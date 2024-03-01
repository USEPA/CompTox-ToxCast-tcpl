#' tcpl Wrapper for tcplfit2_core including additional calculations to fit into new schema
#'
#' @param dat output from level 3 processing
#' @param fitmodels list of the models that should be fit with the data
#' @param bmed baseline value, typically should be 0
#'
#' @return Data.table with an additional column fitparams that includes all of the fitting parameters
#' @importFrom tcplfit2 tcplfit2_core
tcplFit2 <- function(dat,
                     fitmodels = c("cnst", "hill", "gnls", "poly1", "poly2", "pow", "exp2", "exp3", "exp4", "exp5"),
                     bmed = NULL) {
  #variable binding
  resp  <-bmad  <-aeid  <-osd  <-m3id <- concentration_unlogged  <-response <- NULL
  # do all the regular fitting things that still need to be done
  res <- dat[, `:=`(c("rmns", "rmds", "nconcs", "med_rmds_pos", "med_rmds_neg"), {
    rmns <- mean(resp)
    rmds <- median(resp)
    nconcs <- .N
    med_rmds_pos <- rmds >= (3 * bmad)
    med_rmds_neg <- rmds <= (-3 * bmad)
    .(rmns, rmds, nconcs, med_rmds_pos, med_rmds_neg)
  }), keyby = .(aeid, spid, conc)][, .(
    bmad = min(bmad), osd = min(osd), bmed = ifelse(is.null(bmed), 0, max(bmed)),
    resp_max = max(resp), resp_min = min(resp),
    max_mean = max(rmns), max_mean_conc = conc[which.max(rmns)],
    max_med = max(rmds), max_med_conc = conc[which.max(rmds)],
    min_mean = min(rmns), min_mean_conc = conc[which.min(rmns)],
    min_med = min(rmds), min_med_conc = conc[which.min(rmds)],
    conc_max = max(conc), conc_min = min(conc), nconc = length(unique(conc)),
    npts = .N, nrep = median(as.numeric(nconcs)), 
    nmed_gtbl_pos = sum(med_rmds_pos) / first(nconcs), 
    nmed_gtbl_neg = sum(med_rmds_neg) / first(nconcs),
    concentration_unlogged = list(conc), response = list(resp), m3ids = list(m3id)
  ),
  keyby = .(aeid, spid)
  ][, `:=`(c("max_med_diff", "max_med_diff_conc"), {
    max_med_diff <- ifelse(abs(max_med) > abs(min_med), max_med, min_med)
    max_med_diff_conc <- ifelse(abs(max_med) > abs(min_med), max_med_conc, min_med_conc)
    .(max_med_diff, max_med_diff_conc)
  })][, `:=`(tmpi = seq_len(.N)), keyby = .(aeid)][,
    `:=`(fitparams = list(tcplfit2::tcplfit2_core(unlist(concentration_unlogged),
      unlist(response),
      cutoff = bmad,
      bidirectional = TRUE,
      verbose = FALSE, force.fit = TRUE,
      fitmodels = fitmodels
    ))),
    keyby = .(spid)
  ]

  # return dat with fitted curves in fitparams
  res
}

#' Hitcalling with tcplfit2
#'
#' @param mc4 data.table with level 4 data 
#' @param coff cutoff value for hitcalling
#' 
#'
#' @return Data.table with key value pairs of hitcalling parameters
#' @importFrom dplyr %>% filter group_by summarise left_join inner_join select rowwise mutate pull ungroup
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr separate_wider_delim
#' @importFrom tcplfit2 tcplhit2_core
tcplHit2 <- function(mc4, coff) {
  
  #variable binding
  top <- a <- b <- ga <- la <- p <- tp <- conc_min <- conc_max <- fitc <- NULL
  model <- m4id  <-model_param  <-model_val  <-resp  <- NULL
  params  <-conc  <-bmed  <-onesd  <-df  <-aeid  <- NULL
  fit_method  <-hitcall  <-cutoff  <-top_over_cutoff  <-bmd  <-hit_val <- NULL

  long_mc4 <- mc4 |> tidyr::pivot_longer(cols = matches("cnst|hill|gnls|poly1|poly2|pow|exp2|exp3|exp4|exp5|all|errfun"), names_to = "model", values_to = "model_val") |> tidyr::separate_wider_delim(col = "model",delim = "_", names = c("model","model_param"), too_many = "merge")
  
  nested_mc4 <- long_mc4 %>%
    filter(model != "all") %>%
    group_by(m4id) %>%
    summarise(params = list(tcplFit2_nest(data.table(model = model, model_param = model_param, model_val = model_val))))

  # get lvl 3 conc/resp information
  l4_agg <- tcplLoadData(lvl = "agg", fld = "m4id", val = nested_mc4$m4id)
  l3_dat <- l4_agg %>% left_join(tcplLoadData(lvl = 3, fld = "m3id", val = l4_agg$m3id), by = c("aeid", "m3id", "m2id", "m1id", "m0id", "spid", "conc", "resp"))

  # unlog and plug into nested
  nested_mc4 <- nested_mc4 %>% left_join(l3_dat %>% group_by(m4id) %>% summarise(conc = list(conc), resp = list(resp)), by = "m4id")

  # rejoin the onesd for tcplfit2
  nested_mc4 <- nested_mc4 %>% inner_join(long_mc4 %>% filter(model_param == "onesd") %>% select(m4id, onesd = model_val), by = "m4id")

  # rejoin for bmed
  nested_mc4 <- nested_mc4 %>% inner_join(long_mc4 %>% filter(model_param == "bmed") %>% select(m4id, bmed = model_val), by = "m4id")

  # add the cutoff
  # nested_mc4$cutoff <- coff
  # browser()
  test <- nested_mc4 %>%
    dplyr::rowwise() %>%
    mutate(df = list(tcplhit2_core(params = params, conc = unlist(conc), resp = unlist(resp), bmed = bmed, cutoff = coff, onesd = onesd, bmd_low_bnd = .1, bmd_up_bnd = 10))) %>%
    select(-conc, -resp)
  # test <- NULL
  # for(id in nested_mc4$m4id){
  #   l4_dat <- nested_mc4 %>% filter(m4id == id)
  #   test <- rbind(test,
  #                 tcplhit2_core(params = l4_dat$params[[1]],
  #                 conc = unlist(l4_dat$conc),
  #                 resp = unlist(l4_dat$resp),
  #                 bmed = l4_dat$bmed,
  #                 cutoff = coff,
  #                 onesd = l4_dat$onesd) %>% select(-conc,-resp))
  # }

  res <- cbind(test %>% as.data.table(), test %>% as.data.table() %>% pull(df) %>% rbindlist())
  
  # fitc calculations -------------------------------------------------------
  res <- res %>%
    rowwise() %>%
    mutate(ac95 = tcplfit2::acy(.95 * top, list(a = a, b = b, ga = ga, la = la, p = p, q = q, tp = tp)[!is.na(list(a = a, b = b, ga = ga, la = la, p = p, q = q, tp = tp))], fit_method)) %>%
    dplyr::ungroup()
  res <- res %>% mutate(coff_upper = 1.2 * cutoff, coff_lower = .8 * cutoff)
  res <- res %>%
    left_join(long_mc4 %>% select(m4id, conc_min, conc_max) %>% unique(), by = "m4id") %>%
    mutate(fitc = case_when(
      hitcall >= .9 & abs(top) <= coff_upper & ac50 <= conc_min ~ 36L,
      hitcall >= .9 & abs(top) <= coff_upper & ac50 > conc_min & ac95 < conc_max ~ 37L,
      hitcall >= .9 & abs(top) <= coff_upper & ac50 > conc_min & ac95 >= conc_max ~ 38L,
      hitcall >= .9 & abs(top) > coff_upper & ac50 <= conc_min ~ 40L,
      hitcall >= .9 & abs(top) > coff_upper & ac50 > conc_min & ac95 < conc_max ~ 41L,
      hitcall >= .9 & abs(top) > coff_upper & ac50 > conc_min & ac95 >= conc_max ~ 42L,
      hitcall < .9 & abs(top) < coff_lower ~ 13L,
      hitcall < .9 & abs(top) >= coff_lower ~ 15L,
      fit_method == "none" ~ 2L,
      TRUE ~ NA_integer_
    ))
  
  
  # mc5 table
  mc5 <- res %>%
    left_join(long_mc4 %>% select(m4id, aeid) %>% unique(), by = "m4id") %>%
    select(m4id, aeid, modl = fit_method, hitc = hitcall,fitc, coff = cutoff) %>%
    mutate(model_type = 2)

  # mc5 param table
  mc5_param <- res %>%
    left_join(long_mc4 %>% select(m4id, aeid) %>% unique(), by = "m4id") %>%
    select(m4id, aeid, top_over_cutoff:bmd)
  mc5_param <- mc5_param %>%
    tidyr::pivot_longer(cols = top_over_cutoff:bmd, names_to = "hit_param", values_to = "hit_val") %>%
    filter(!is.na(hit_val))

  mc5 %>%
    inner_join(mc5_param, by = c("m4id", "aeid")) %>%
    as.data.table()
  
}


#' Unnest tcplfit2 parameters into a dataframe
#'
#' @param output list of output from tcplfit2
#'
#' @return list of parameters unnested and compiled into a dataframe
tcplFit2_unnest <- function(output) {
  # unnest for use in invitrodb
  modelnames <- output$modelnames
  for (m in modelnames) {
    assign(m, output[[m]][!names(output[[m]]) %in% c("pars", "sds", "modl")])
  }
  res <- mget(modelnames)

  test <- NULL
  for (m in modelnames) {
    lst <- lapply(res[[m]], function(x){ if(length(x) < 1) { x <- NA }; x })
    test <- rbind(test, data.frame(model = m, model_param = names(lst), model_val = unlist(lst), stringsAsFactors = FALSE, row.names = NULL))
  }
  # include error function and return
  rbind(test, list(model = "errfun", model_param = output$errfun, model_val = NA))
}


#' Nest dataframe into a list that is readable by tcplfit2
#'
#' @param dat a dataframe that has all of the fitting parameters in the style of tcplloaddata
#'
#' @return a list of fitting parameters that can be consumed by tcplfit2
tcplFit2_nest <- function(dat) {
  # get errfun and filter it out
  errfun <- filter(dat, model == "errfun")$model_param
  dat <- filter(dat, model != "errfun")
  
  # renest
  modelnames <- unique(dat$model)
  for (m in modelnames) {
    assign(m, split(dat[dat$model == m, ]$model_val, dat[dat$model == m, ]$model_param))
  }
  
  # each model requires the pars parameter for tcplhit2_core
  for (m in modelnames) {
    if(m == "cnst") modpars <-  "er"
    if(m == "exp2") modpars <- paste0(c("a", "b", "er"))
    if(m == "exp3") modpars <- paste0(c("a", "b", "p", "er"))
    if(m == "exp4") modpars <- paste0(c("tp", "ga", "er"))
    if(m == "exp5") modpars <- paste0(c("tp", "ga", "p", "er"))
    if(m == "hill") modpars <- paste0(c("tp", "ga", "p", "er"))
    if(m == "poly1") modpars <- paste0(c("a", "er"))
    if(m == "poly2") modpars <- paste0(c("a", "b", "er"))
    if(m == "pow") modpars <- paste0(c("a", "p", "er"))
    if(m == "gnls") modpars <- paste0(c("tp", "ga", "p", "la", "q", "er"))
    assign(m,append(get(m),list(pars = modpars)))
  }

  out1 <- c(
    mget(modelnames),
    list(modelnames = modelnames),
    errfun = errfun
  )
}

log_model_params <- function(datc) {
  datc %>%
    mutate(model_val = case_when(
      model_param == "ac50" ~ log10(model_val),
      TRUE ~ model_val
    )) %>%
    as.data.table()
}

unlog_model_params <- function(datc) {
  datc %>%
    mutate(model_val = case_when(
      model_param == "ac50" ~ 10^(model_val),
      TRUE ~ model_val
    )) %>%
    as.data.table()
}
