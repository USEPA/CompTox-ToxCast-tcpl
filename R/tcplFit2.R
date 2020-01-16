#' tcpl Wrapper for tcplfit2_core including additional calculations to fit into new schema
#'
#' @param dat
#'
#' @return
#' @importFrom tcplfit2 tcplfit2_core
#' @examples
tcplFit2 <- function(dat) {
  # do all the regular fitting things that still need to be done
  res <- dat[, `:=`(c("rmns", "rmds", "nconcs", "med_rmds"), {
    rmns <- mean(resp)
    rmds <- median(resp)
    nconcs <- .N
    med_rmds <- rmds >= (3 * bmad)
    .(rmns, rmds, nconcs, med_rmds)
  }), keyby = .(aeid, spid, logc)][, .(
    bmad = min(bmad), resp_max = max(resp),
    resp_min = min(resp), max_mean = max(rmns), max_mean_conc = logc[which.max(rmns)],
    max_med = max(rmds), max_med_conc = logc[which.max(rmds)],
    logc_max = max(logc), logc_min = min(logc), nconc = length(unique(logc)),
    npts = .N, nrep = median(as.numeric(nconcs)), nmed_gtbl = sum(med_rmds) / first(nconcs),
    concentration_unlogged = list(exp(logc)), response = list(resp)
  ),
  keyby = .(aeid, spid)
  ][, `:=`(tmpi = seq_len(.N)), keyby = .(aeid)][,
    `:=`(fitparams = list(tcplfit2::tcplfit2_core(unlist(concentration_unlogged),
      unlist(response), 0,
      verbose = FALSE, force.fit = TRUE
    ))),
    keyby = .(spid)
  ]

  # return dat with fitted curves in fitparams
  res
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
