# clear environment
rm(list = ls())
#-------------------------------------------------------------------------------------#
# populate environment with input values to replicate the error
#-------------------------------------------------------------------------------------#
# input 1 
lvl = 5, fld = "aeid", val = 2767, output = "pdf", multi = FALSE, verbose = TRUE 
# Error: More than 1 concentration series returned for given field/val combination.
# Set output to pdf or reduce the number of curves to 1. Current number of curves: 97

# input 2
lvl = 5, fld = "aeid", val =2767, output = "pdf", verbose = TRUE #multi = NULL, nrow = NULL, ncol = NULL
# with or without specifying produces same error
# Error: More than 1 concentration series returned for given field/val combination.
# Set output to pdf or reduce the number of curves to 1. Current number of curves: 97

# input 3
lvl = 5, fld = "m5id", val = 1589353, output = "pdf", multi = FALSE, verbose = TRUE
# If you try to plot only one curve by m5id , you get this weird message: 
#TableGrob (1 x 2) "arrange": 2 grobs
#z     cells    name               grob
#1 1 (1-1,1-1) arrange     gtable[layout]
#2 2 (1-1,2-2) arrange gtable[colhead-fg]

# input 4
lvl = 5, fld = "m5id", val = 1589353, output = "console", multi = FALSE, verbose = TRUE
#-------------------------------------------------------------------------------------#
# specify input that plots properly
lvl = 5, fld = "aeid", val= , output="pdf", verbose = TRUE
#-------------------------------------------------------------------------------------#
# install main/local version of tcpl and packages
#-------------------------------------------------------------------------------------#
install.packages("devtools")
devtools::install_git('https://ccte-bitbucket.epa.gov/scm/tox/tcpl.git', branch="TOX-538") 
library(tcpl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(data.table)
library(gridExtra)

tcplConf(user="kdunham", pass="Today072622K!!", db="invitrodb",
         drvr="MySQL", host="ccte-mysql-res.epa.gov") 
tcplConfList()
#-------------------------------------------------------------------------------------#
# pre-load user-defined functions
#-------------------------------------------------------------------------------------#
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
tcplPlotlyPlot <- function(dat, lvl = 5){
  
  #library(plotly)
  #library(dplyr)
  
  #variable binding
  model_stats <- model <- param <- value <- ac50 <- hitc <- NULL
  
  l3_dat <- tibble(conc = unlist(dat$conc), resp = unlist(dat$resp))
  #get models from columns that have an ac50 listed
  models <- gsub("_ac50","",colnames(dat)[grepl("_ac50",colnames(dat))])
  ac50s <- tibble(model = models, ac50 = dat %>% select(colnames(dat)[grepl("_ac50",colnames(dat))]) %>% unlist)
  #don't need loss direction ac50s
  ac50s <- ac50s %>% filter(!grepl("_loss",model))
  models <- models[!grepl("_loss",models)]
  # dat$models <- NULL
  # dat$ac50 <- NULL
  # l4_dat <- as_tibble(dat[3:length(dat)])
  # extract range from level 3 data for creating plotting all the functions
  # increase resolution to get smoother curves
  resolution <- 100
  l3_range <- l3_dat %>%
    pull(.data$conc) %>%
    range()
  
  l3_range <- l3_range * resolution
  x_range <- floor(l3_range[1]):ceiling(l3_range[2]) / resolution
  
  # calculate y values for each function
  if ("hill" %in% models) y_hill <- tcplfit2::hillfn(ps = c(dat$hill_tp,dat$hill_ga,dat$hill_p), x = x_range)
  #tp = ps[1], ga = ps[2], p = ps[3], la = ps[4], q = ps[5]
  if ("gnls" %in% models) y_gnls <- tcplfit2::gnls(ps = c(dat$gnls_tp,dat$gnls_ga,dat$gnls_p,dat$gnls_la,dat$gnls_q),x = x_range)
  #a = ps[1], b = ps[2]
  if ("exp2" %in% models) y_exp2 <- tcplfit2::exp2(ps = c(dat$exp2_a,dat$exp2_b), x = x_range)
  #a = ps[1], b = ps[2], p = ps[3]
  if ("exp3" %in% models) y_exp3 <- tcplfit2::exp3(ps = c(dat$exp3_a,dat$exp3_b,dat$exp3_p), x = x_range)
  #tp = ps[1], ga = ps[2]
  if ("exp4" %in% models) y_exp4 <- tcplfit2::exp4(ps = c(dat$exp4_tp,dat$exp4_ga), x = x_range)
  #tp = ps[1], ga = ps[2], p = ps[3]
  if ("exp5" %in% models) y_exp5 <- tcplfit2::exp5(ps = c(dat$exp5_tp,dat$exp5_ga,dat$exp5_p), x = x_range)
  #a = ps[1]
  if ("poly1" %in% models) y_poly1 <- tcplfit2::poly1(ps = c(dat$poly1_a), x = x_range)
  #a = ps[1], b = ps[2]
  if ("poly2" %in% models) y_poly2 <- tcplfit2::poly2(ps = c(dat$poly2_a,dat$poly2_b), x = x_range)
  #a = ps[1], p = ps[2]
  if ("pow" %in% models) y_pow <- tcplfit2::pow(ps = c(dat$pow_a,dat$pow_p), x = x_range)
  
  if (dat$fitc == 100) {
    # loec is stored as modl_acc
    x_loec <- rep(dat$modl_acc, resolution)
    l3_resp <- l3_dat %>%
      pull(.data$resp) %>%
      range()
    y_loec <- seq(from = l3_resp[1], to = l3_resp[2], length.out = resolution)
  }
  
  # for model type 0 (default) add constant model
  y_cnst <- x_range * 0
  ac50s <- ac50s %>% rbind(c(model = "cnst", ac50 = NA))
  models <- c(models, "cnst")
  
  
  model_stats <- dat %>%
    select(ends_with("aic"), ends_with("rme"), ends_with("_top"), ends_with("_p")) %>%
    tidyr::pivot_longer(everything(),
                        names_to = c("model", "param"),
                        names_pattern = "(.*)_(.*)"
    ) %>%
    tidyr::pivot_wider(names_from = param, values_from = value)
  ac50s$ac50 <- as.numeric(ac50s$ac50)
  
  # set background opacity
  op <- .2
  opacity <- tibble(model = models, opacity = op) %>% mutate(opacity = ifelse(.data$model == dat$modl, 1, opacity))
  line.fmt <- tibble(model = models, dash = "dash") %>% mutate(dash = ifelse(.data$model == dat$modl, "solid", .data$dash))
  
  # build data table for plotly
  m <- opacity %>%
    inner_join(line.fmt, by = "model") %>%
    inner_join(ac50s, by = "model") %>%
    rowwise() %>%
    mutate(x = ifelse(dat$fitc == 100,list(x_loec),list(x_range)), y = list(get(paste0("y_", .data$model)))) %>%
    tidyr::unnest(cols = c(x, y))
  
  # if we have model stats we want them included in the hoverover
  if (!is.null(model_stats)) {
    m <- m %>% inner_join(model_stats, by = "model")
  }
  
  # function for truncating decimals
  specify_decimal <- function(x, k) {
    if (!is.numeric(x)) {
      return(NA)
    }
    trimws(format(round(x, k), nsmall = k))
  }
  
  # start creation of actual plot
  fig <- plot_ly(
    data = l3_dat,
    x = ~conc,
    y = ~resp,
    type = "scatter",
    mode = "markers",
    name = "response",
    hoverinfo = "text",
    text = ~ paste(
      # "</br> Assay Plate ID: ", apid,
      "</br> Concentration: ", specify_decimal(conc,2),
      "</br> Response: ", specify_decimal(resp,2)
    )
  )
  # formatting for y axis
  y <- list(
    title = "Percent Activity",
    # set zeroline to false otherwise there would be a big horizontal line at y = 0
    zeroline = FALSE
  )
  # formatting for x axis
  x <- list(
    title = "Concentration",
    # set zeroline to false so there is no vertical line at x = 0
    type = "log",
    zeroline = FALSE
  )
  
  # function to generate vertical line
  vline <- function(x = 0, color = "black") {
    list(
      type = "line",
      y0 = 0,
      y1 = 1,
      yref = "paper",
      x0 = x,
      x1 = x,
      layer = "below",
      line = list(color = color, dash = "dash")
    )
  }
  
  # function to generate horizontal line
  hline <- function(y = 0, color = "black") {
    list(
      type = "line",
      y0 = y,
      y1 = y,
      x0 = 0,
      x1 = 1,
      xref = "paper",
      layer = "below",
      line = list(color = color, dash = "dash")
    )
  }
  
  # # add cutoff annotation
  fig <- fig %>% add_trace(
    data = tibble(x = x_range, y = dat$coff),
    x = ~x,
    y = ~y,
    type = "scatter",
    mode = "lines",
    name = "cutoff",
    line = list(dash = "dash", width = 1.5, color = NULL),
    inherit = FALSE,
    hoverinfo = "text",
    text = ~ paste(
      "</br>", paste0("Cut Off (", specify_decimal(dat$coff,2), ")")
    )
  )
  
  # currently only support for model types 1 and 0 but need to expand or make this generic
  if (dat$fitc == 100) {
    # apply axis and lines to figure
    fig <- fig %>% plotly::layout(xaxis = x, yaxis = y)
    
    # add the loec line if hitc == 1.
    fig <- fig %>% add_trace(
      data = tibble(x = x_loec, y = y_loec),
      x = ~x,
      y = ~y,
      name = "LOEC",
      type = "scatter",
      mode = "lines",
      line = list(dash = "solid", width = 1.5, color = NULL),
      inherit = FALSE,
      hoverinfo = "text",
      text = ~ paste(
        "</br>", "LOEC",
        "</br> Log Concentration: ", x
      )
    )
  } else {
    if (!dat$modl == "cnst") {
      dat_lines <- vline(ac50s %>% filter(model == dat$modl) %>% pull(ac50) %>% as.numeric())
      fig <- fig %>% plotly::layout(xaxis = x, yaxis = y, shapes = dat_lines)
    } else {
      fig <- fig %>% plotly::layout(xaxis = x, yaxis = y)
    }
    
    # add ac50 line for appropriate models (hitc=1)
    if (!dat$modl == "cnst") {
      fig <- fig %>% add_annotations(
        yref = "paper",
        xref = "x",
        x = ac50s %>% filter(model == dat$modl) %>% pull(ac50) %>% as.numeric() %>% log10(),
        y = 1,
        text = paste0("Winning Model Log AC50 (", specify_decimal(ac50s %>% filter(model == dat$modl) %>% pull(ac50) %>% as.numeric(),2), ")"),
        showarrow = F,
        textangle = 90,
        xanchor = "left"
      )
    }
    
    # add all non-winning models
    fig <- fig %>% add_trace(
      data = m %>% filter(.data$model != dat$modl),
      x = ~x,
      y = ~y,
      type = "scatter",
      mode = "lines",
      split = ~model,
      opacity = ~opacity,
      line = list(dash = ~dash, width = 1.5, color = NULL),
      inherit = FALSE,
      hoverinfo = "text",
      text = ~ paste(
        "</br>", model,
        "</br> ac50: ", specify_decimal(ac50, 2),
        "</br> Concentration: ", specify_decimal(x,2),
        "</br> Response: ", specify_decimal(y, 2),
        "</br> AIC: ", specify_decimal(aic, 2),
        "</br> RME: ", specify_decimal(rme, 2),
        "</br> TOP: ", specify_decimal(top, 2),
        "</br> SLOPE: ", specify_decimal(p, 2)
      )
    )
    
    # add line for winning model
    fig <- fig %>% add_trace(
      data = m %>% filter(.data$model == dat$modl),
      x = ~x,
      y = ~y,
      type = "scatter",
      mode = "lines",
      split = ~model,
      opacity = ~opacity,
      line = list(dash = ~dash, width = 1.5, color = NULL),
      inherit = FALSE,
      hoverinfo = "text",
      text = ~ paste(
        "</br>", model,
        "</br> ac50: ", specify_decimal(ac50, 2),
        "</br> Concentration: ", specify_decimal(x,2),
        "</br> Response: ", specify_decimal(y, 2),
        "</br> AIC: ", specify_decimal(aic, 2),
        "</br> RME: ", specify_decimal(rme, 2),
        "</br> TOP: ", specify_decimal(top, 2),
        "</br> SLOPE: ", specify_decimal(p, 2)
      )
    )
    
    # get hitcall
    hitcall <- dat %>% pull(hitc)
    
    # add annotations
    fig <- fig %>% add_annotations(
      text = paste0(
        dat %>% pull(.data$aenm), "<br>",
        case_when(
          hitcall == 1 ~ "ACTIVE",
          hitcall == 0 ~ "INACTIVE",
          hitcall == -1 ~ "NO CALL",
          TRUE ~ paste0(hitcall)
        ), "<br>",
        dat %>% pull(.data$chnm), " (", dat %>% pull(.data$casn), ")", "<br>",
        dat %>% pull(.data$dsstox_substance_id), "<br>",
        dat %>% pull(.data$spid), "<br>",
        ifelse(!is.null(dat$flag), gsub("\\|\\|", "<br>", paste0("Flags: ", dat %>% pull(.data$flag))), "")
      ),
      xref = "paper",
      x = 0.05,
      yref = "paper",
      y = .95,
      showarrow = FALSE,
      textposition = "bottom right",
      align = "left"
    )
  }
  # return figure
  fig
}
tcplggplot <- function(dat, lvl = 5, verbose = FALSE){
  
  #variable binding
  conc <- resp <- xpos <- ypos <- hjustvar <- vjustvar <- NULL
  annotateText <- name <- aic <- NULL
  l3_dat <- tibble(conc = unlist(dat$conc), resp = unlist(dat$resp))
  l3_range <- l3_dat %>%
    pull(.data$conc) %>%
    range()
  
  annotations <- data.frame(
    xpos = c(l3_range[1]),
    ypos =  c(Inf),
    annotateText = c(paste0(ifelse(verbose,"",paste0(
      "HITC: ", paste0(trimws(format(round(dat$hitc, 3), nsmall = 3))))), "\n",
      ifelse(!is.null(dat$flag), gsub("\\|\\|", "\n", paste0("Flags: ", dat %>% pull(.data$flag))), "")
    )),
    hjustvar = c(0) ,
    vjustvar = c(1)) #<- adjust
  
  #check if winning model has negative top.  If so coff should be negative
  if(!is.null(dat$top) && !is.null(dat$coff) && !is.na(dat$top)){
    if(dat$top<0){
      dat$coff <- dat$coff*-1
    }
  }
  
  winning_model_string <- paste0("Winning Model\n(",dat$modl,")")
  model_test <- function(modeltype){
    ifelse(dat$modl == modeltype,winning_model_string,"Losing Models")
  }
  
  gg <- ggplot(l3_dat, aes(conc, resp)) + 
    geom_function(aes(color = !!model_test("gnls"),linetype = !!model_test("gnls")),fun = function(x) tcplfit2::gnls(ps = c(dat$gnls_tp,dat$gnls_ga,dat$gnls_p,dat$gnls_la,dat$gnls_q),x = x)) +
    geom_function(aes(color = !!model_test("exp2"),linetype = !!model_test("exp2")),fun = function(x) tcplfit2::exp2(ps = c(dat$exp2_a,dat$exp2_b), x = x)) +
    geom_function(aes(color = !!model_test("exp3"),linetype = !!model_test("exp3")),fun = function(x) tcplfit2::exp3(ps = c(dat$exp3_a,dat$exp3_b,dat$exp3_p), x = x)) +
    geom_function(aes(color = !!model_test("exp4"),linetype = !!model_test("exp4")),fun = function(x) tcplfit2::exp4(ps = c(dat$exp4_tp,dat$exp4_ga), x = x)) +
    geom_function(aes(color = !!model_test("exp5"),linetype = !!model_test("exp5")),fun = function(x) tcplfit2::exp5(ps = c(dat$exp5_tp,dat$exp5_ga,dat$exp5_p), x = x)) +
    geom_function(aes(color = !!model_test("poly1"),linetype = !!model_test("poly1")),fun = function(x) tcplfit2::poly1(ps = c(dat$poly1_a), x = x)) + 
    geom_function(aes(color = !!model_test("poly2"),linetype = !!model_test("poly2")),fun = function(x) tcplfit2::poly2(ps = c(dat$poly2_a,dat$poly2_b), x = x)) + 
    geom_function(aes(color = !!model_test("pow"),linetype = !!model_test("pow")),fun = function(x) tcplfit2::pow(ps = c(dat$pow_a,dat$pow_p), x = x)) +
    geom_function(aes(color = !!model_test("hill"),linetype = !!model_test("hill")),fun = function(x) tcplfit2::hillfn(ps = c(dat$hill_tp,dat$hill_ga,dat$hill_p), x = x)) +
    geom_vline(aes(xintercept=dat$ac50, color = "AC50",linetype = "AC50")) +
    geom_hline(aes(yintercept=dat$coff, color = "Cutoff", linetype = "Cutoff")) +
    geom_point() + 
    scale_x_continuous(limits = l3_range, trans='log10') +
    scale_color_viridis_d("",direction = -1, guide = guide_legend(reverse = TRUE, order = 2)) +
    scale_linetype_manual("",guide = guide_legend(reverse = TRUE, order = 2), values = c(2,2,3,1)) +
    xlab("Concentration (\u03BCM)") + 
    ylab(stringr::str_to_title(gsub("_"," ",dat$normalized_data_type))) +
    geom_text(data=annotations,aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label=annotateText)) +
    labs(
      title = stringr::str_wrap(paste0(
        dat %>% pull(.data$chnm), " (", dat %>% pull(.data$casn), ") ",
        dat %>% pull(.data$dsstox_substance_id)
      ),50),
      subtitle = paste0(
        "SPID: ", dat %>% pull(.data$spid), " ",
        "AENM: ", dat %>% pull(.data$aenm)
      )
    ) +
    theme(legend.title=element_blank(),
          legend.margin = margin(0, 0, 0, 0),
          legend.spacing.x = unit(0, "mm"),
          legend.spacing.y = unit(0, "mm"))
  
  
  p <- lapply(dat %>% select(contains("aic")) %>% colnames() %>% stringr::str_extract("[:alnum:]+"), function(x) {
    dat %>%
      select(contains(paste0(x,c("_aic","_rme")))) %>%
      tidyr::pivot_longer(cols = everything()) %>% as_tibble()
  })
  
  round_n <- function(x,n){format(round(x, n), nsmall = 3)}
  
  combined_p <- data.table::rbindlist(p)
  pivoted_p <- combined_p %>% tidyr::extract(name,c("model","param"),"([[:alnum:]]+)_([[:alnum:]]+)") %>% pivot_wider(names_from = "param",values_from = "value")
  pivoted_p <- pivoted_p %>% mutate_if(is.numeric,~round_n(.,3))
  pivoted_p <- pivoted_p %>% arrange(as.numeric(aic))
  #print(pivoted_p)
  t <- tableGrob(pivoted_p,rows = NULL)
  
  l5_details <- tibble(Hitcall = dat$hitc, BMD = dat$bmd, AC50 = dat$ac50)
  l5_details <- l5_details %>% mutate_if(is.numeric,~round_n(.,3))
  l5_t <- tableGrob(l5_details,rows = NULL)
  
  
  valigned <- gtable_combine(l5_t,t, along=2)
  
  ifelse(verbose,
         return(arrangeGrob(gg,valigned,nrow = 1,widths = 2:1)),
         return(gg))
  
}

#-------------------------------------------------------------------------------------#
# tcplPlot.R starts here
#-------------------------------------------------------------------------------------#
#variable binding
  resp <- NULL
  # check_tcpl_db_schema is a user-defined function found in v3_schema_functions.R file
  if(check_tcpl_db_schema()) {
    # check that input combination is unique
    input <- tcplLoadData(lvl = lvl, fld = fld, val = val)
    if(nrow(input) == 0) stop("No data for fld/val provided")
    # default assign for output="pdf" is multi=TRUE
    if (output == "pdf" && is.null(multi)) {
     multi <- TRUE
    }
    if(nrow(input) > 1 && output == "pdf" && multi == FALSE) stop("Currently, plotting multiple plots per page (multi=TRUE) in pdf output is supported. Plotting multiple plots, one plot per page (multi=FALSE) in pdf output is currently in development.")
    if(nrow(input) > 1  && multi == FALSE) stop("More than 1 concentration series returned for given field/val combination.  Set output to pdf or reduce the number of curves to 1. Current number of curves: ", nrow(input))
    # default assign for output="console" is multi=FALSE
    if(output =="console" && is.null(multi)) {
      multi <- FALSE
      }
    if(is.null(nrow)){
      nrow <- ifelse(verbose,2,2)
      }
    if(is.null(ncol)){
      ncol <- ifelse(verbose,2,3)
      }
  
  m4id <- input$m4id
  
  # load dat
  l4 <- tcplLoadData(lvl = 4, fld = "m4id", val = m4id, add.fld = T)
  agg <- tcplLoadData(lvl = "agg", fld = "m4id", val = m4id)
  
  
  if (lvl >= 5L) {
    l5 <- tcplLoadData(lvl = 5, fld = "m4id", val = m4id, add.fld = T)
    dat <- l4[l5, on = "m4id"]
  }
  
  dat <- tcplPrepOtpt(dat)
  
  # add normalized data type for y axis
  ndt <- tcplLoadAeid(fld = "aeid", val = dat$aeid, add.fld = "normalized_data_type")
  dat <- dat[ndt, on = "aeid"]
  
  # unlog concs
  conc_resp_table <- agg %>% group_by(m4id) %>% summarise(conc = list(10^logc), resp = list(resp)) %>% as.data.table()
  dat <- dat[conc_resp_table, on = "m4id"]
  # dat$conc <- list(10^agg$logc)
  # dat$resp <- list(agg$resp)
  # added AND verbose=FALSE to nrow(input)=1 condition to avoid TableGrob error in tcplggplot
  if (nrow(input) == 1 && verbose==FALSE) {
    # plot single graph
    # this needs to be fixed to be more succinct about users selected option
    ifelse(output[1] == "console",
           # tcplPlotlyPlot() is the user-defined function found in tcplPlot.R file used to connect tcpl and plotly packages
           return(tcplPlotlyPlot(dat)),
           # tcplggplot() is the user-defined function found in tcplPlot.R file used to connect tcpl and ggplot2 packages  
           return(tcplggplot(dat, verbose = verbose))
    ) 
  } else {
    split_dat <- list(dat)
    if(!is.null(by)){
      split_dat <- split(dat,f = factor(dat %>% pull(all_of(by))))
    }
    for(d in split_dat){
      plot_list <- by(d, (nrow(d)), tcplggplot, verbose = verbose)
      # m1 <- do.call("marrangeGrob", c(plot_list, ncol=2))
      m1 <- marrangeGrob(plot_list, nrow = nrow, ncol = ncol)
      if(!verbose){
        ggsave(paste0(fileprefix,ifelse(is.null(by),"", paste0("_",by,"_",d %>% pull(all_of(by)) %>% unique())), ".pdf"), m1,width = ncol*4.88, height = nrow*3.04)
        }
      else{
        ggsave(paste0(fileprefix,ifelse(is.null(by),"",paste0("_",by,"_",d %>% pull(all_of(by)) %>% unique())), ".pdf"), m1,width = ncol*7, height = nrow*5)
      }
    }
  }
  
} else {
  if (length(lvl) > 1 | !lvl %in% 4:7) stop("invalid lvl input.")
  if (length(output) > 1) output <- output[1]
  
  prs <- list(type = "mc", fld = fld, val = val)
  
  if (lvl == 4L) dat <- do.call(tcplLoadData, args = c(lvl = 4L, prs))
  if (lvl >= 5L) dat <- do.call(tcplLoadData, args = c(lvl = 5L, prs))
  if (lvl >= 6L) {
    flg <- do.call(tcplLoadData, args = c(lvl = 6L, prs))
  } else {
    flg <- NULL
  }
  if (lvl == 7L) {
    boot <- do.call(tcplLoadData, args = c(lvl = 7L, prs))
  } else {
    boot <- NULL
  }
  
  if (nrow(dat) == 0) stop("No data for fld/val provided")
  
  agg <- do.call(tcplLoadData, args = c(lvl = "agg", prs))
  
  if (nrow(dat) == 1 & output == "console") {
    tcplPlotFits(dat = dat, agg = agg, flg = flg, boot = boot)
  }
  if (nrow(dat) > 1 & output == "console") stop("More than 1 concentration series returned for given field/val combination.  Set output to pdf or reduce the number of curves to 1. Current number of curves: ", nrow(dat))
  
  
  if (is.null(by)) {
    if (output == "pdf" & !multi) {
      graphics.off()
      pdf(
        file = file.path(
          getwd(),
          paste0(fileprefix, ".", output)
        ),
        height = 6,
        width = 10,
        pointsize = 10
      )
      tcplPlotFits(dat = dat, agg = agg, flg = flg, boot = boot)
      graphics.off()
    }
    # plotting if using multiplot function
    hitc.all <- TRUE
    # browser()
    if (multi) {
      graphics.off()
      pdf(file = file.path(getwd(), paste0(fileprefix, ".", output)), height = 10, width = 6, pointsize = 10)
      par(mfrow = c(3, 2))
      tcplMultiplot(dat = dat, agg = agg, flg = flg, boot = boot, hitc.all = hitc.all)
      graphics.off()
    }
  } else {
    if (!by %in% names(dat)) stop("grouping variable unavailable.")
    subset <- unlist(unique(dat[, by, with = FALSE]))
    for (s in subset) {
      if (output == "pdf" & !multi) {
        graphics.off()
        pdf(
          file = file.path(
            getwd(),
            paste0(fileprefix, "_", by, "_", s, ".", output)
          ),
          height = 6,
          width = 10,
          pointsize = 10
        )
        tcplPlotFits(dat = dat[get(by) == s], agg = agg, flg = flg, boot = boot)
        graphics.off()
      }
      # plotting if using multiplot function
      hitc.all <- TRUE
      # browser()
      if (multi) {
        graphics.off()
        pdf(file = file.path(getwd(), paste0(fileprefix, "_", by, "_", s, ".", output)), height = 10, width = 6, pointsize = 10)
        par(mfrow = c(3, 2))
        tcplMultiplot(dat = dat[get(by) == s], agg = agg, flg = flg, boot = boot, hitc.all = hitc.all)
        graphics.off()
      }
    }
  }
}
