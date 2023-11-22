#' #-------------------------------------------------------------------------------
# tcplPlot: plot tcpl data
#-------------------------------------------------------------------------------

#'  Generic Plotting Function for tcpl
#'
#' @description
#' \code{tcplLoadData} queries the tcpl databases and returns a plot
#' for the given level and data type.
#'
#' @param type Character of length 1, the data type, "sc" or "mc".
#' @param fld Character, the field(s) to query on.
#' @param val List, vectors of values for each field to query on. Must be in
#' the same order as 'fld'.
#' @param output How should the plot be presented. To view the plot in application,
#'  use "console", or to save as a file type, use "pdf", "jpg", "png", "svg", or "tiff".
#' @param multi Boolean, by default TRUE for "pdf". If multi is TRUE, output
#' by  default 4 plots per page for 'verbose' = TRUE and 6 plots per page for
#' 'verbose' = FALSE.
#' @param fileprefix Prefix of file when saving.
#' @param by Parameter to divide files into e.g. "aeid".
#' @param verbose Boolean, by default FALSE. If TRUE, a table with fitting parameters
#'  is included with the plot.
#' @param nrow Integer, number of rows in multiplot. By default 2.
#' @param ncol Integer, number of columns in multiplot. By default 3, 2 if verbose.
#' @param dpi Integer, image print resolution. By default 600.
#' @param flags Boolean, by default FALSE. If TRUE, level 6 flags are displayed
#' below annotations on plot
#' @param yuniform Boolean, by default FALSE. If TRUE, all plots will have uniform
#' y axis scaling, automatically determined.
#' @param yrange Integer of length 2, for directly setting the y-axis range, 
#' c(<min>,<max>). By default, c(NA,NA).
#'
#' @details
#' The data type can be either 'mc' for mutliple concentration data, or 'sc'
#' for single concentration data. Multiple concentration data will be loaded
#' into the 'mc' tables, whereas the single concentration will be loaded into
#' the 'sc' tables.
#'
#' Leaving \code{fld} NULL will return all data.
#' @import data.table
#' @importFrom gridExtra marrangeGrob
#' @importFrom ggplot2 ggsave
#' @importFrom dplyr %>% all_of pull
#' @export
#'
#' @examples
#' ## Store the current config settings, so they can be reloaded at the end
#' ## of the examples
#' conf_store <- tcplConfList()
#' tcplConfExample()
#'
#' tcplPlot(fld = "m4id", val = c(18609966)) ## Create a level 4 plot
#'
#' ## Reset configuration
#' options(conf_store)
tcplPlot <- function(type = "mc", fld = "m4id", val = NULL, by = NULL, output = c("console", "pdf", "png", "jpg", "svg", "tiff"), fileprefix = paste0("tcplPlot_", Sys.Date()), multi = NULL, verbose = FALSE, nrow = NULL, ncol = NULL, dpi = 600, flags = FALSE, yuniform = FALSE, yrange=c(NA,NA)) {
  #variable binding
  conc_unit <- bmd <- resp <- NULL
  
  resp <- NULL
  lvl <- 5
  if (type == "sc") {
    lvl <- 2
    if (flags == TRUE) {
      warning("'flags' was set to TRUE - no flags exist for plotting single concentration")
    }
  }
  
  if (length(yrange) != 2) {
    stop("'yrange' must be of length 2")
  }

  # check_tcpl_db_schema is a user-defined function found in v3_schema_functions.R file
  if (check_tcpl_db_schema()) {
    # check that input combination is unique
    input <- tcplLoadData(lvl = lvl, fld = fld, val = val, type = type)
    if (nrow(input) == 0) stop("No data for fld/val provided")
    # default assign multi=TRUE for output="pdf" 
    if (output == "pdf" && is.null(multi)) {
      multi <- TRUE
    }
    # forced assign multi=FALSE for output = c("console","png","jpg","svg","tiff"), verbose=FALSE for output="console"
    if (output !="pdf") {
      multi <- FALSE
      if(output =="console"){
        verbose <- FALSE
      }
    }
    # assign nrow = ncol = 1 for output="pdf" and multi=FALSE to plot one plot per page
    if(nrow(input) > 1 && output == "pdf" && multi == FALSE) {
      nrow = ncol = 1
    }
    # error message for output="console" and multi=FALSE to avoid multiple plots in console
    if(nrow(input) > 1 && output == "console" && multi == FALSE) stop("More than 1 concentration series returned for given field/val combination.  Set output to pdf or reduce the number of curves to 1. Current number of curves: ", nrow(input))
    if(is.null(nrow)){
      nrow <- ifelse(verbose,2,2)
    }
    if(is.null(ncol)){
      ncol <- ifelse(!verbose | type == "sc",3,2)
    }
    
    if (type == "mc") {
      m4id <- input$m4id
      
      # load dat
      l4 <- tcplLoadData(lvl = 4, fld = "m4id", val = m4id, add.fld = T)
      agg <- tcplLoadData(lvl = "agg", fld = "m4id", val = m4id)
      l5 <- tcplLoadData(lvl = 5, fld = "m4id", val = m4id, add.fld = T)
      dat <- l4[l5, on = "m4id"]
      if (flags == TRUE) {
        l6 <- tcplLoadData(lvl=6, fld='m4id', val=m4id, type='mc')
        if (nrow(l6) > 0) {
          l6 <- l6[ , .( flag = paste(flag, collapse=";\n")), by = m4id]
          no_flags <- setdiff(m4id, l6$m4id)
          if (length(no_flags) > 0) {
            l6 <- rbindlist(list(l6, data.table("m4id" = no_flags, "flag" = "None")))
          } 
        } else {
          l6 <- data.table(m4id, "flag" = "None")
        }
        dat <- dat[l6, on = "m4id"]
      }
      dat <- tcplPrepOtpt(dat)
    } else {
      s2id <- input$s2id
      
      # load dat
      l2 <- tcplLoadData(lvl = lvl, fld = "s2id", val = s2id, type = "sc", add.fld = T)
      agg <- tcplLoadData(lvl = "agg", fld = "s2id", val = s2id, type = "sc")
      dat <- tcplPrepOtpt(l2)
      
    }
    
    
    
    # correct concentration unit label for x-axis
    dat <- dat[is.na(conc_unit), conc_unit:="\u03BCM"]
    dat <- dat[conc_unit=="uM", conc_unit:="\u03BCM"]
    dat <- dat[conc_unit=="mg/l", conc_unit:="mg/L"]
    
    # add normalized data type for y axis
    ndt <- tcplLoadAeid(fld = "aeid", val = dat$aeid, add.fld = "normalized_data_type")
    dat <- dat[ndt, on = "aeid"]
    
    # check for null bmd in dat table
    if (verbose){
      dat <- dat[is.null(dat$bmd), bmd:=NA]
    }
    
    # unlog concs
    if (type == "mc") {
      conc_resp_table <- agg %>% group_by(m4id) %>% summarise(conc = list(10^logc), resp = list(resp)) %>% as.data.table()
      dat <- dat[conc_resp_table, on = "m4id"]
    } else {
      conc_resp_table <- agg %>% group_by(s2id) %>% summarise(conc = list(10^logc), resp = list(resp)) %>% as.data.table()
      dat <- dat[conc_resp_table, on = "s2id"]
    }
    
    # set range
    if (yuniform == TRUE && identical(yrange, c(NA,NA))) {
      min <- min(dat$resp_min, unlist(dat$resp))
      max <- max(dat$resp_max, unlist(dat$resp))
      # any bidirectional models contained in dat, cutoff both ways
      if (2 %in% dat$model_type) {
        cutoffs <- dat[model_type == 2]$coff
        min <- min(min, cutoffs, cutoffs * -1)
        max <- max(max, cutoffs, cutoffs * -1)
      }
      # any gain models contained in dat, cutoff only positive
      if (3 %in% dat$model_type) {
        cutoffs <- dat[model_type == 3]$coff
        min <- min(min, cutoffs)
        max <- max(max, cutoffs)
      }
      # any loss models contained in dat, cutoff only negative
      if (4 %in% dat$model_type) {
        cutoffs <- dat[model_type == 4]$coff
        min <- min(min, cutoffs * -1)
        max <- max(max, cutoffs * -1)
      }
      yrange = c(min, max)
    }
    
    # dat$conc <- list(10^agg$logc)
    # dat$resp <- list(agg$resp)
    # added AND verbose=FALSE to nrow(input)=1 condition to avoid TableGrob error in tcplggplot
    if (nrow(input) == 1 && verbose==FALSE) {
      # plot single graph
      # this needs to be fixed to be more succinct about users selected option
      ifelse(output[1] == "console",
      # tcplPlotlyplot is the user-defined function found in tcplPlot.R file used to connect tcpl and plotly packages
      # tcplggplot is the user-defined function found in tcplPlot.R file used to connect tcpl and ggplot2 packages
        return(tcplPlotlyPlot(dat, lvl)),
        return(ggsave(filename=paste0(fileprefix,"_",dat$m4id,".",output),
                      plot=tcplggplot(dat,verbose = verbose, lvl = lvl, flags = flags, yrange = yrange), width = 7, height = 5, dpi=dpi))
      )
    } else {
      split_dat <- list(dat)
      if(!is.null(by)){
        split_dat <- split(dat,f = factor(dat %>% pull(all_of(by))))
      }
      for(d in split_dat){
        plot_list <- by(d,seq(nrow(d)),tcplggplot,verbose = verbose, lvl = lvl, flags = flags, yrange = yrange)
        # m1 <- do.call("marrangeGrob", c(plot_list, ncol=2))
        m1 <- marrangeGrob(plot_list, nrow = nrow, ncol = ncol)
        if(output=="pdf"){
          w <- ifelse(type == "mc", ncol*7, ncol*5)
          h <- ifelse(type == "mc", nrow*5, nrow*6)
          ggsave(paste0(fileprefix,ifelse(is.null(by),"",paste0("_",by,"_",d %>% pull(all_of(by)) %>% unique())), ".pdf"), m1,width = w, height = h)
        } else {
          names(plot_list) <- d$m4id
          w <- ifelse(type == "mc", 7, 4)
          h <- ifelse(type == "mc", 5, 6)
          lapply(names(plot_list), function(x)ggsave(filename=paste0(fileprefix,"_",x,".",output),
                                                     plot=arrangeGrob(grobs=plot_list[x]), width = 7, height = 5, dpi=dpi))
        }
      }
    }

  } else {

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
}


#' tcplPlotlyPlot
#'
#' @param dat data table with all required conc/resp data
#' @param lvl integer level of data that should be plotted
#' level 2 - for 'sc' plotting
#' level 5 - for 'mc' plotting, all fit models and winning model with hitcall
#'
#' @return A plotly plot
#' @importFrom dplyr %>% filter group_by summarise left_join inner_join select rowwise mutate pull
#' @importFrom dplyr tibble ends_with everything case_when
#' @importFrom plotly plot_ly add_trace add_annotations
#' @importFrom dplyr .data
#'
tcplPlotlyPlot <- function(dat, lvl = 5){
  
  #library(plotly)
  #library(dplyr)
  
  #variable binding
  model_stats <- model <- param <- value <- ac50 <- hitc <- NULL
  
  l3_dat <- tibble(conc = unlist(dat$conc), resp = unlist(dat$resp))
  
  # extract range from level 3 data for creating plotting all the functions
  # increase resolution to get smoother curves
  resolution <- 100
  x_min_max <- range(l3_dat$conc)
  x_range <- 10^(seq(from = log10(x_min_max[1]), to = log10(x_min_max[2]), length.out = resolution))
  
  
  #check if winning model = none 
  if (!lvl == 2 && !dat$modl == "none"){
  
    #check if winning model has negative top.  If so coff should be negative
    if(!is.null(dat$top) && !is.null(dat$coff) && !is.na(dat$top)){
      if(dat$top<0){
        dat$coff <- dat$coff*-1
      }
    }
    
    #get models from columns that have an ac50 listed
    models <- gsub("_ac50","",colnames(dat)[grepl("_ac50",colnames(dat))])
    ac50s <- tibble(model = models, ac50 = dat %>% select(colnames(dat)[grepl("_ac50",colnames(dat))]) %>% unlist)
    #don't need loss direction ac50s
    ac50s <- ac50s %>% filter(!grepl("_loss",model))
    models <- models[!grepl("_loss",models)]
    # dat$models <- NULL
    # dat$ac50 <- NULL
    # l4_dat <- as_tibble(dat[3:length(dat)])
    
    
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
    title = stringr::str_to_title(gsub("_"," ",dat$normalized_data_type)),
    # set zeroline to false otherwise there would be a big horizontal line at y = 0
    zeroline = FALSE
  )
  # formatting for x axis
  x <- list(
    title = paste0("Concentration ","(",dat$conc_unit,")"),
    # set zeroline to false so there is no vertical line at x = 0
    type = "log",
    zeroline = FALSE,
    dtick=1
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
  coff <- ifelse(dat$max_med >= 0, dat$coff, dat$coff * -1)
  fig <- fig %>% add_trace(
    data = tibble(x = x_range, y = coff),
    x = ~x,
    y = ~y,
    type = "scatter",
    mode = "lines",
    name = "cutoff",
    line = list(dash = "dash", width = 1.5, color = NULL),
    inherit = FALSE,
    hoverinfo = "text",
    text = ~ paste(
      "</br>", paste0("Cut Off (", specify_decimal(coff,2), ")")
    )
  )
  
  if (lvl == 2) {
    # # add max median annotation
    fig <- fig %>% add_trace(
      data = tibble(x = x_range, y = dat$max_med),
      x = ~x,
      y = ~y,
      type = "scatter",
      mode = "lines",
      name = "max median",
      line = list(dash = "dash", width = 1.5, color = NULL),
      inherit = FALSE,
      hoverinfo = "text",
      text = ~ paste(
        "</br>", paste0("Max Median (", specify_decimal(dat$max_med,2), ")")
      )
    )
  }
  
  # currently only support for model types 1 and 0 but need to expand or make this generic
  if (!lvl == 2 && dat$fitc == 100) {
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
    if (!lvl == 2 && !dat$modl == "cnst" && !dat$modl == "none") {
      dat_lines <- vline(ac50s %>% filter(model == dat$modl) %>% pull(ac50) %>% as.numeric())
      fig <- fig %>% plotly::layout(xaxis = x, yaxis = y, shapes = dat_lines)
    } else {
      fig <- fig %>% plotly::layout(xaxis = x, yaxis = y)
  }
    
    
    # add ac50 line for appropriate models (hitc=1)
    if (!lvl == 2 && !dat$modl == "cnst" && !dat$modl == "none") {
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
    
    if (!lvl == 2 && !dat$modl == "none"){
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
    }
    # get hitcall
    hitcall <- dat %>% pull(hitc)
    
    # add annotations
    fig <- fig %>% add_annotations(
      text = paste0(
        dat %>% pull(.data$aenm), "<br>",
        case_when(
          #updated binary hitcall designation to three decimal rounding
          #hitcall == 1 ~ "ACTIVE",
          #hitcall == 0 ~ "INACTIVE",
          #hitcall == -1 ~ "NO CALL",
          TRUE ~ paste0("HITC: ", paste0(trimws(format(round(dat$hitc, 3), nsmall = 3))))
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


#' tcplggplot
#' 
#' @param dat data table with all required conc/resp data
#' @param lvl integer level of data that should be plotted
#' level 2 - for 'sc' plotting
#' level 5 - for 'mc' plotting, all fit models and winning model with hitcall
#' @param verbose boolean should plotting include table of values next to the plot
#' @param flags boolean should plotting include level 6 flags in plot caption
#' @param yrange Integer of length 2, for overriding the y-axis range, c(<min>,<max>). 
#' By default, c(NA,NA).
#'
#' @return A ggplot object or grob with accompanied table depending on verbose option
#' @importFrom dplyr %>% filter group_by summarise left_join inner_join select rowwise mutate pull mutate_if
#' @importFrom dplyr tibble contains everything as_tibble arrange .data
#' @importFrom ggplot2 ggplot aes geom_function geom_vline geom_hline geom_point scale_x_continuous scale_y_continuous scale_color_viridis_d
#' @importFrom ggplot2 guide_legend scale_linetype_manual xlab ylab geom_text labs theme element_blank
#' @importFrom ggplot2 margin unit element_text geom_segment
#' @import gridExtra
#' @import stringr
tcplggplot <- function(dat, lvl = 5, verbose = FALSE, flags = FALSE, yrange = c(NA,NA)) {
  # variable binding
  conc <- resp <- xpos <- ypos <- hjustvar <- vjustvar <- NULL
  annotateText <- name <- aic <- NULL
  l3_dat <- tibble(conc = unlist(dat$conc), resp = unlist(dat$resp), max_med = dat$max_med)
  l3_range <- l3_dat %>%
    pull(.data$conc) %>%
    range()
  
  # check if model_type is 3 or 4, which means an override method was assigned
  if (dat$model_type == 3) { # gain direction
    # leave coff but bmr should flip if top is negative
    if (!is.null(dat$top) && !is.na(dat$top) && !is.null(dat$bmr)) {
      if (dat$top < 0) {
        dat$bmr <- dat$bmr * -1
      }
    }
  } else if (dat$model_type == 4) { # loss direction
    # coff and bmr(if top < 0) should be negative
    if (!is.null(dat$top) && !is.null(dat$coff) && !is.na(dat$top) && !is.null(dat$bmr)) {
      dat$coff <- dat$coff * -1
      if (dat$top < 0) {
        dat$bmr <- dat$bmr * -1
      }
    }
  } else { # bidirectional
    # check if winning model has negative top.  If so coff,bmr should be negative
    if (!is.null(dat$top) && !is.null(dat$coff) && !is.na(dat$top) && !is.null(dat$bmr)) {
      if (dat$top < 0) {
        dat$coff <- dat$coff * -1
        dat$bmr <- dat$bmr * -1
      }
    }
  }

  # check if data is outside bounds of yrange. If so, expand yrange bounds
  if (!identical(yrange, c(NA,NA))) {
    yrange[1] <- min(dat$resp_min, dat$coff, yrange[1], unlist(dat$resp))
    yrange[2] <- max(dat$resp_max, dat$coff, yrange[2], unlist(dat$resp))
  }

  winning_model_string <- paste0("Winning Model\n(", dat$modl, ")")
  model_test <- function(modeltype) {
    ifelse(dat$modl == modeltype, winning_model_string, "Losing Models")
  }
  
  flag_count <- 0
  if (flags && dat$flag != "None") {
    flag_count <- str_count(dat$flag, "\n") + 1
  }
  

  if (lvl == 2) {
    gg <- ggplot(l3_dat, aes(x = conc)) +
      geom_hline(aes(yintercept = dat$max_med, linetype = "Max Median"), color="red") +
      geom_hline(aes(yintercept = ifelse(dat$max_med >= 0, dat$coff, dat$coff * -1), linetype="Cutoff"), color="blue") +
      geom_point(aes(y = resp)) +
      scale_x_continuous(limits = l3_range, trans = "log10") +
      scale_y_continuous(limits = yrange) +
      scale_linetype_manual("", 
                            guide = guide_legend(override.aes = list(color = c("blue", "red"))), 
                            values = c(2, 2)) +
      xlab(paste0("Concentration ", "(", dat$conc_unit, ")")) +
      ylab(stringr::str_to_title(gsub("_", " ", dat$normalized_data_type))) +
      labs(
        title = paste0(
          stringr::str_trunc(paste0(
            dat %>% pull(.data$dsstox_substance_id), " ",
            dat %>% pull(.data$chnm)
          ), 75), "\n",
          stringr::str_trunc(paste0(
            "AEID:", dat %>% pull(.data$aeid), "  ",
            "AENM:", dat %>% pull(.data$aenm)), 70),"\n",
          "SPID:", dat %>% pull(.data$spid), "  ",
          "S2ID:", dat %>% pull(.data$s2id), "  ",
          ifelse(verbose, "", paste0(
            "HITC:", paste0(trimws(format(round(dat$hitc, 3), nsmall = 3)))
          ))
        )
      ) +
      theme(
        plot.title = element_text(size = 12),
        legend.title = element_blank(),
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm")
      )
  } else {
    gg <- ggplot(l3_dat, aes(conc, resp)) +
      geom_function(aes(color = !!model_test("gnls"), linetype = !!model_test("gnls")), fun = function(x) tcplfit2::gnls(ps = c(dat$gnls_tp, dat$gnls_ga, dat$gnls_p, dat$gnls_la, dat$gnls_q), x = x)) +
      geom_function(aes(color = !!model_test("exp2"), linetype = !!model_test("exp2")), fun = function(x) tcplfit2::exp2(ps = c(dat$exp2_a, dat$exp2_b), x = x)) +
      geom_function(aes(color = !!model_test("exp3"), linetype = !!model_test("exp3")), fun = function(x) tcplfit2::exp3(ps = c(dat$exp3_a, dat$exp3_b, dat$exp3_p), x = x)) +
      geom_function(aes(color = !!model_test("exp4"), linetype = !!model_test("exp4")), fun = function(x) tcplfit2::exp4(ps = c(dat$exp4_tp, dat$exp4_ga), x = x)) +
      geom_function(aes(color = !!model_test("exp5"), linetype = !!model_test("exp5")), fun = function(x) tcplfit2::exp5(ps = c(dat$exp5_tp, dat$exp5_ga, dat$exp5_p), x = x)) +
      geom_function(aes(color = !!model_test("poly1"), linetype = !!model_test("poly1")), fun = function(x) tcplfit2::poly1(ps = c(dat$poly1_a), x = x)) +
      geom_function(aes(color = !!model_test("poly2"), linetype = !!model_test("poly2")), fun = function(x) tcplfit2::poly2(ps = c(dat$poly2_a, dat$poly2_b), x = x)) +
      geom_function(aes(color = !!model_test("pow"), linetype = !!model_test("pow")), fun = function(x) tcplfit2::pow(ps = c(dat$pow_a, dat$pow_p), x = x)) +
      geom_function(aes(color = !!model_test("hill"), linetype = !!model_test("hill")), fun = function(x) tcplfit2::hillfn(ps = c(dat$hill_tp, dat$hill_ga, dat$hill_p), x = x)) +
      geom_vline(aes(xintercept = dat$ac50, color = "AC50", linetype = "AC50")) +
      geom_hline(aes(yintercept = dat$coff, color = "Cutoff", linetype = "Cutoff")) +
      geom_point() +
      scale_x_continuous(limits = l3_range, trans = "log10") +
      scale_y_continuous(limits = yrange) +
      scale_color_viridis_d("", direction = -1, guide = guide_legend(reverse = TRUE, order = 2), end = 0.9) +
      scale_linetype_manual("", guide = guide_legend(reverse = TRUE, order = 2), values = c(2, 2, 2, 3, 1)) +
      xlab(paste0("Concentration ", "(", dat$conc_unit, ")")) +
      ylab(stringr::str_to_title(gsub("_", " ", dat$normalized_data_type))) +
      labs(
        title = paste0(
          stringr::str_trunc(paste0(
            dat %>% pull(.data$dsstox_substance_id), " ",
            dat %>% pull(.data$chnm)
          ), 75), "\n",
          stringr::str_trunc(paste0(
            "SPID:", dat %>% pull(.data$spid), "  ",
            "AEID:", dat %>% pull(.data$aeid), "  ",
            "AENM:", dat %>% pull(.data$aenm)), 70),"\n",
          "M4ID:", dat %>% pull(.data$m4id), "  ",
          ifelse(verbose, "", paste0(
            "\nHITC:", paste0(trimws(format(round(dat$hitc, 3), nsmall = 3)))
          ))
        ),
        caption = ifelse(flags, paste0(
          "\nFlags(", flag_count, "): ", paste0(trimws(format(dat$flag, nsmall = 3)))
        ), "")
      ) +
      theme(
        plot.title = element_text(size = 12),
        plot.caption = element_text(hjust = 0, margin = margin(-1,0,1,0)),
        axis.title.x = element_text(margin = margin(3,0,-5,0)),
        legend.title = element_blank(),
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm")
      )
  }
  if (!is.null(dat$bmd) && !is.null(dat$bmr)){ gg = gg + 
    geom_segment(aes(x=dat$bmd, xend=dat$bmd, y=-Inf, yend=dat$bmr, color = "BMD", linetype = "BMD")) + 
    geom_segment(x=-Inf, aes(xend=dat$bmd, y = dat$bmr, yend=dat$bmr, color = "BMD", linetype = "BMD"))
  }

  p <- lapply(dat %>% select(contains("aic")) %>% colnames() %>% stringr::str_extract("[:alnum:]+"), function(x) {
    dat %>%
      select(contains(paste0(x, c("_aic", "_rme")))) %>%
      tidyr::pivot_longer(cols = everything()) %>%
      as_tibble()
  })

  # general function to round/shorten values for plotting tables
  round_n <- function(x, n=3) {
    if (!is.na(x)) {
      if (x >= 1000 | (x<=0.0005 & x != 0)) {
        # if x>=1000, convert value to scientific notation
        formatC(x, format = "e", digits = 1)
      } else { # else, round the value to 3 decimal places
        format(round(x, n), nsmall = 3)
      }
    } else {
      return(NA)
    }
  }
  round_n <- Vectorize(round_n)
  
  combined_p <- data.table::rbindlist(p)
  pivoted_p <- combined_p
  t <- NULL
  if (lvl != 2) {
    l5_details <- tibble(Hitcall = dat$hitc, BMD = dat$bmd, AC50 = dat$ac50)
    l5_details <- l5_details %>% mutate_if(is.numeric, ~ round_n(., 3))
    l5_t <- tableGrob(l5_details, rows = NULL)
    pivoted_p <- combined_p %>%
      tidyr::extract(name, c("model", "param"), "([[:alnum:]]+)_([[:alnum:]]+)") %>%
      pivot_wider(names_from = "param", values_from = "value")
    pivoted_p <- pivoted_p %>% mutate_if(is.numeric, ~ round_n(., 3))
    pivoted_p <- pivoted_p %>% arrange(as.numeric(aic))
    t <- tableGrob(pivoted_p, rows = NULL)
    valigned <- gtable_combine(l5_t, t, along = 2)
  } else {
    l5_details <- tibble(Hitcall = dat$hitc)
    l5_details <- l5_details %>% mutate_if(is.numeric, ~ round_n(., 3))
    l5_t <- tableGrob(l5_details, rows = NULL)
    valigned <- gtable_combine(l5_t, along = 2)
  }

  if (lvl == 2) {
    ifelse(verbose,
           return(arrangeGrob(gg, valigned, ncol = 1, heights = c(4,1))),
           return(gg)
    )
  } else {
    ifelse(verbose,
           return(arrangeGrob(gg, valigned, nrow = 1, widths = 2:1)),
           return(gg)
    )
  }
}
