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
#' @param compare.val List, vectors of values for each field to query on to 
#' compare with val. Must be in the same order as 'fld'. Must have the same
#' length as val (1:1 comparison). Must be set to compare plots; otherwise leave
#' NULL
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
tcplPlot <- function(dat = NULL, type = "mc", fld = "m4id", val = NULL, compare.val = NULL, by = NULL, output = c("console", "pdf", "png", "jpg", "svg", "tiff"), fileprefix = paste0("tcplPlot_", Sys.Date()), multi = NULL, verbose = FALSE, nrow = NULL, ncol = NULL, dpi = 600, flags = FALSE, yuniform = FALSE, yrange=c(NA,NA)) {
  #variable binding
  conc_unit <- bmd <- resp <- compare.dat <- NULL
  
  # Validate vars based on some assumed properties
  validated_vars <- tcplPlotValidate(type = type,flags = flags,output = output,multi = multi,verbose = verbose)
  # take list of validated vars and add them to the function's environment
  list2env(validated_vars, envir = environment())

  # check_tcpl_db_schema is a user-defined function found in v3_schema_functions.R file
  if (check_tcpl_db_schema() | !is.null(dat)) {
    # check if user supplied data.  If not, load from db connection
    if(is.null(dat)){
      dat <- tcplPlotLoadData(lvl = lvl, fld = fld, val = val, type = type,flags = flags, compare = FALSE) #code defined in tcplPlotUtils.R
    } else {
      # if user supplies dat we still need to add compare indicator
      dat <- dat[,compare := FALSE]
    }
    if(!is.null(compare.val)){
      # check that val and compare.val are the same length 
      if (!is.null(compare.val) && length(unlist(val)) != length(unlist(compare.val))) stop("'compare.val' must be of equal length to 'val'")
      
      compare.dat <- tcplPlotLoadData(lvl = lvl, fld = fld, val = compare.val, type = type,flags = flags, compare = TRUE) #code defined in tcplPlotUtils.R
      if (nrow(compare.dat) == 0) stop("No compare data for fld/val provided")
    }
    
    
    # check for null bmd in dat table
    if (verbose){
      dat <- dat[is.null(dat$bmd), bmd:=NA]
    }
    
    
  
    # join with given val/compare.val if lengths don't match
    if (!is.null(compare.val) && nrow(dat) + nrow(compare.dat) != length(val) + length(compare.val)) {
      val_dt <- as.data.table(val)
      colnames(val_dt) <- "m4id"
      compare.val_dt <- as.data.table(compare.val)
      colnames(compare.val_dt) <- "m4id"
      dat <- val_dt %>% inner_join(dat, by = "m4id")
      compare.dat <- compare.val_dt %>% inner_join(compare.dat, by = "m4id")
      dat <- rbind(dat, compare.dat, fill = TRUE)
    } else {
      # preserve user-given order
      setorder(dat, order)
    }
    
    # set yrange from tcplPlotUtils.R
    yrange <- tcplPlotSetYRange(dat,yuniform,yrange)
    
    # if you have compare data, join it back to main datatable
    if(!is.null(compare.dat)){
    dat <- rbind(dat,compare.dat, fill = TRUE)
    }
    
    
    # assign nrow = ncol = 1 for output="pdf" and multi=FALSE to plot one plot per page
    if(nrow(dat) > 1 && output == "pdf" && multi == FALSE) {
      nrow = ncol = 1
    }
    # error message for output="console" and multi=FALSE to avoid multiple plots in console
    if(nrow(dat) > 1 && output == "console" && multi == FALSE) stop("More than 1 concentration series returned for given field/val combination.  Set output to pdf or reduce the number of curves to 1. Current number of curves: ", nrow(input))
    if(is.null(nrow)){
      nrow <- ifelse(verbose,2,2)
    }
    if(is.null(ncol)){
      ncol <- ifelse(!verbose | type == "sc",3,2)
    }
    
    
    
    
    if (nrow(dat) == 1) {
      # plot single graph
      # this needs to be fixed to be more succinct about users selected option
      ifelse(output[1] == "console",
      # tcplPlotlyplot is the user-defined function found in tcplPlot.R file used to connect tcpl and plotly packages
      # tcplggplot is the user-defined function found in tcplPlot.R file used to connect tcpl and ggplot2 packages
        return(tcplPlotlyPlot(dat, lvl)),
        return(ggsave(filename=paste0(fileprefix,"_",paste0(dat$m4id, collapse = "_"),".",output),
                      plot= if(is.null(compare.val)) tcplggplot(dat,verbose = verbose, lvl = lvl, flags = flags, yrange = yrange) else tcplggplotCompare(dat[compare == FALSE],dat[compare == TRUE],verbose = verbose, lvl = lvl, flags = flags, yrange = yrange), width = 7, height = 5, dpi=dpi))
      )
    } else {
      split_dat <- list(dat)
      if(!is.null(by)){
        split_dat <- split(dat,f = factor(dat %>% pull(all_of(by))))
      }
      for(d in split_dat){
        if (is.null(compare.val)) {
          plot_list <- by(d,seq(nrow(d)),tcplggplot,verbose = verbose, lvl = lvl, flags = flags, yrange = yrange)
        } else {
          plot_list <- mapply(tcplggplotCompare, asplit(d[compare == FALSE],1), asplit(d[compare == TRUE],1), MoreArgs = list(verbose = verbose, lvl = lvl, flags = flags, yrange = yrange))
        }
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
    tcplLegacyPlot()
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
  
  compare.dat <- dat[compare == TRUE]
  dat <- dat[compare == FALSE]
  
  l3_dat_main <- tibble(conc = unlist(dat$conc), resp = unlist(dat$resp), max_med = dat$max_med, l3 = "response A")
  l3_dat_compare <- tibble(conc = unlist(compare.dat$conc), resp = unlist(compare.dat$resp), max_med = compare.dat$max_med, l3 = "response B")
  l3_dat_both <- rbind(l3_dat_main, l3_dat_compare)
  
  # extract range from level 3 data for creating plotting all the functions
  # increase resolution to get smoother curves
  resolution <- 100
  x_min_max <- range(l3_dat_both$conc)
  #if the overall minimum conc is greater than 0 (test wells)
  if (x_min_max[1] > 0) {
    hline_range <- 10^(seq(from = log10(x_min_max[1]/100), to = log10(x_min_max[2]*100), length.out = resolution))
    x_range <- 10^(seq(from = log10(x_min_max[1]), to = log10(x_min_max[2]), length.out = resolution))
    use_log <- TRUE
  } else if (x_min_max[2] <= 0) { #if the overall max conc is less than 0 (all concs are likely 0) -- create bounds
    x_range <- hline_range <- seq(from = -10, to = 10, length.out = resolution)
    use_log <- FALSE
  } else { #if the minimum conc is 0 but max is greater than 0 -- can't use log scale
    hline_range <- seq(from = x_min_max[1]/100, to = x_min_max[2]*100, length.out = resolution)
    x_range <- seq(from = x_min_max[1], to = x_min_max[2], length.out = resolution)
    use_log <- FALSE
  }
  
  #check if winning model = none 
  if (!lvl == 2 && !dat$modl == "none"){
  
    # main data
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
    
    # compare data
    if (nrow(compare.dat) > 0) {
      # check if model_type is 3 or 4, which means an override method was assigned
      if (compare.dat$model_type == 3) { # gain direction
        # leave coff but bmr should flip if top is negative
        if (!is.null(compare.dat$top) && !is.na(compare.dat$top) && !is.null(compare.dat$bmr)) {
          if (compare.dat$top < 0) {
            compare.dat$bmr <- compare.dat$bmr * -1
          }
        }
      } else if (compare.dat$model_type == 4) { # loss direction
        # coff and bmr(if top < 0) should be negative
        if (!is.null(compare.dat$top) && !is.null(compare.dat$coff) && !is.na(compare.dat$top) && !is.null(compare.dat$bmr)) {
          compare.dat$coff <- compare.dat$coff * -1
          if (compare.dat$top < 0) {
            compare.dat$bmr <- compare.dat$bmr * -1
          }
        }
      } else { # bidirectional
        # check if winning model has negative top.  If so coff,bmr should be negative
        if (!is.null(compare.dat$top) && !is.null(compare.dat$coff) && !is.na(compare.dat$top) && !is.null(compare.dat$bmr)) {
          if (compare.dat$top < 0) {
            compare.dat$coff <- compare.dat$coff * -1
            compare.dat$bmr <- compare.dat$bmr * -1
          }
        }
      }
    }
    
    #get models from columns that have an ac50 listed
    models <- gsub("_ac50","",colnames(dat)[grepl("_ac50",colnames(dat))])
    ac50s <- tibble(model = models, ac50 = dat %>% select(colnames(dat)[grepl("_ac50",colnames(dat))]) %>% unlist)
    #don't need loss direction ac50s
    ac50s <- ac50s %>% filter(!grepl("_loss",model))
    models <- models[!grepl("_loss",models)]
    
    
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
      l3_resp <- l3_dat_main %>%
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
    
    if (nrow(compare.dat) > 0) {
      ac50s_compare <- tibble(model = models, ac50 = compare.dat %>% select(colnames(compare.dat)[grepl("_ac50",colnames(compare.dat))]) %>% unlist)
      ac50s_compare <- ac50s_compare %>% filter(!grepl("_loss",model))
      
      # calculate y values for each function 
      if ("hill" %in% models) y_hill_compare <- tcplfit2::hillfn(ps = c(compare.dat$hill_tp,compare.dat$hill_ga,compare.dat$hill_p), x = x_range)
      #tp = ps[1], ga = ps[2], p = ps[3], la = ps[4], q = ps[5]
      if ("gnls" %in% models) y_gnls_compare <- tcplfit2::gnls(ps = c(compare.dat$gnls_tp,compare.dat$gnls_ga,compare.dat$gnls_p,compare.dat$gnls_la,compare.dat$gnls_q),x = x_range)
      #a = ps[1], b = ps[2]
      if ("exp2" %in% models) y_exp2_compare <- tcplfit2::exp2(ps = c(compare.dat$exp2_a,compare.dat$exp2_b), x = x_range)
      #a = ps[1], b = ps[2], p = ps[3]
      if ("exp3" %in% models) y_exp3_compare <- tcplfit2::exp3(ps = c(compare.dat$exp3_a,compare.dat$exp3_b,compare.dat$exp3_p), x = x_range)
      #tp = ps[1], ga = ps[2]
      if ("exp4" %in% models) y_exp4_compare <- tcplfit2::exp4(ps = c(compare.dat$exp4_tp,compare.dat$exp4_ga), x = x_range)
      #tp = ps[1], ga = ps[2], p = ps[3]
      if ("exp5" %in% models) y_exp5_compare <- tcplfit2::exp5(ps = c(compare.dat$exp5_tp,compare.dat$exp5_ga,compare.dat$exp5_p), x = x_range)
      #a = ps[1]
      if ("poly1" %in% models) y_poly1_compare <- tcplfit2::poly1(ps = c(compare.dat$poly1_a), x = x_range)
      #a = ps[1], b = ps[2]
      if ("poly2" %in% models) y_poly2_compare <- tcplfit2::poly2(ps = c(compare.dat$poly2_a,compare.dat$poly2_b), x = x_range)
      #a = ps[1], p = ps[2]
      if ("pow" %in% models) y_pow_compare <- tcplfit2::pow(ps = c(compare.dat$pow_a,compare.dat$pow_p), x = x_range)
      
      if (compare.dat$fitc == 100) {
        # loec is stored as modl_acc
        x_loec_compare <- rep(compare.dat$modl_acc, resolution)
        l3_resp_compare <- l3_dat_compare %>%
          pull(.data$resp) %>%
          range()
        y_loec_compare <- seq(from = l3_resp[1], to = l3_resp[2], length.out = resolution)
      }
      
      y_cnst_compare <- x_range * 0
      ac50s_compare <- ac50s_compare %>% rbind(c(model = "cnst", ac50 = NA))
      model_stats_compare <- compare.dat %>%
        select(ends_with("aic"), ends_with("rme"), ends_with("_top"), ends_with("_p")) %>%
        tidyr::pivot_longer(everything(),
                            names_to = c("model", "param"),
                            names_pattern = "(.*)_(.*)"
        ) %>%
        tidyr::pivot_wider(names_from = param, values_from = value)
      ac50s_compare$ac50 <- as.numeric(ac50s_compare$ac50)
      opacity.compare <- tibble(model = models, opacity = op) %>% mutate(opacity = ifelse(.data$model == compare.dat$modl, 1, opacity))
      line.fmt.compare <- tibble(model = models, dash = "dash") %>% mutate(dash = ifelse(.data$model == compare.dat$modl, "solid", .data$dash))
      m_compare <- opacity.compare %>%
        inner_join(line.fmt.compare, by = "model") %>%
        inner_join(ac50s_compare, by = "model") %>%
        rowwise() %>%
        mutate(x = ifelse(compare.dat$fitc == 100,list(x_loec_compare),list(x_range)), y = list(get(paste0("y_", .data$model, "_compare")))) %>%
        tidyr::unnest(cols = c(x, y))
      # if we have model stats we want them included in the hoverover
      if (!is.null(model_stats_compare)) {
        m_compare <- m_compare %>% inner_join(model_stats_compare, by = "model")
      }
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
    data = if (nrow(compare.dat) > 0) l3_dat_both else l3_dat_main,
    x = ~conc,
    y = ~resp, 
    color = ~l3,
    colors = ifelse(nrow(compare.dat) > 0, "Set1", "red"),
    opacity = ifelse(nrow(compare.dat) > 0, 0.67, 1),
    type = "scatter",
    mode = "markers",
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
    type = ifelse(use_log, "log", "linear"),
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
  
  # cutoff for A
  fig <- fig %>% add_trace(
    data = tibble(x = hline_range, y = dat$coff),
    x = ~x,
    y = ~y,
    type = "scatter",
    mode = "lines",
    name = "cutoff A",
    opacity = ifelse(nrow(compare.dat) > 0, 0.5, 1),
    line = list(dash = "dash", width = 1.5, color = ifelse(nrow(compare.dat) > 0, "red", "blue")),
    inherit = FALSE,
    hoverinfo = "text",
    text = ~ paste(
      "</br>", paste0("Cut Off A (", specify_decimal(dat$coff,2), ")")
    )
  )
  
  if (nrow(compare.dat) > 0) {
    # cutoff for B
    fig <- fig %>% add_trace(
      data = tibble(x = hline_range, y = compare.dat$coff),
      x = ~x,
      y = ~y,
      type = "scatter",
      mode = "lines",
      name = "cutoff B",
      opacity = ifelse(nrow(compare.dat) > 0, 0.5, 1),
      line = list(dash = "dash", width = 1.5, color = "blue"),
      inherit = FALSE,
      hoverinfo = "text",
      text = ~ paste(
        "</br>", paste0("Cut Off B (", specify_decimal(dat$coff,2), ")")
      )
    )
  }
  
  if (lvl == 2) {
    # # add max median annotation
    fig <- fig %>% add_trace(
      data = tibble(x = hline_range, y = dat$max_med),
      x = ~x,
      y = ~y,
      type = "scatter",
      mode = "lines",
      name = "max median A",
      opacity = ifelse(nrow(compare.dat) > 0, 0.5, 1),
      line = list(dash = ifelse(nrow(compare.dat) > 0, "solid", "dash"), width = 1.5, color = "red"),
      inherit = FALSE,
      hoverinfo = "text",
      text = ~ paste(
        "</br>", paste0("Max Median A(", specify_decimal(dat$max_med,2), ")")
      )
    )
  }
  
  if (lvl == 2 && nrow(compare.dat) > 0) {
    # # add max median annotation
    fig <- fig %>% add_trace(
      data = tibble(x = hline_range, y = compare.dat$max_med),
      x = ~x,
      y = ~y,
      type = "scatter",
      mode = "lines",
      name = "max median B",
      opacity = ifelse(nrow(compare.dat) > 0, 0.5, 1),
      line = list(dash = "solid", width = 1.5, color = "blue"),
      inherit = FALSE,
      hoverinfo = "text",
      text = ~ paste(
        "</br>", paste0("Max Median B(", specify_decimal(compare.dat$max_med,2), ")")
      )
    )
  }
  
  compare.fitc <- compare.dat$fitc
  if (nrow(compare.dat) == 0) compare.fitc = -1
  
  # currently only support for model types 1 and 0 but need to expand or make this generic
  if (!lvl == 2 && (dat$fitc == 100 || compare.fitc == 100)) {
    # apply axis and lines to figure
    fig <- fig %>% plotly::layout(xaxis = x, yaxis = y)
    if (dat$fitc == 100) {
      # add the loec line if hitc == 1.
      fig <- fig %>% add_trace(
        data = tibble(x = x_loec, y = y_loec),
        x = ~x,
        y = ~y,
        name = "LOEC A",
        type = "scatter",
        mode = "lines",
        line = list(dash = "solid", width = 1.5, color = "red"),
        inherit = FALSE,
        hoverinfo = "text",
        text = ~ paste(
          "</br>", "LOEC A",
          "</br> Log Concentration: ", x
        )
      )
    }
    if (compare.dat$fitc == 100) {
      # add the loec line if hitc == 1.
      fig <- fig %>% add_trace(
        data = tibble(x = x_loec_compare, y = y_loec_compare),
        x = ~x,
        y = ~y,
        name = "LOEC B",
        type = "scatter",
        mode = "lines",
        line = list(dash = "solid", width = 1.5, color = "blue"),
        inherit = FALSE,
        hoverinfo = "text",
        text = ~ paste(
          "</br>", "LOEC B",
          "</br> Log Concentration: ", x
        )
      )
    }
  } else {
    if (!lvl == 2 && !dat$modl == "cnst" && !dat$modl == "none") {
      dat_lines <- vline(ac50s %>% filter(model == dat$modl) %>% pull(ac50) %>% as.numeric(), color = ifelse(nrow(compare.dat) > 0, "red", "black"))
      dat_lines_compare <- NULL
      if (nrow(compare.dat) > 0) dat_lines_compare <- vline(ac50s_compare %>% filter(model == compare.dat$modl) %>% pull(ac50) %>% as.numeric(), color = "blue")
      fig <- fig %>% plotly::layout(xaxis = x, yaxis = y, shapes = list(dat_lines, dat_lines_compare))
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
        text = paste0("A Winning Model Log AC50 (", specify_decimal(ac50s %>% filter(model == dat$modl) %>% pull(ac50) %>% as.numeric(),2), ")"),
        showarrow = F,
        textangle = 90,
        xanchor = "left"
      )
      if (nrow(compare.dat) > 0) {
        fig <- fig %>% add_annotations(
          yref = "paper",
          xref = "x",
          x = ac50s_compare %>% filter(model == compare.dat$modl) %>% pull(ac50) %>% as.numeric() %>% log10(),
          y = 1,
          text = paste0("B Winning Model Log AC50 (", specify_decimal(ac50s_compare %>% filter(model == compare.dat$modl) %>% pull(ac50) %>% as.numeric(),2), ")"),
          showarrow = F,
          textangle = 90,
          xanchor = "left"
        )
      }
      
    }
    
    if (!lvl == 2 && !dat$modl == "none"){
      if (nrow(compare.dat) == 0) {
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
          name = paste0("model A(", dat$modl, ")"),
          split = ~model,
          opacity = ~opacity,
          line = list(dash = ~dash, width = 1.5, color = "red"),
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
      } else {
        # add line for winning model
        fig <- fig %>% add_trace(
          data = m %>% filter(.data$model == dat$modl),
          x = ~x,
          y = ~y,
          type = "scatter",
          mode = "lines",
          name = paste0("model A(", dat$modl, ")"),
          split = ~model,
          opacity = ~opacity,
          line = list(dash = ~dash, width = 1.5, color = "red"),
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
        # add line for winning model for compare data
        fig <- fig %>% add_trace(
          data = m_compare %>% filter(.data$model == compare.dat$modl),
          x = ~x,
          y = ~y,
          type = "scatter",
          mode = "lines",
          name = paste0("model B(", compare.dat$modl, ")"),
          split = ~model,
          opacity = ~opacity,
          line = list(dash = ~dash, width = 1.5, color = "blue"),
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
    }
    
    if (nrow(compare.dat) > 0) {
      identical_title <- paste0(stringr::str_trunc(paste0(
        ifelse(dat$dsstox_substance_id == compare.dat$dsstox_substance_id, paste0(dat$dsstox_substance_id, " "), ""),
        ifelse(dat$chnm == compare.dat$chnm, paste0(dat$chnm, "\n"), "")
      ), 75),
      stringr::str_trunc(paste0(
        ifelse(dat$spid == compare.dat$spid, paste0("SPID:", dat$spid, "  "), ""),
        ifelse(dat$aeid == compare.dat$aeid, paste0("AEID:", dat$aeid, "  "), ""),
        ifelse(dat$aenm == compare.dat$aenm, paste0("AENM:", dat$aenm), "")), 70))
      if (identical_title != "" & !endsWith(identical_title, "\n")) {
        identical_title <- paste0(identical_title, "\n")
      }
    }
    
    # add annotations
    fig <- fig %>% add_annotations(
      text = ifelse(nrow(compare.dat) > 0, 
                    # compare
                    paste0(
                      ifelse(identical_title != "", paste0(identical_title, "\n"), ""),
                      paste0(
                        "A: ",
                        ifelse(dat$dsstox_substance_id != compare.dat$dsstox_substance_id, paste0(dat$dsstox_substance_id, " "), ""),
                        ifelse(dat$chnm != compare.dat$chnm, paste0(dat$chnm, "\n"), "")
                      ), 
                      paste0(
                        ifelse(dat$spid != compare.dat$spid, paste0("SPID:", dat$spid, "  "), ""),
                        ifelse(dat$aeid != compare.dat$aeid, paste0("AEID:", dat$aeid, "  "), ""),
                        ifelse(dat$aenm != compare.dat$aenm, paste0("AENM:", dat$aenm, "\n"), "")),
                      ifelse(lvl != 2, paste0("M4ID:", dat$m4id),paste0("S2ID:", dat$s2id)), "  ",
                      paste0(
                        "\nHITC:", paste0(trimws(format(round(dat$hitc, 3), nsmall = 3)))
                      ),
                      ifelse(!is.null(dat$flag), gsub("\\|\\|", "<br>", paste0("\nFlags: ", dat %>% pull(.data$flag))), ""),
                      paste0("\n\n",
                        "B: ",
                        ifelse(dat$dsstox_substance_id != compare.dat$dsstox_substance_id, paste0(compare.dat$dsstox_substance_id, " "), ""),
                        ifelse(dat$chnm != compare.dat$chnm, paste0(compare.dat$chnm, "\n"), "")
                      ), 
                      paste0(
                        ifelse(dat$spid != compare.dat$spid, paste0("SPID:", compare.dat$spid, "  "), ""),
                        ifelse(dat$aeid != compare.dat$aeid, paste0("AEID:", compare.dat$aeid, "  "), ""),
                        ifelse(dat$aenm != compare.dat$aenm, paste0("AENM:", compare.dat$aenm, "\n"), "")),
                      ifelse(lvl != 2, paste0("M4ID:", compare.dat$m4id),paste0("S2ID:", compare.dat$s2id)), "  ",
                      paste0(
                        "\nHITC:", paste0(trimws(format(round(compare.dat$hitc, 3), nsmall = 3)))
                      ),
                      ifelse(!is.null(compare.dat$flag), gsub("\\|\\|", "<br>", paste0("\nFlags: ", compare.dat %>% pull(.data$flag))), "")
                    ),
                    # no compare
                    paste0(
                      dat %>% pull(.data$aenm), "<br>",
                      case_when(
                        TRUE ~ paste0("HITC: ", paste0(trimws(format(round(dat$hitc, 3), nsmall = 3))))
                      ), "<br>",
                      dat %>% pull(.data$chnm), " (", dat %>% pull(.data$casn), ")", "<br>",
                      dat %>% pull(.data$dsstox_substance_id), "<br>",
                      dat %>% pull(.data$spid), "<br>",
                      ifelse(lvl != 2, paste0("M4ID:", dat$m4id),paste0("S2ID:", dat$s2id)), "<br>",
                      ifelse(!is.null(dat$flag), gsub("\\|\\|", "<br>", paste0("Flags: ", dat %>% pull(.data$flag))), "")
                    )
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
  if (lvl == 5 && dat$model_type == 3) { # gain direction
    # leave coff but bmr should flip if top is negative
    if (!is.null(dat$top) && !is.na(dat$top) && !is.null(dat$bmr)) {
      if (dat$top < 0) {
        dat$bmr <- dat$bmr * -1
      }
    }
  } else if (lvl == 5 && dat$model_type == 4) { # loss direction
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
  
  # check if ac50 is null and assign NA if it is 
  dat$ac50 <- ifelse(is.null(dat$ac50), NA, dat$ac50)
  
  # check if dtxsid is NA, pull wllt in from lvl 3
  if (is.na(dat$dsstox_substance_id) | is.na(dat$chnm)) {
    wllt <- unique(tcplLoadData(lvl = 0, fld = list("spid","acid"), 
                                list(dat$spid, tcplLoadAcid(fld = "aeid", val = dat$aeid)$acid))$wllt)
    if (length(wllt) == 1) {
      if (wllt == 'c' | wllt == 'p') {
        dat$dsstox_substance_id <- "Gain-of-signal control"
        dat$chnm <- ""
      }
      else if (wllt == 'm' | wllt == 'o') {
        dat$dsstox_substance_id <- "Loss-of-signal control"
        dat$chnm <- ""
      }
      else if (wllt == 'n') {
        dat$dsstox_substance_id <- "Neutral/negative control"
        dat$chnm <- ""
      }
      else if (wllt == 'b') {
        dat$dsstox_substance_id <- "Blank"
        dat$chnm <- ""
      }
      else if (wllt == 'v') {
        dat$dsstox_substance_id <- "Viability control"
        dat$chnm <- ""
      }
      else {
        data$dsstox_substance_id <- paste0("Well type: ", wllt)
        data$chnm <- ""
      }
    } 
    else {
      warning(paste0("wllt for SPID: ", dat$spid, " is missing or length > 1. 
                     Leaving dsstox_substance_id and chnm as NA."))
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
      scale_x_continuous(limits = l3_range, trans = ifelse(0 %in% l3_dat$conc,"identity","log10")) +
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
      scale_x_continuous(limits = l3_range, trans = ifelse(0 %in% l3_dat$conc,"identity","log10")) +
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



#' tcplggplotCompare
#' 
#' @param dat data table with all required conc/resp data
#' @param compare.dat data table with all required conc/resp data for comparison
#' overlay
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
#' @importFrom ggplot2 margin unit element_text geom_segment scale_color_manual
#' @import gridExtra
#' @import stringr
tcplggplotCompare <- function(dat, compare.dat, lvl = 5, verbose = FALSE, flags = FALSE, yrange = c(NA,NA)) {
  # variable binding
  conc <- resp <- xpos <- ypos <- hjustvar <- vjustvar <- NULL
  annotateText <- name <- aic <- NULL
  l3_dat_main <- tibble(conc = unlist(dat$conc), resp = unlist(dat$resp), max_med = dat$max_med, l3 = "main")
  l3_dat_compare <- tibble(conc = unlist(compare.dat$conc), resp = unlist(compare.dat$resp), max_med = compare.dat$max_med, l3 = "compare")
  l3_dat_both <- rbind(l3_dat_main, l3_dat_compare)
  l3_range <- l3_dat_both %>%
    pull(.data$conc) %>%
    range()
  
  if (dat$conc_unit != compare.dat$conc_unit || dat$normalized_data_type != compare.dat$normalized_data_type) stop("Units do not match.")
  
  # main data
  # check if model_type is 3 or 4, which means an override method was assigned
  if (lvl == 5 && dat$model_type == 3) { # gain direction
    # leave coff but bmr should flip if top is negative
    if (!is.null(dat$top) && !is.na(dat$top) && !is.null(dat$bmr)) {
      if (dat$top < 0) {
        dat$bmr <- dat$bmr * -1
      }
    }
  } else if (lvl == 5 && dat$model_type == 4) { # loss direction
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
  
  # compare data
  # check if model_type is 3 or 4, which means an override method was assigned
  if (lvl == 5 && compare.dat$model_type == 3) { # gain direction
    # leave coff but bmr should flip if top is negative
    if (!is.null(compare.dat$top) && !is.na(compare.dat$top) && !is.null(compare.dat$bmr)) {
      if (compare.dat$top < 0) {
        compare.dat$bmr <- compare.dat$bmr * -1
      }
    }
  } else if (lvl == 5 && compare.dat$model_type == 4) { # loss direction
    # coff and bmr(if top < 0) should be negative
    if (!is.null(compare.dat$top) && !is.null(compare.dat$coff) && !is.na(compare.dat$top) && !is.null(compare.dat$bmr)) {
      compare.dat$coff <- compare.dat$coff * -1
      if (compare.dat$top < 0) {
        compare.dat$bmr <- compare.dat$bmr * -1
      }
    }
  } else { # bidirectional
    # check if winning model has negative top.  If so coff,bmr should be negative
    if (!is.null(compare.dat$top) && !is.null(compare.dat$coff) && !is.na(compare.dat$top) && !is.null(compare.dat$bmr)) {
      if (compare.dat$top < 0) {
        compare.dat$coff <- compare.dat$coff * -1
        compare.dat$bmr <- compare.dat$bmr * -1
      }
    }
  }
  
  
  
  # check if data is outside bounds of yrange. If so, expand yrange bounds
  if (!identical(yrange, c(NA,NA))) {
    yrange[1] <- min(dat$resp_min, dat$coff, yrange[1], unlist(dat$resp), 
                     compare.dat$resp_min, compare.dat$coff, unlist(compare.dat$resp))
    yrange[2] <- max(dat$resp_max, dat$coff, yrange[2], unlist(dat$resp), 
                     compare.dat$resp_max, compare.dat$coff, unlist(compare.dat$resp))
  }
  
  check_wllt <- function(data) {
    # check if dtxsid is NA, pull wllt in from lvl 3
    if (is.na(data$dsstox_substance_id) | is.na(data$chnm)) {
      wllt <- unique(tcplLoadData(lvl = 0, fld = list("spid","acid"), 
                                  list(data$spid, tcplLoadAcid(fld = "aeid", val = data$aeid)$acid))$wllt)
      if (length(wllt) == 1) {
        if (wllt == 'c' | wllt == 'p') {
          data$dsstox_substance_id <- "Gain-of-signal control"
          data$chnm <- ""
        }
        else if (wllt == 'm' | wllt == 'o') {
          data$dsstox_substance_id <- "Loss-of-signal control"
          data$chnm <- ""
        }
        else if (wllt == 'n') {
          data$dsstox_substance_id <- "Neutral/negative control"
          data$chnm <- ""
        }
        else if (wllt == 'b') {
          data$dsstox_substance_id <- "Blank"
          data$chnm <- ""
        }
        else if (wllt == 'v') {
          data$dsstox_substance_id <- "Viability control"
          data$chnm <- ""
        }
        else {
          data$dsstox_substance_id <- paste0("Well type: ", wllt)
          data$chnm <- ""
        }
      } 
      else {
        warning(paste0("wllt for SPID: ", data$spid, " is missing or length > 1. 
                     Leaving dsstox_substance_id and chnm as NA."))
      }
    }
    return(data)
  }
  dat <- check_wllt(dat)
  compare.dat <- check_wllt(compare.dat)
  
  dat$winning_model_string <- paste0("Model A(", dat$modl, ")")
  compare.dat$winning_model_string <- paste0("Model B(", compare.dat$modl, ")")
  winning_model_geom_function <- function(data, x) {
    if (data$modl == "gnls") {
      tcplfit2::gnls(ps = c(data$gnls_tp, data$gnls_ga, data$gnls_p, data$gnls_la, data$gnls_q), x = x)
    } else if (data$modl == "exp3") {
      tcplfit2::exp3(ps = c(data$exp3_a, data$exp3_b, data$exp3_p), x = x)
    } else if (data$modl == "exp4") {
      tcplfit2::exp4(ps = c(data$exp4_tp, data$exp4_ga), x = x)
    } else if (data$modl == "exp5") {
      tcplfit2::exp5(ps = c(data$exp5_tp, data$exp5_ga, data$exp5_p), x = x)
    } else if (data$modl == "poly1") {
      tcplfit2::poly1(ps = c(data$poly1_a), x = x)
    } else if (data$modl == "exp2") {
      tcplfit2::exp2(ps = c(data$exp2_a, data$exp2_b), x = x)
    } else if (data$modl == "poly2") {
      tcplfit2::poly2(ps = c(data$poly2_a, data$poly2_b), x = x)
    } else if (data$modl == "pow") {
      tcplfit2::pow(ps = c(data$pow_a, data$pow_p), x = x)
    } else if (data$modl == "hill") {
      tcplfit2::hillfn(ps = c(data$hill_tp, data$hill_ga, data$hill_p), x = x)
    } else {
      x*NA
    }
  }
  
  flag_count <- 0
  flag_count_compare <- 0
  if (flags && dat$flag != "None") {
    flag_count <- str_count(dat$flag, "\n") + 1
  }
  if (flags && compare.dat$flag != "None") {
    flag_count_compare <- str_count(compare.dat$flag, "\n") + 1
  }
  
  
  identical_title <- paste0(stringr::str_trunc(paste0(
    ifelse(dat$dsstox_substance_id == compare.dat$dsstox_substance_id, paste0(dat$dsstox_substance_id, " "), ""),
    ifelse(dat$chnm == compare.dat$chnm, paste0(dat$chnm, "\n"), "")
    ), 75),
    stringr::str_trunc(paste0(
      ifelse(dat$spid == compare.dat$spid, paste0("SPID:", dat$spid, "  "), ""),
      ifelse(dat$aeid == compare.dat$aeid, paste0("AEID:", dat$aeid, "  "), ""),
      ifelse(dat$aenm == compare.dat$aenm, paste0("AENM:", dat$aenm), "")), 70))
  if (identical_title != "" & !endsWith(identical_title, "\n")) {
    identical_title <- paste0(identical_title, "\n")
  }
  
  
  if (lvl == 2) {
    gg <- ggplot(l3_dat_both, aes(conc, resp, color = l3)) +
      geom_hline(aes(yintercept = dat$max_med, linetype = "Max Median A"), color="blue") +
      geom_hline(aes(yintercept = compare.dat$max_med, linetype = "Max Median B"), color="red") +
      geom_hline(aes(yintercept = compare.dat$coff, linetype="Cutoff B"), color="red") +
      geom_hline(aes(yintercept = dat$coff, linetype="Cutoff A"), color="blue") +
      geom_point() +
      scale_x_continuous(limits = l3_range, trans = ifelse(0 %in% l3_dat_both$conc,"identity","log10")) +
      scale_y_continuous(limits = yrange) +
      scale_linetype_manual("", breaks = c("Max Median A", "Max Median B", "Cutoff A", "Cutoff B"),
                            guide = guide_legend(override.aes = list(color = c("blue", "red", "blue", "red"), linetype = c("solid", "solid", "dashed", "dashed"))), 
                            values = if (compare.dat$coff == dat$coff) c("solid", "solid", "39", "15393933") else c("solid", "solid", "33", "33")) +
      scale_color_manual(breaks = c(), guide = guide_legend(reverse = TRUE), values=c("red", "blue")) + 
      xlab(paste0("Concentration ", "(", dat$conc_unit, ")")) +
      ylab(stringr::str_to_title(gsub("_", " ", dat$normalized_data_type))) +
      labs(
        title = paste0(
          ifelse(identical_title != "", paste0(identical_title, "\n"), ""),
          stringr::str_trunc(paste0(
            "A: ",
            ifelse(dat$dsstox_substance_id != compare.dat$dsstox_substance_id, paste0(dat$dsstox_substance_id, " "), ""),
            ifelse(dat$chnm != compare.dat$chnm, paste0(dat$chnm, "\n"), "")
          ), 75), 
          stringr::str_trunc(paste0(
            ifelse(dat$aeid != compare.dat$aeid, paste0("AEID:", dat$aeid, "  "), ""),
            ifelse(dat$aenm != compare.dat$aenm, paste0("AENM:", dat$aenm, "\n"), ""),
            ifelse(dat$spid != compare.dat$spid, paste0("SPID:", dat$spid, "  "), "")), 70),
          "S2ID:", dat$s2id, "  ",
          ifelse(verbose, "", paste0(
            "\nHITC:", paste0(trimws(format(round(dat$hitc, 3), nsmall = 3)))
          )),
          stringr::str_trunc(paste0(
            "\n\n",
            "B: ",
            ifelse(dat$dsstox_substance_id != compare.dat$dsstox_substance_id, paste0(compare.dat$dsstox_substance_id, " "), ""),
            ifelse(dat$chnm != compare.dat$chnm, paste0(compare.dat$chnm, "\n"), "")
          ), 75),
          stringr::str_trunc(paste0(
            ifelse(dat$aeid != compare.dat$aeid, paste0("AEID:", compare.dat$aeid, "  "), ""),
            ifelse(dat$aenm != compare.dat$aenm, paste0("AENM:", compare.dat$aenm, "\n"), ""),
            ifelse(dat$spid != compare.dat$spid, paste0("SPID:", compare.dat$spid, "  "), "")), 70),
          "S2ID:", compare.dat$s2id, "  ",
          ifelse(verbose, "", paste0(
            "\nHITC:", paste0(trimws(format(round(compare.dat$hitc, 3), nsmall = 3)))
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
    gg <- ggplot(l3_dat_both, aes(conc, resp, color = l3)) +
      geom_function(aes(linetype = dat$winning_model_string), fun = function(x) winning_model_geom_function(dat, x), color = "blue", alpha = 0.5) +
      geom_function(aes(linetype = compare.dat$winning_model_string), fun = function(x) winning_model_geom_function(compare.dat, x), color = "red", alpha = 0.5) +
      geom_hline(aes(yintercept = compare.dat$coff, linetype = "Cutoff B"), color = "red") +
      geom_hline(aes(yintercept = dat$coff, linetype = "Cutoff A"), color = "blue") +
      geom_point(alpha = 0.5) +
      scale_x_continuous(limits = l3_range, trans = ifelse(0 %in% l3_dat_both$conc,"identity","log10")) +
      scale_y_continuous(limits = yrange) +
      scale_linetype_manual("", breaks = c(dat$winning_model_string, compare.dat$winning_model_string, "Cutoff A", "Cutoff B"), 
                            guide = guide_legend(override.aes = list(color = c("blue", "red", "blue", "red"), linetype = c("solid", "solid", "dashed", "dashed"))), 
                            values = if (compare.dat$coff == dat$coff) c("solid", "solid", "39", "15393933") else c("solid", "solid", "33", "33"),) + 
      scale_color_manual(breaks = c(), guide = guide_legend(reverse = TRUE), values=c("red", "blue")) + 
      xlab(paste0("Concentration ", "(", dat$conc_unit, ")")) +
      ylab(stringr::str_to_title(gsub("_", " ", dat$normalized_data_type))) +
      labs(
        title = paste0(
          ifelse(identical_title != "", paste0(identical_title, "\n"), ""),
          stringr::str_trunc(paste0(
            "A: ",
            ifelse(dat$dsstox_substance_id != compare.dat$dsstox_substance_id, paste0(dat$dsstox_substance_id, " "), ""),
            ifelse(dat$chnm != compare.dat$chnm, paste0(dat$chnm, "\n"), "")
          ), 75), 
          stringr::str_trunc(paste0(
            ifelse(dat$spid != compare.dat$spid, paste0("SPID:", dat$spid, "  "), ""),
            ifelse(dat$aeid != compare.dat$aeid, paste0("AEID:", dat$aeid, "  "), ""),
            ifelse(dat$aenm != compare.dat$aenm, paste0("AENM:", dat$aenm, "\n"), "")), 70),
          "M4ID:", dat$m4id, "  ",
          ifelse(verbose, "", paste0(
            "\nHITC:", paste0(trimws(format(round(dat$hitc, 3), nsmall = 3)))
          )),
          stringr::str_trunc(paste0(
            "\n\n",
            "B: ",
            ifelse(dat$dsstox_substance_id != compare.dat$dsstox_substance_id, paste0(compare.dat$dsstox_substance_id, " "), ""),
            ifelse(dat$chnm != compare.dat$chnm, paste0(compare.dat$chnm, "\n"), "")
          ), 75),
          stringr::str_trunc(paste0(
            ifelse(dat$spid != compare.dat$spid, paste0("SPID:", compare.dat$spid, "  "), ""),
            ifelse(dat$aeid != compare.dat$aeid, paste0("AEID:", compare.dat$aeid, "  "), ""),
            ifelse(dat$aenm != compare.dat$aenm, paste0("AENM:", compare.dat$aenm, "\n"), "")), 70),
          "M4ID:", compare.dat$m4id, "  ",
          ifelse(verbose, "", paste0(
            "\nHITC:", paste0(trimws(format(round(compare.dat$hitc, 3), nsmall = 3)))
          ))
        ),
        caption = ifelse(flags, paste0(
          "\nFlags:\nA(", flag_count, "): ", paste0(trimws(format(dat$flag, nsmall = 3))), 
          "\n\nB(", flag_count_compare, "): ", paste0(trimws(format(compare.dat$flag, nsmall = 3)))
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
  
  t <- NULL
  if (lvl != 2) {
    details <- tibble(Hitcall = c(dat$hitc, compare.dat$hitc), 
                      BMD = c(dat$bmd, compare.dat$bmd), 
                      AC50 = c(dat$ac50, compare.dat$ac50))
    details <- details %>% mutate_if(is.numeric, ~ round_n(., 3))
    details <- as.data.frame(details)
    t <- tableGrob(details, rows = c("A", "B"))
    ifelse(verbose,
           return(arrangeGrob(gg, t, nrow = 1, widths = 2:1)),
           return(arrangeGrob(gg))
    )
  } else {
    details <- tibble(Hitcall = c(dat$hitc, compare.dat$hitc))
    details <- details %>% mutate_if(is.numeric, ~ round_n(., 3))
    t <- tableGrob(details, rows = c("A", "B"))
    ifelse(verbose,
           return(arrangeGrob(gg, t, ncol = 1, heights = c(4,1))),
           return(gg)
    )
  }
}
