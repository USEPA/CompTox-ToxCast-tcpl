#' #-------------------------------------------------------------------------------
# tcplPlot: plot tcpl data
#-------------------------------------------------------------------------------

#'  Generic Plotting Function for tcpl
#'
#' @description
#' \code{tcplPlot} queries the tcpl databases and returns a plot
#' for the given level and data type.
#'
#' @param dat data.table or list of data.tables containing plot-prepared data, not
#' required. Used for stand-alone (ToxCast or other tcplfit2-fit data) plotting or 
#' advanced plotting (generating comparison plots across multiple database configurations). 
#' Pass a data.table for default behavior, which will split data by 'compare'. Pass 
#' a list of data.tables to directly specify the groupings for comparison plots 
#' where each list item (data.table) will be printed on a single plot. See \code{tcplPlotLoadData}.
#' @param type Character of length 1, the data type, "sc" or "mc".
#' @param fld Character vector, the field(s) to query on.
#' @param val List containing vectors of values for each field to query on. Must 
#' be in the same order as 'fld'.
#' @param compare Character vector, the field(s) to join samples on to create comparison
#' plots. By default 'm4id'. As every endpoint-sample will always have its own m4id,
#' this will create individual plots. To create a comparison plot across the same
#' chemicals, use a chemical identifier like "dsstox_substance_id". Likewise, to 
#' create a comparison plot across the same sample ids, use "spid". Use "aeid" to
#' create a comparison plot across the same assay component endpoints, which will 
#' likely trigger the large compare plot style; for more info see 'group.threshold'. 
#' To use a custom field to create comparison, 'dat' should be supplied as a data.table 
#' generated from tcplPlotLoadData with the custom column added. If 'dat' is instead 
#' a list of data.tables, setting 'compare' will be ignored in favor of the list 
#' groups.
#' @param output How should the plot be presented. To work with the plot in 
#' environment, use "ggplot"; to interact with the plot in application, use 
#' "console"; or to save as a file type, use "pdf", "jpg", "png", "svg", or "tiff".
#' @param multi Boolean, by default TRUE for "pdf". Prints variable number of plots
#' per page depending on 'verbose' and 'type' settings.
#' @param fileprefix Prefix of file when saving.
#' @param by Parameter to divide files into e.g. "aeid".
#' @param verbose Boolean, by default TRUE. If TRUE, a table with fitting parameters
#' is included with the plot. To return or save simple plot, set to FALSE. In comparison
#' plotting, verbose as TRUE will also return annotation information in a table,
#' which is hidden using verbose = FALSE. 
#' @param nrow Integer, number of rows in multiplot. By default 2.
#' @param ncol Integer, number of columns in multiplot. By default 3, 2 if verbose, 
#' 1 for verbose compare plots.
#' @param dpi Integer, image print resolution. By default 600.
#' @param flags Boolean, by default FALSE. If TRUE, level 6 flags are displayed
#' within output.
#' @param yuniform Boolean, by default FALSE. If TRUE, all plots will have uniform
#' y axis scaling, automatically determined.
#' @param yrange Integer of length 2, for directly setting the y-axis range, 
#' c(<min>,<max>). By default, c(NA,NA). Plots containing points or curves outside
#' this range will be expanded to fit.
#' @param group.fld string column name to group curves by when number of comparison 
#' curves exceeds group.threshold, default being 'modl' for MC and 'hitc' for SC
#' @param group.threshold integer of length 1, number of curves where comparison 
#' plot style should change to instead group by a given group.fld, default of 9 
#' -- greater than 8 curves
#'
#' @details
#' The data type can be either 'mc' for multiple concentration data, or 'sc'
#' for single concentration data. 
#'
#' Leaving \code{fld} NULL will return all data.
#' @import data.table
#' @importFrom gridExtra marrangeGrob
#' @importFrom ggplot2 ggsave is.ggplot
#' @importFrom dplyr %>% all_of pull
#' @importFrom grDevices pdf.options
#' @export
#'
#' @examples
#' \dontrun{
#' tcplPlot(fld = "m4id", val = c(18609966)) ## Create a level 4 plot
#' }
tcplPlot <- function(dat = NULL, type = "mc", fld = "m4id", val = NULL, compare = "m4id", 
                     by = NULL, output = c("ggplot", "console", "pdf", "png", "jpg", "svg", "tiff"), 
                     fileprefix = paste0("tcplPlot_", Sys.Date()), multi = NULL, 
                     verbose = TRUE, nrow = NULL, ncol = NULL, dpi = 600, flags = FALSE, 
                     yuniform = FALSE, yrange=c(NA,NA), group.fld = NULL, group.threshold = 9) {
  
  #set pdf options
  enc <- pdf.options()$encoding
  pdf.options(encoding="CP1253.enc")
  on.exit(pdf.options(encoding = enc))
  
  # Validate vars based on some assumed properties
  validated_vars <- tcplPlotValidate(dat = dat,type = type,compare = compare,by=by,flags = flags,output = output,multi = multi,verbose = verbose)
  # take list of validated vars and add them to the function's environment
  list2env(validated_vars, envir = environment())

  # check_tcpl_db_schema is a user-defined function found in v3_schema_functions.R file
  if (check_tcpl_db_schema() | !is.null(dat) | getOption("TCPL_DRVR") == "API") {
    # check if user supplied data.  If not, load from db connection
    if(is.null(dat)){
      dat <- tcplPlotLoadData(type = type, fld = fld, val = val, flags = flags) #code defined in tcplPlotUtils.R
    } else if (!is.data.table(dat)) {
      dat <- rbindlist(lapply(seq_along(dat), function(i) dat[[i]][,group_from_list_dat := i]))
      compare <- "group_from_list_dat"
    }
    
    # set yrange from tcplPlotUtils.R
    yrange <- tcplPlotSetYRange(dat,yuniform,yrange,type)
    
    # validate 'by' and 'compare' against available cols from dat
    if (!is.null(by) && !by %in% colnames(dat)) stop("'by' must be a valid field contained in dat.")
    if (any(!compare %in% colnames(dat))) stop("'compare' must all be valid field(s) contained in dat.")

    if (is.null(by) || output %in% c("ggplot", "console")) { # split data into tables for plots
      split_dat <- list(split(dat, by = compare))
    } else { # split data into lists for files and then tables for plots
      split_dat <- split(dat, by = c(by,compare), flatten = FALSE)
      if (length(compare) > 1) { # collapse nested lists 
        unnest_list <- function(nested_list) {
          unnested_list <- list()
          for (element in nested_list) { # if element is still a list, recurse further
            unnested_list <- c(unnested_list, ifelse(!is.data.table(element),
                                                     unnest_list(element),
                                                     list(element)))
          }
          return(unnested_list)
        }
        split_dat <- lapply(split_dat, function(i) unnest_list(i))
      }
    }
    
    n <- 0 # the number of individual plots
    nrows <- NULL # the number of rows across every plot
    for (f in split_dat) {
      n <- n + length(f)
      for (p in f) {
        nrows <- c(nrows, nrow(p))
      }
    }
    
    # error message for output="console" and multi=FALSE to avoid multiple plots in console
    if(n > 1 && output %in% c("ggplot", "console")) 
      stop("More than 1 concentration series returned for given field/val combination. Set output to pdf or reduce the number of plots to 1. Current number of individual plots: ", n)
    
    if (output == "console") {
      # tcplPlotlyplot is the user-defined function used to connect tcpl and plotly packages
      return(tcplPlotlyPlot(split_dat[[1]][[1]], lvl))
    }
    
    aspect_ratio_vars <- tcplPlotCalcAspectRatio(type=type, verbose=verbose, multi = multi, 
                                                 nrows = nrows, nrow=nrow, ncol=ncol, flags=flags,
                                                 group.threshold=group.threshold, output = output)
    list2env(aspect_ratio_vars, envir = environment())
    
    for (f in split_dat) {
      plot_list <- lapply(f, tcplggplot2, type = type, compare = compare, verbose = verbose, flags = flags, 
                          yrange = yrange, group.fld = group.fld, group.threshold = group.threshold)
      if (output == "ggplot") {
        if (!is.ggplot(plot_list[[1]])) message("ggplot and verbose table arranged into one grob. To work with a simple ggplot object, set `verbose = FALSE` and 'flags = FALSE'.")
        return(plot_list[[1]])
      }
      m1 <- marrangeGrob(plot_list, nrow = nrow, ncol = ncol)
      if(output=="pdf"){
        ggsave(paste0(fileprefix,ifelse(is.null(by),"",paste0("_",by,"_",unique(rbindlist(f)[[by]]))), ".pdf"), m1, width = w*ncol, height = h*nrow)
      } else {
        names(plot_list) <- lapply(seq_along(f), function(i) paste(f[[i]]$m4id, collapse = "_"))
        lapply(names(plot_list), function(x)ggsave(filename=paste0(fileprefix,"_",x,".",output),
                                                   plot=arrangeGrob(grobs=plot_list[x]), width = w, height = h, dpi=dpi))
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
  model_stats <- model <- param <- value <- ac50 <- hitc <- compare <- NULL
  
  compare.dat <- dat[0]
  if (nrow(dat) > 2) {
    stop("tcplPlotlyPlot currently supports only comparisons up to 2 curves.")
  } else if (nrow(dat) == 2) {
    compare.dat <- dat[2]
  }
  dat <- dat[1]
  
  l3_dat_main <- tibble(conc = unlist(dat$conc), resp = unlist(dat$resp), max_med = dat$max_med, l3 = "response A")
  l3_dat_compare <- tibble(conc = unlist(compare.dat$conc), resp = unlist(compare.dat$resp), max_med = compare.dat$max_med, l3 = "response B")
  l3_dat_both <- rbind(l3_dat_main, l3_dat_compare)
  
  # extract range from level 3 data for creating plotting all the functions
  # increase resolution to get smoother curves
  resolution <- 100
  x_min_max <- range(l3_dat_both$conc, na.rm=TRUE)
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
    
  } else if (lvl == 2) { #single conc
    # main data
    if (!is.null(dat$coff) && dat$max_med < 0) {
      dat$coff <- dat$coff * -1
    }
    if (!is.null(dat$coff) && !is.null(dat$hitc) && dat$hitc < 0) {
      dat$coff <- dat$coff * -1
    }
    
    # compare data
    if (nrow(compare.dat) > 0) {
      if (!is.null(compare.dat$coff) && compare.dat$max_med < 0) {
        compare.dat$coff <- compare.dat$coff * -1
      }
      if (!is.null(compare.dat$coff) && !is.null(compare.dat$hitc) && compare.dat$hitc < 0) {
        compare.dat$coff <- compare.dat$coff * -1
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
        "</br>", paste0("Cut Off B (", specify_decimal(compare.dat$coff,2), ")")
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