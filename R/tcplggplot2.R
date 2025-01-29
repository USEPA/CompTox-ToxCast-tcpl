#' tcplggplot2
#' 
#' @param dat data table with all required conc/resp data; each row will extend comparison
#' @param type string type of data that should be plotted
#' 'sc' plotting - max medians plotted with hitcall
#' 'mc' plotting - all fit models and winning model with hitcall
#' @param compare Character vector, the field(s) samples were joined on for comparison
#' @param verbose boolean should plotting include table of values next to the plot
#' @param flags boolean should plotting include level 6 flags in plot caption
#' @param yrange Integer of length 2, for overriding the y-axis range, c(<min>,<max>). 
#' By default, c(NA,NA).
#' @param group.fld string column name to group curves by when number of curves 
#' exceeds group.threshold, default being 'modl' for MC and 'hitc' for SC
#' @param group.threshold integer of length 1, number of curves where plot style
#' should change to instead group by a given group.fld, default of 9 -- greater 
#' than 8 curves
#'
#' @return A ggplot object or grob with accompanied table depending on verbose option
#' @importFrom dplyr %>% filter group_by summarise left_join inner_join select rowwise mutate pull mutate_if case_when
#' @importFrom dplyr tibble contains everything as_tibble arrange .data summarize n
#' @importFrom tidyr unnest_longer
#' @importFrom ggplot2 ggplot aes geom_function geom_vline geom_hline geom_point scale_x_continuous scale_y_continuous scale_color_viridis_d
#' @importFrom ggplot2 guide_legend scale_linetype_manual xlab ylab geom_text labs theme element_blank
#' @importFrom ggplot2 margin unit element_text geom_segment scale_color_manual
#' @importFrom viridis viridis
#' @import gridExtra
#' @import stringr
tcplggplot2 <- function(dat, type = "mc", compare = "m4id", verbose = TRUE, flags = FALSE, 
                        yrange = c(NA,NA), group.fld = NULL, group.threshold = 9) {
  
  # variable binding for R CMD check
  N <- coff <- conc <- resp <- cutoff_string <- ac50 <- name <- aic <- NULL
  flag <- max_med <- max_med_string <- winning_model_string <- modl <- index <- NULL
  
  # checks
  if (is.null(dat)) 
    stop("'dat' must not be NULL.")
  if (nrow(dat) == 0)
    stop("'dat' contains 0 rows.")
  if (!type %in% c("mc", "sc")) 
    stop("'type' must be 'mc' or 'sc'.")
  if (type == "sc" && flags == TRUE)
    flags <- FALSE
  if (!verbose && flags && nrow(dat) > 1 && nrow(dat) < group.threshold)
    warning("'verbose' = FALSE and 'flags' = TRUE. Flags will not be included in comparison plots unless 'verbose' = TRUE.")
  if (is.null(group.fld)) # set default group.fld for large comparisons
    group.fld = ifelse(type == "mc", "modl", "hitc")
  if (length(group.fld) > 1)
    stop("'group.fld' must be of length 1.")
  if (!group.fld %in% colnames(dat))
    stop("'group.fld' must be a column name of 'dat': ", paste(colnames(dat), collapse = ", "))
  if (lu(dat$conc_unit) != 1 || lu(dat$normalized_data_type) != 1)
    stop("Concentration or normalized data type units do not match.")
  
  
  # add winning model/max median string for legend
  if (type == "mc") {
    dat$winning_model_string <- paste0("Winning Model\n(", dat$modl[1], ")")
    dat[modl == "loec", winning_model_string := "LOEC"]
  } else {
    dat$max_med_string <- "Max Median"
  }
  # add cutoff string and linetype for legend
  dat$cutoff_string <- "Cutoff"
  dat$cutoff_linetype <- "dashed"
  # add color for plots
  dat$color <- "black"
  # if there is more than one sample (comparison plot)
  if (nrow(dat) > 1) {
    # assign index to each row (comparison A, B, C...)
    dat$index <- MORELETTERS(1:nrow(dat))
    if (type == "mc") {
      dat$winning_model_string <- paste0("Model ", dat$index, "(", dat$modl, ")")
      dat[modl == "loec", winning_model_string := paste("LOEC", index)]
    } else {
      dat$max_med_string <- paste0("Max Median ", dat$index)
    }
    dat$cutoff_string <- paste0("Cutoff ", dat$index)
    # get cutoffs which have a frequency greater than 1 (need special linetypes)
    repeated_coff <- data.table(table(dat$coff))[N > 1]
    if (nrow(repeated_coff) > 0) {
      for(i in 1:nrow(repeated_coff)) {
        row <- repeated_coff[i]
        # 2 or 4 digits represents "on" and "off" alternating
        # a linetype of "1533" is 1 on, 5 off, 3 on, 3 off, repeating
        linetypes <- switch(row$N, "dashed",
                            c("1533", "39"),
                            c("1B33", "1539", "3F"),
                            c("1B31", "1735", "1339", "3D"),
                            c("1B21", "1824", "1527", "122A", "2D"),
                            c("1911", "1713", "1515", "1317", "1119", "1B"),
                            c("1B11", "1913", "1715", "1517", "1319", "111B", "1D"),
                            c("1D11", "1B13", "1915", "1717", "1519", "131B", "111D", "1F"))
        dat[coff == row$V1]$cutoff_linetype <- if(is.null(linetypes)) "dashed" else linetypes
      }
    }
    dat$color <- viridis::viridis(nrow(dat), begin = 0.1, end = 0.9, option = "turbo")
  }
  
  # function to return a formatted conc_resp table
  process_row <- function(row) {
    return(data.table(
      conc = replace(unlist(row$conc[[1]]), is.na(unlist(row$conc[[1]])), 0), 
      resp = unlist(row$resp[[1]]),
      model = if (type == "mc" && row$modl != "none" && row$modl != "loec") tcplfit2_fun(row, row$modl, unlist(row$conc[[1]])) else row$max_med
    ))
  }
  
  conc_resp <- lapply(1:nrow(dat), function(i) process_row(dat[i]))
  conc_resp_bound <- rbindlist(conc_resp)
  conc_range <- if (nrow(conc_resp_bound) > 0) range(conc_resp_bound$conc) else c(NA,NA)
  model_range <- if (nrow(conc_resp_bound) > 0) range(conc_resp_bound$model) else c(NA,NA)
  
  # check if data is outside bounds of yrange. If so, expand yrange bounds
  if (!all(is.na(yrange))) {
    yrange[1] <- min(unlist(dat$resp), dat$coff, dat$top, model_range[1], yrange[1], na.rm=TRUE)
    yrange[2] <- max(unlist(dat$resp), dat$coff, dat$top, model_range[2], yrange[2], na.rm=TRUE)
  }
  
  # applies to comparison and single plots, sc and mc
  gg <- ggplot() +
    scale_x_continuous(limits = conc_range, trans = ifelse(0 %in% conc_resp_bound$conc,"identity","log10")) +
    scale_y_continuous(limits = yrange) +
    xlab(paste0("Concentration ", "(", dat$conc_unit[1], ")")) +
    ylab(stringr::str_to_title(gsub("_", " ", dat$normalized_data_type[1]))) +
    theme(
      plot.title = element_text(size = 12),
      plot.caption = element_text(hjust = 0, margin = margin(-1,0,1,0)),
      axis.title.x = element_text(margin = margin(3,0,-5,0)),
     # legend.title = element_blank(),
      legend.margin = margin(0, 0, 0, 0),
      legend.spacing.x = unit(0, "mm"),
      legend.spacing.y = unit(0, "mm")
    )
  
  if (nrow(dat) == 1) { # add points and cutoff lines for single plot
    
    gg <- gg + 
      geom_point(data = conc_resp[[1]], aes(conc, resp), show.legend = FALSE) + 
      geom_hline(data = dat, aes(yintercept = coff, color = dat$cutoff_string, linetype = dat$cutoff_string)) +
      labs(title = get_plot_title(dat, type, compare, verbose),
           caption = ifelse(flags, get_plot_caption(dat), ""))
    
  } else if (nrow(dat) < group.threshold) { # add points and cutoff with specific aesthetics for comparison plot
    
    gg <- gg + 
      lapply(1:nrow(dat), function(i) {
        row <- dat[i]
        geom_point(data = conc_resp[[i]], aes(conc, resp), color = row$color, alpha = 0.5, show.legend = FALSE)
      }) +
      geom_hline(data = dat, aes(yintercept = coff, linetype = cutoff_string, color = cutoff_string))
    
    plot_title <- get_plot_title(dat, type, compare, verbose)
    if (verbose == FALSE && plot_title != "") {
      gg <- gg + 
        labs(title = plot_title)
    }
    
  } else {
    
    gg <- gg + 
      geom_point(data = conc_resp_bound, aes(conc, resp), alpha = 0.25) + 
      labs(title = get_plot_title(dat, type, compare, verbose))
    
  }
  
  if (type == "mc") { # multi conc
    
    if (nrow(dat) == 1) { # add winning/losing models with aesthetic overrides
      
      vals <- c(1,1,3,2,2,2)
      brks <- c("LOEC", dat$winning_model_string, "Losing Models", dat$cutoff_string, "BMD", "AC50")
      names(vals) <- brks
      models <- dat |> select(contains("aic")) |> colnames() |> stringr::str_extract("[:alnum:]+")
      losing_models <- models[!models %in% dat$modl]
      
      if (dat$modl == "loec") {
        gg <- gg + geom_vline(aes(xintercept = dat$loec, color = "LOEC", linetype = "LOEC"), na.rm=TRUE)
      } else {
        # add ac50 as NA if missing
        dat[is.null(dat$ac50), ac50 := NA]
        gg <- gg + 
          geom_function(data = dat, aes(linetype = winning_model_string, color = winning_model_string),
                        fun = function(x) tcplfit2_fun(dat, dat$modl, x), na.rm=TRUE) +
          geom_vline(aes(xintercept = dat$ac50, color = "AC50", linetype = "AC50"), na.rm=TRUE)
      }
      
      gg <- gg + 
        lapply(losing_models, function(model) {
          geom_function(aes(linetype = "Losing Models", color = "Losing Models"), 
                        fun = function(x) tcplfit2_fun(dat, model, x), na.rm=TRUE)
        }) +
        scale_color_viridis_d("", direction = -1, breaks = brks, end = 0.85) + 
        scale_linetype_manual("", breaks = brks, values = vals)
      
      if (!is.null(dat$bmd) && !is.null(dat$bmr) && !is.na(dat$bmd) && !is.na(dat$bmr)){ # add bmd segments
        gg <- gg + 
          geom_segment(aes(x=dat$bmd, xend=dat$bmd, y=-Inf, yend=dat$bmr, color = "BMD", linetype = "BMD"), na.rm=TRUE) + 
          geom_segment(x=-Inf, aes(xend=dat$bmd, y = dat$bmr, yend=dat$bmr, color = "BMD", linetype = "BMD"), na.rm=TRUE)
      }
      
      if (verbose) {
        
        combined_p <- rbindlist(lapply(models, function(x) {
          dat |>
            select(contains(paste0(x, c("_aic", "_rme")))) |>
            tidyr::pivot_longer(cols = everything()) |>
            as_tibble()
        }))
        
        l5_verbose <- tableGrob(tibble(Hitcall = dat$hitc, BMD = dat$bmd, AC50 = dat$ac50) |> 
                                  mutate_if(is.numeric, ~ round_n(., 3)), rows = NULL)
        
        pivoted_p <- combined_p |>
          tidyr::extract(name, c("model", "param"), "([[:alnum:]]+)_([[:alnum:]]+)") |>
          pivot_wider(names_from = "param", values_from = "value") |>
          mutate_if(is.numeric, ~ round_n(., 3)) |>
          arrange(as.numeric(aic))
        
        l4_verbose <- tableGrob(pivoted_p, rows = NULL)
        
        valigned <- gtable_combine(l5_verbose, l4_verbose, along = 2)
        
        return(arrangeGrob(gg, valigned, nrow = 1, widths = 2:1))
        
      }
      
    } else if (nrow(dat) < group.threshold) { # add winning models with specific mapped colors
      
      gg <- gg + 
        lapply(1:nrow(dat), function(i) {
          row <- dat[i]
          if (row$modl == "loec") {
            return(geom_vline(data = row, aes(xintercept = loec, color = winning_model_string, linetype = winning_model_string), na.rm=TRUE))
          } else {
            return(geom_function(data = row, aes(linetype = winning_model_string, color = winning_model_string), 
                                 fun = function(x) tcplfit2_fun(row, row$modl, x), xlim = c(log10(min(row$conc[[1]])),log10(max(row$conc[[1]]))), na.rm=TRUE))
          }
        }) +
        scale_color_manual("", breaks = c(dat$winning_model_string, dat$cutoff_string), 
                           values = c(dat$color, dat$color)) +
        scale_linetype_manual("", breaks = c(dat$winning_model_string, dat$cutoff_string),
                              values = c(rep("solid", nrow(dat)), dat$cutoff_linetype),
                              guide = guide_legend(override.aes = list(linetype = c(rep("solid", nrow(dat)), rep("dashed", nrow(dat))))))
      
      if (verbose) {
        
        plot_height <- case_when(nrow(dat) == 2 ~ 7, nrow(dat) == 3 ~ 5, .default = 4)
        tbls <- get_verbose_tables(dat, type, compare, flags)
        return(arrangeGrob(tbls$anno_tbl, 
                           arrangeGrob(gg, tbls$l5_tbl, nrow = 1, widths = c(3,1)),
                           ncol = 1, heights = c(3,plot_height)))
        
      }
      
    } else {
      
      gg <- gg + 
        lapply(1:nrow(dat), function(i) {
          row <- dat[i]
          if (row$modl == "loec") {
            return(geom_vline(data = row, aes(xintercept = loec, color = as.character(!!sym(group.fld))), alpha = 0.5, na.rm=TRUE))
          } else {
            return(geom_function(data = row, aes(color = as.character(!!sym(group.fld))), 
                                 alpha = 0.5, fun = function(x) tcplfit2_fun(row, row$modl, x), 
                                 xlim = c(log10(min(row$conc[[1]])),log10(max(row$conc[[1]]))), na.rm=TRUE))
          }
        }) +
        geom_hline(data = dat, aes(yintercept = coff), linetype = "dashed") +
        scale_color_viridis_d(group.fld, begin = 0.1, end = 0.9, option = "turbo")
      
      if (flags) {
        dat$flag <- stringr::str_split(dat$flag, ";\n")
        flagdat <- select(dat, flag) |> unnest_longer(flag) |> group_by(flag) |> 
          summarize(N = n()) |> arrange(-N)
        plot_height <- case_when(nrow(flagdat) <= 4 ~ 9, nrow(flagdat) >= 10 ~ 3, .default = 13 - nrow(flagdat))
        return(arrangeGrob(gg, tableGrob(flagdat, rows = NULL), ncol = 1, heights = c(plot_height,3)))
      }
      
    }
    
  } else if (type == "sc") { # single conc
    
    if (nrow(dat) == 1) { # add max med line with aesthetic overrides
      
      gg <- gg +
        geom_hline(data = dat, aes(yintercept = max_med, color = max_med_string, linetype = max_med_string)) +
        scale_color_viridis_d("", guide = guide_legend(reverse = TRUE, order = 2), 
                              begin = 0.1, end = 0.9, option = "turbo") + 
        scale_linetype_manual("", guide = guide_legend(reverse = TRUE, order = 2), values = c(2, 1))
      
      if (verbose) {
        
        l2_verbose <- tableGrob(tibble(Hitcall = dat$hitc) |> 
                                  mutate_if(is.numeric, ~ round_n(., 3)), rows = NULL)
        
        return(arrangeGrob(gg, l2_verbose, ncol = 1, heights = c(4,1)))
        
      }
      
    } else if (nrow(dat) < group.threshold) { # add max med lines with specific mapped colors
      
      gg <- gg + 
        geom_hline(data = dat, aes(yintercept = max_med, linetype = max_med_string, color = max_med_string)) +
        scale_color_manual("", breaks = c(dat$cutoff_string, dat$max_med_string), 
                           values = c(dat$color, dat$color)) +
        scale_linetype_manual("", breaks = c(dat$cutoff_string, dat$max_med_string),
                              values = c(dat$cutoff_linetype, rep("solid", nrow(dat))),
                              guide = guide_legend(override.aes = list(linetype = c(rep("dashed", nrow(dat)), rep("solid", nrow(dat))))))
        
      if (verbose) {
        
        plot_height <- case_when(nrow(dat) == 2 ~ 7, nrow(dat) == 3 ~ 5, .default = 4)
        tbls <- get_verbose_tables(dat, type, compare, flags)
        return(arrangeGrob(tbls$anno_tbl, 
                           arrangeGrob(gg, tbls$l5_tbl, nrow = 1, widths = c(3,1)),
                           ncol = 1, heights = c(3,plot_height)))
        
      }
      
    } else {
      
      gg <- gg +
        geom_hline(data = dat, aes(yintercept = max_med, color = as.character(!!sym(group.fld))), alpha = 0.5) +
        geom_hline(data = dat, aes(yintercept = coff), linetype = "dashed") +
        scale_color_viridis_d(group.fld, begin = 0.1, end = 0.9, option = "turbo")
      
    }
    
  }
  
  return(gg)
  
}
