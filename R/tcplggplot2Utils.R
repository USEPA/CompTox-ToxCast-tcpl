#' MORELETTERS
#' Extends LETTERS recursively to any number of letters
#' 
#' @param i numeric vector of all needed letter combinations, i.e. 1:nrow(dat)
#' 
#' @return char vector containing all letter/combinations
#' @author https://stackoverflow.com/a/25881167
MORELETTERS <- function(i) {
  base10toA <- function(n, A) {
    stopifnot(n >= 0L)
    N <- length(A)
    j <- n %/% N 
    if (j == 0L) A[n + 1L] else paste0(Recall(j - 1L, A), A[n %% N + 1L])
  }   
  vapply(i-1L, base10toA, character(1L), LETTERS)
}


#' tcplfit2_fun
#' Returns tcplfit2 function for drawing models given input
#' 
#' @param row data table with all required conc/resp data
#' @param model string of model to draw from tcplfit2
#' @param x int vector of x values to calculate curve (y) values for 
#'
#' @return a tcplfit2 function call
tcplfit2_fun <- function(row, model, x) {
  if (model == "gnls") {
    tcplfit2::gnls(ps = c(row$gnls_tp, row$gnls_ga, row$gnls_p, row$gnls_la, row$gnls_q), x = x)
  } else if (model == "exp2") {
    tcplfit2::exp2(ps = c(row$exp2_a, row$exp2_b), x = x)
  } else if (model == "exp3") {
    tcplfit2::exp3(ps = c(row$exp3_a, row$exp3_b, row$exp3_p), x = x)
  } else if (model == "exp4") {
    tcplfit2::exp4(ps = c(row$exp4_tp, row$exp4_ga), x = x)
  } else if (model == "exp5") {
    tcplfit2::exp5(ps = c(row$exp5_tp, row$exp5_ga, row$exp5_p), x = x)
  } else if (model == "poly1") {
    tcplfit2::poly1(ps = c(row$poly1_a), x = x)
  } else if (model == "poly2") {
    tcplfit2::poly2(ps = c(row$poly2_a, row$poly2_b), x = x)
  } else if (model == "pow") {
    tcplfit2::pow(ps = c(row$pow_a, row$pow_p), x = x)
  } else if (model == "hill") {
    tcplfit2::hillfn(ps = c(row$hill_tp, row$hill_ga, row$hill_p), x = x)
  } else {
    x*NA
  }
}

#' get_plot_title
#' Generate plot title given number of rows and length of unique elements
#' 
#' @param dat data table with all required conc/resp data; each row will extend comparison
#' @param type string of mc or sc indicating if it is single or multi conc
#' @param compare Character vector, the field(s) samples were joined on for comparison
#' @param verbose bool if hitc should be included in the title
#'
#' @return a string title for the individual plot
#' @importFrom stringr str_trunc
get_plot_title <- function(dat = NULL, type = "mc", compare = "m4id", verbose = TRUE) {
  # identify matching elements of the title across all compared samples
  title <- trimws(paste0(
    stringr::str_trunc(
      paste0(
        ifelse(lu(dat$dsstox_substance_id) == 1 && all(dat$dsstox_substance_id != dat$chnm), paste0(dat$dsstox_substance_id[1], " "), ""),
        ifelse(lu(dat$chnm) == 1, paste0(dat$chnm[1]), ""), "\n"), 
      75, ellipsis = "...\n"),
    stringr::str_trunc(
      paste0(
        ifelse(lu(dat$spid) == 1, paste0("SPID:", dat$spid[1], "  "), ""),
        ifelse(lu(dat$aeid) == 1, paste0("AEID:", dat$aeid[1], "  "), ""),
        ifelse(lu(dat$aenm) == 1, paste0("AENM:", dat$aenm[1]), ""), "\n"), 
      70, ellipsis = "...\n"),
    ifelse(type == "mc",
           ifelse(lu(dat$m4id) == 1, paste0("M4ID:", dat$m4id[1]), ""),
           ifelse(lu(dat$s2id) == 1, paste0("S2ID:", dat$s2id[1]), "")),
    ifelse(!verbose && nrow(dat) == 1, 
           paste0("\nHITC:", paste0(format(round(dat$hitc, 3), nsmall = 3))), "")))
  title <- trimws(title)
  if (title == "") {
    if (all(compare == "group_from_list_dat")) {
      return(paste0("User defined group: ", unique(dat$group_from_list_dat)))
    }
    return(paste0(compare, ": ", unique(dat[,compare,with=FALSE]), collapse = "; "))
  }
  title
}


#' get_plot_caption
#' Generate plot caption given number of flags and length of unique elements
#' 
#' @param dat data table including all required flags
#'
#' @return a string caption for the individual plot
get_plot_caption <- function(dat = NULL) {
  if (nrow(dat) == 1)
    return(paste0("\nFlags(", dat$flag_count, "): ", dat$flag))
  caption <- "Flags:\n"
  some <- which(dat$flag_count != 0)
  if (length(some) > 0) {
    for (i in some) {
      row <- dat[i]
      if (i != some[1]) caption <- paste0(caption, " | ")
      caption <- paste0(caption, 
                        row$index, "(", row$flag_count, "): ", row$flag)
    }
  }
  none <- which(dat$flag_count == 0)
  if (length(none) > 0)
    caption <- paste0(caption, ifelse(length(some) > 0," | ", ""), paste(dat$index[none], collapse = ","), "(0): None")
  caption
}

#' round_n
#' General function to round/shorten values for plotting tables
#' 
#' @param x numeric value
#' @param n numeric number of decimal places to round to
#' 
#' @return format string in decimal or scientific notation
round_n <- Vectorize(function(x, n=3) {
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
})


#' dynamic_table_trunc
#' Dynamically truncate lengths of column values of they are longer than a
#' calculated width. Strings contained in verbose table output can be very long, 
#' and this function ensures that the 128 character row limit is kept while
#' growing/shrinking default columns widths that may or may not need the space.
#' 
#' @param tbl data.table with potential long values to truncate
#' @param all_cols every annotation column to assign lengths
#' 
#' @return altered data table with truncated strings
#' @importFrom stringr str_trunc
dynamic_table_trunc <- function(tbl = NULL, all_cols) {
  # define trunc-able columns (these columns are variable width and can be long)
  trunc_cols <- c("chnm", "spid", "aenm")
  # subset trunc cols by available cols in tbl
  trunc_cols <- trunc_cols[trunc_cols %in% colnames(tbl)]
  # exit early if there are no trunc-cols
  if (length(trunc_cols) == 0) return(tbl)
  # define default width for each column (index, m4id, dtxsid, chnm, spid, aeid, aenm, color)
  all_lens <- c(2, 9, 13, 30, 20, 5, 40, 9)
  names(all_lens) <- all_cols
  # select default chnm, spid, and aenm as these will be variable
  lens <- all_lens[trunc_cols]
  # determine actual width of longest column string, or default if smaller than longest
  act_lens <- mapply(function(col, len) {
    min(len, max(nchar(tbl[[col]])))
  }, trunc_cols, lens)
  # calculate differences between default col width and actual
  diffs <- mapply(function(len, act_len) len - act_len, lens, act_lens)
  # calculate the number of available chars to assign to columns that need the extra width
  chars_to_give <- sum(all_lens) - sum(act_lens) - 
    sum(all_lens[!names(all_lens) %in% trunc_cols & names(all_lens) %in% colnames(tbl)])
  # proportionately divvy up the chars to give, if diffs is 0 (max col width greater than default)
  act_lens[diffs == 0] <- round(act_lens[diffs == 0] / sum(act_lens[diffs == 0]) * 
                                  (chars_to_give + sum(act_lens[diffs == 0])))
  # perform truncation on the variable width trunc cols using calculated lengths
  tr_map <- mapply(function(col, len) {
    stringr::str_trunc(tbl[[col]], len, side = "center")
  }, trunc_cols, act_lens, SIMPLIFY = FALSE)
  # insert truncated strings into tbl and return
  tbl[,(names(tr_map)) := tr_map]
  return(tbl)
}


#' get_verbose_tables
#' Generates 'gt' package tables containing annotations and level 5 metrics
#' 
#' @param dat data table with all level 5 metrics and mapped annotations
#' @param type string of mc or sc indicating if it is single or multi conc
#' @param compare Character vector, the field(s) samples were joined on for comparison
#' @param flags bool if flags should be included in the table footer
#' 
#' @return list of 2 GT tables
#' @import gt
#' @importFrom dplyr select all_of mutate_if
#' @importFrom stringr str_trunc
get_verbose_tables <- function(dat = NULL, type = "mc", compare = "m4id", flags = FALSE) {
  
  # variable binding for R CMD check
  index <- hitc <- bmd <- ac50 <- color <- INDEX <- NULL
  
  # potential annotation columns for table
  all_cols <- c("index",ifelse(type == "mc", "m4id", "s2id"),
                "dsstox_substance_id","chnm","spid","aeid","aenm","color")
  # bool vector if the column's length of unique values is greater than 1
  lu_gt_one <- sapply(all_cols, function(col) {
    lu(dat[,col,with=FALSE][[1]]) > 1
  })
  tbl_cols <- all_cols[lu_gt_one]
  # select columns which the length of unique is greater than 1
  tbl <- dat |> select(all_of(tbl_cols))
  tbl[is.na(tbl)] <- "" # replace NAs with empties
  
  # truncate long values
  tbl <- dynamic_table_trunc(tbl, all_cols)
  
  if (type == "mc") {
    
    # create second table using level 5 potency values
    tbl2 <- dat |> select(index, hitc, bmd, ac50, color) |>
      mutate_if(is.numeric, ~ round_n(., 3))
    
  } else if (type == "sc") {
    
    # create second table using level 2 hitc values
    tbl2 <- dat |> select(index, hitc, color) |>
      mutate_if(is.numeric, ~ round_n(., 3))
    
  }
  
  # set names to upper
  colnames(tbl)[names(tbl) == "dsstox_substance_id"] <- "dtxsid"
  colnames(tbl) <- colnames(tbl) |> toupper()
  colnames(tbl2) <- colnames(tbl2) |> toupper()
  
  # define style used by both tables
  gt_style <- function(GT) {
    GT |> 
      tab_style(
        style = list(
          cell_text(weight = "bold",),
          cell_fill(color = "gray")
        ),
        locations = cells_column_labels()
      ) |>
      tab_style(
        style = list(
          cell_text(weight = "bold",
                    align = "center",
                    style = "italic"),
          cell_fill(color = from_column(column = "COLOR"))
        ),
        locations = cells_body(columns = INDEX)
      ) |>
      cols_label(INDEX = "") |>
      cols_hide(columns = "COLOR") |>
      opt_row_striping()
  }
  
  title <- stringr::str_trunc(get_plot_title(dat, type, compare, TRUE),105)
  
  anno_tbl <- gt(tbl) |>
    gt_style()
  
  if (title != "") {
    anno_tbl<- anno_tbl |>
      tab_header(
        title = stringr::str_replace_all(title, "\n", "  "),
        subtitle = "Above are common, below are unique"
      )
  }
  
  if (flags) {
    anno_tbl <- anno_tbl  |>
      tab_source_note(
        source_note = stringr::str_wrap(stringr::str_replace_all(get_plot_caption(dat), "\n", "  "), 160)
        )
  }
  
  l5_tbl <- gt(tbl2) |>
    gt_style()
  
  return(list(anno_tbl = as_gtable(anno_tbl), l5_tbl = as_gtable(l5_tbl)))
  
}