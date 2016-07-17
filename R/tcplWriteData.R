#-------------------------------------------------------------------------------
# tcplWriteData: Write screening data into the tcpl databases
#-------------------------------------------------------------------------------

#' @title Write screening data into the tcpl databases
#' 
#' @description
#' \code{tcplWriteData} takes a data.table with screening data and writes the 
#' data into the given level table in the tcpl databases.
#' 
#' @param dat data.table, the screening data to load
#' @param lvl Integer of length 1, the data processing level
#' @param type Character of length 1, the data type, "sc" or "mc"
#' 
#' @details
#' This function appends data onto the existing table. It also deletes all the 
#' data for any acids or aeids dat contains from the given and all downstream 
#' tables.
#' 
#' The data type can be either 'mc' for mutliple concentration data, or 'sc'
#' for single concentration data. Multiple concentration data will be loaded
#' into the level tables, whereas the single concentration will be loaded into
#' the single tables.
#' 
#' @note
#' This function is not exported and is not inteded to be used by the user. 
#' The user should only write level 0 data, which is written with 
#' \code{\link{tcplWriteLvl0}}.
#' 
#' @seealso \code{\link{tcplCascade}}, \code{\link{tcplAppend}},
#' \code{\link{tcplWriteLvl0}}
#' 
#' @import data.table

tcplWriteData <- function(dat, lvl, type) {
  
  ## Variable-binding to pass R CMD Check
  modified_by <- NULL
  
  if (length(lvl) > 1 | !lvl %in% 0L:6L) {
    stop("Invalid lvl input - must be an integer of length 1 between 0 & 6.")
  }
  
  fkey <- if (lvl > 2 | (lvl > 0 & type == "sc")) "aeid" else "acid"
  ids <- dat[ , unique(get(fkey))]
  
  if (length(ids) >= 500) {
    
    ibins <- split(ids, ceiling(seq_along(ids)/500))
    lapply(ibins, function(x) tcplCascade(lvl = lvl, type = type, id = x))
    
  } else {
    
    tcplCascade(lvl = lvl, type = type, id = ids)
    
  }
  
  mb <- paste(Sys.info()[c("login", "user", "effective_user")], collapse = ".")
  dat[ , modified_by := mb]
  
  if (lvl == 4) {
    
    mc4_cols <- c("aeid",
                  "bmad",
                  "spid",
                  "resp_max",
                  "resp_min",
                  "max_mean",
                  "max_mean_conc",
                  "max_med",
                  "max_med_conc",
                  "logc_max",
                  "logc_min",
                  "cnst",
                  "hill",
                  "hcov",
                  "gnls",
                  "gcov",
                  "cnst_er",
                  "cnst_aic",
                  "cnst_rmse",
                  "cnst_prob",
                  "hill_tp",
                  "hill_tp_sd",
                  "hill_ga",
                  "hill_ga_sd",
                  "hill_gw",
                  "hill_gw_sd",
                  "hill_er",
                  "hill_er_sd",
                  "hill_aic",
                  "hill_rmse",
                  "hill_prob",
                  "gnls_tp",
                  "gnls_tp_sd",
                  "gnls_ga",
                  "gnls_ga_sd",
                  "gnls_gw",
                  "gnls_gw_sd",
                  "gnls_la",
                  "gnls_la_sd",
                  "gnls_lw",
                  "gnls_lw_sd",
                  "gnls_er",
                  "gnls_er_sd",
                  "gnls_aic",
                  "gnls_rmse",
                  "gnls_prob",
                  "nconc", 
                  "npts",
                  "nrep",
                  "nmed_gtbl",
                  "tmpi",
                  "modified_by") 
    
    mc4_agg_cols <- c(paste0("m", 0:4, "id"), "aeid")
    
    tcplAppend(dat = copy(dat[ , unique(.SD) , .SDcols = mc4_cols]), 
               tbl = "mc4", 
               db = getOption("TCPL_DB"))
    
    qformat <- "SELECT m4id, aeid, tmpi FROM mc4 WHERE aeid IN (%s);"
    qstring <- sprintf(qformat, paste0("\"", ids, "\"", collapse = ","))
    
    m4id_map <- tcplQuery(query = qstring, db = getOption("TCPL_DB"))
    setkeyv(m4id_map, c("aeid", "tmpi"))
    setkeyv(dat, c("aeid", "tmpi"))
    
    dat <- m4id_map[dat]
    
    tcplAppend(dat = dat[ , .SD, .SDcols = mc4_agg_cols], 
               tbl = "mc4_agg", 
               db = getOption("TCPL_DB"))
    
  } else if (lvl == 2 & type == "sc")  {
    
    sc2_cols <- c("aeid",
                  "bmad",
                  "spid",
                  "max_med",
                  "hitc",
                  "coff",
                  "tmpi",
                  "modified_by") 
    
    sc2_agg_cols <- c(paste0("s", 0:2, "id"), "aeid")
    
    tcplAppend(dat = copy(dat[ , unique(.SD) , .SDcols = sc2_cols]), 
               tbl = "sc2", 
               db = getOption("TCPL_DB"))
    
    qformat <- "SELECT s2id, aeid, tmpi FROM sc2 WHERE aeid IN (%s);"
    qstring <- sprintf(qformat, paste0("\"", ids, "\"", collapse = ","))
    
    s2id_map <- tcplQuery(query = qstring, db = getOption("TCPL_DB"))
    setkeyv(s2id_map, c("aeid", "tmpi"))
    setkeyv(dat, c("aeid", "tmpi"))
    
    dat <- s2id_map[dat]
    
    tcplAppend(dat = dat[ , .SD, .SDcols = sc2_agg_cols], 
               tbl = "sc2_agg", 
               db = getOption("TCPL_DB"))
    
  } else {
    
    n <- nrow(dat)
    tbl <- paste0(if (type == "mc") "mc" else "sc", lvl)
    db <- getOption("TCPL_DB")
    
    if (n <= 1e6) {
      
      tcplAppend(dat = dat, tbl = tbl, db = db) 
    
    } else {
     
      rbins <- split(1:n, ceiling(seq_along(1:n)/1e6))
      lapply(rbins, function(x) tcplAppend(dat = dat[x], tbl = tbl, db = db))
      
    }
    
  }
  
  TRUE
   
}

#-------------------------------------------------------------------------------
