#-------------------------------------------------------------------------------
# tcplLoadData: Load tcpl data
#-------------------------------------------------------------------------------

#' @title Load tcpl data
#'
#' @description
#' \code{tcplLoadData} queries the tcpl databases and returns a data.table with
#' data for the given level and data type.
#'
#' @param lvl Integer of length 1, the level of data to load
#' @param type Character of length 1, the data type, "sc" or "mc"
#' @param fld Character, the field(s) to query on
#' @param val List, vectors of values for each field to query on. Must be in
#' the same order as 'fld'.
#' @param add.fld Boolean if true we want to return 
#' the additional parameters fit with tcplfit2
#' 
#' @details
#' The data type can be either 'mc' for mutliple concentration data, or 'sc'
#' for single concentration data. Multiple concentration data will be loaded
#' into the 'mc' tables, whereas the single concentration will be loaded into
#' the 'sc' tables.
#'
#' Setting 'lvl' to "agg" will return an aggregate table containing the m4id
#' with the concentration-response data and m3id to map back to well-level
#' information.
#' 
#' If \code{tcplConf()} was set with "API" as the driver, then \code{tcplLoadData} 
#' will return data from the CCTE Bioactivity API. API data is available for
#' \code{type = 'mc'} and lvl = c(3,4,5,6) and 'agg'. Only fields relating to the
#' requested level are returned, but not all fields that usually return from
#' invitrodb are available from the API. To have all fields available from the
#' API return, regardless of what lvl is set to, set \code{add.fld} to 
#' \code{TRUE}. API query-able fields include "aeid", "spid", "m4id", and 
#' "dtxsid".
#'
#' Leaving \code{fld} NULL will return all data.
#'
#' Valid \code{fld} inputs are based on the data level and type:
#' \tabular{ccl}{
#' type \tab lvl \tab  Queried tables \cr
#' sc \tab 0 \tab sc0 \cr
#' sc \tab 1 \tab sc0, sc1 \cr
#' sc \tab agg \tab sc1, sc2_agg \cr
#' sc \tab 2 \tab sc2 \cr
#' mc \tab 0 \tab mc0 \cr
#' mc \tab 1 \tab mc0, mc1 \cr
#' mc \tab 2 \tab mc0, mc1, mc2 \cr
#' mc \tab 3 \tab mc0, mc1, mc3 \cr
#' mc \tab agg \tab mc3, mc4_agg \cr
#' mc \tab 4 \tab mc4 \cr
#' mc \tab 5 \tab mc4, mc5 \cr
#' mc \tab 6 \tab mc4, mc6 \cr
#' mc \tab 7 \tab mc4, mc7
#' }
#'
#' @examples
#' ## Store the current config settings, so they can be reloaded at the end
#' ## of the examples
#' conf_store <- tcplConfList()
#' tcplConfExample()
#'
#' ## Load all of level 0 for multiple-concentration data, note 'mc' is the
#' ## default value for type
#' tcplLoadData(lvl = 0)
#'
#' ## Load all of level 1 for single-concentration
#' tcplLoadData(lvl = 1, type = "sc")
#'
#' ## List the fields available for level 1, coming from tables mc0 and mc1
#' tcplListFlds(tbl = "mc0")
#' tcplListFlds(tbl = "mc1")
#'
#' ## Load level 0 data where the well type is "t" and the concentration
#' ## index is 3 or 4
#' tcplLoadData(lvl = 1, fld = c("wllt", "cndx"), val = list("t", c(3:4)))
#'
#' ## Reset configuration
#' options(conf_store)
#' @return A data.table containing data for the given fields.
#'
#' @seealso \code{\link{tcplQuery}}, \code{\link{data.table}}
#'
#' @import data.table
#' @importFrom tidyr pivot_wider unnest_longer
#' @importFrom utils data
#' @importFrom rlang exec sym
#' @export

tcplLoadData <- function(lvl, fld = NULL, val = NULL, type = "mc", add.fld = TRUE) {
  #variable binding
  model <- model_param <- model_val <- NULL
  hit_param <- hit_val <- sc_vignette <- mc_vignette <- NULL
  
  if (length(lvl) > 1 | length(type) > 1) {
    stop("'lvl' & 'type' must be of length 1.")
  }
  
  drvr <- getOption("TCPL_DRVR")
  if (drvr == "example"){
    if (type == "sc"){
      data("sc_vignette", envir = environment())
      if (lvl == 0L) {
        sc0 <- sc_vignette[["sc0"]]
        sc0 <- sc0[,c("s0id","spid","acid","apid","rowi","coli","wllt","wllq","conc","rval","srcf")]
        return(sc0)
      }
      else if (lvl == 1L) {
        sc1 <- sc_vignette[["sc1"]]
        sc1 <- sc1[,c("s0id","s1id","spid","acid","aeid","apid","rowi","coli","wllt","conc","resp")]
        return(sc1)
      }
      
      else if (lvl == 2L) {
        sc2 <- sc_vignette[["sc2"]]
        sc2 <- sc2[,c("s2id", "spid", "aeid", "bmad", "max_med", "hitc", "coff")]
        return(sc2)
        }
      else if (lvl == "agg") {
        sc1 <- sc_vignette[["sc1"]]
        sc2 <- sc_vignette[["sc2"]]
        agg <- sc1[sc2, on = c("spid","aeid")]
        agg <- agg[,c("aeid","s2id","s1id","s0id","conc","resp")]
        return(agg)
      }
      else stop("example tables for sc0, sc1, sc2, agg available.")
    }
    
    if (type == "mc" ) {
      data("mc_vignette", envir = environment())
      if (lvl == 0L) {
        mc0 <- mc_vignette[["mc0"]]
        mc0 <- mc0[,c("m0id","spid","acid","apid","rowi","coli","wllt","wllq","conc","rval","srcf")]
        return(mc0)
      }
      else if (lvl == 1L) {
        mc1 <- mc_vignette[["mc1"]]
        mc1 <- mc1[,c("m0id","m1id","spid","acid","apid","rowi","coli","wllt", "wllq","conc","rval","cndx","repi","srcf")]
        return(mc1)
      }
      else if (lvl == 2L) {
        mc2 <- mc_vignette[["mc2"]]
        mc2 <- mc2[,c("m0id","m1id","m2id","spid","acid","apid","rowi","coli","wllt","conc","cval","cndx","repi")]
        return(mc2)
      } 
      else if (lvl == 3L) {
        mc3 <- mc_vignette[["mc3"]]
        mc3 <- mc3[,c("m0id","m1id","m2id","m3id","spid","aeid","conc","resp","cndx","wllt","apid","rowi","coli","repi")]
        return(mc3)
      } 
      else if (lvl == 4L) {
        mc4 <- mc_vignette[["mc4"]]
        if (!add.fld) {
          mc4 <- mc4[,c("m4id", "aeid", "spid", "bmad", "resp_max", "resp_min", 
                        "max_mean", "max_mean_conc", "min_mean", "min_mean_conc", 
                        "max_med", "max_med_conc", "min_med", "min_med_conc", 
                        "max_med_diff", "max_med_diff_conc", "conc_max", "conc_min", 
                        "nconc", "npts", "nrep", "nmed_gtbl_pos", "nmed_gtbl_neg")]
        } else {
          mc4 <- mc4[,!c("chid","casn","chnm","dsstox_substance_id","code","aenm","resp_unit","conc_unit")]
          setcolorder(mc4, c("m4id", "aeid", "spid"))
        }
        return(mc4)
      }
      else if (lvl == 5L) {
        mc5 <- mc_vignette[["mc5"]]
        if (!add.fld){
          mc5 <- mc5[,c("m5id","m4id", "aeid", "spid", "bmad", "resp_max", "resp_min", 
                        "max_mean", "max_mean_conc", "min_mean", "min_mean_conc", 
                        "max_med", "max_med_conc", "min_med", "min_med_conc", 
                        "max_med_diff", "max_med_diff_conc", "conc_max", "conc_min", 
                        "nconc", "npts", "nrep", "nmed_gtbl_pos", "nmed_gtbl_neg",
                        "hitc", "modl", "fitc", "coff")]
        } else {
          mc5 <- mc5[,!c("chid","casn","chnm","dsstox_substance_id","code","aenm",
                         "resp_unit","conc_unit","tp","ga","q","la","ac50_loss")]
          setcolorder(mc5, c("m5id", "m4id","aeid", "spid"))
        }
        return(mc5)
      }
      else if (lvl == "agg") {
        mc3 <- mc_vignette[["mc3"]]
        mc4 <- mc_vignette[["mc4"]]
        agg <- mc3[mc4, on = c("spid","aeid")]
        agg <- agg[, c("aeid", "m4id", "m3id", "m2id", "m1id", "m0id", "spid", 
                       "conc", "resp")]
        return(agg)

      }
      else stop("example tables for mc0, mc1, mc2, mc3, mc4, mc5, agg available.")
    }
    
    else stop("Invalid 'lvl' and 'type' combination.")
  }
  else if (drvr == "API") {
    
    # check type and lvl
    if (type != "mc") stop("Only type = 'mc' is supported using API data as source.")
    # if lvl is outside of 3-6 while not agg either
    if ((lvl < 3 | lvl > 6) & lvl != "agg") stop("Only lvl = c(3,4,5,6) and 'agg' are supported using API data as source.")
    
    cols <- NULL
    if (!add.fld) {
      # load default columns returned regular connection to DB
      data("load_data_columns", envir = environment())
      # combine type and lvl into string, like "mc5"
      table <- paste0(type, lvl)
      # pull regular columns for given table
      cols <- unlist(load_data_columns[table])
    }
    
    # query the API 
    dat <- tcplQueryAPI(fld = fld, val = val, return_flds = cols)
    
    if (lvl == 3 | lvl == "agg") {
      dat$resp <- lapply(dat$resp, unlist)
      dat$logc <- lapply(dat$logc, unlist)
      if (lvl == 3) dat <- unnest_longer(dat, c(conc, logc, resp)) %>% as.data.table() 
      else dat <- unnest_longer(dat, c(logc, resp)) %>% as.data.table() 
    }
    
    if (lvl == 6) {
      dat$flag <- lapply(dat$flag, unlist)
      dat <- unnest_longer(dat, flag) %>% filter(flag != "NULL") %>% as.data.table()
    }

    return(dat)

  }
  else {
    
    # add.fld is not possible if invitrodb version less than 4
    if (!check_tcpl_db_schema()) add.fld <- FALSE
    
    table <- paste0(type, lvl)
    tbls_joins <- case_when(
      table == "sc0" ~ list(tbls = "sc0", 
                            joins = NULL),
      table == "sc1" ~ list(tbls = "sc0,sc1", 
                            joins = "sc0.s0id = sc1.s0id"),
      table == "sc2" ~ list(tbls = "sc2", 
                            joins = NULL),
      table == "scagg" ~ list(tbls = "sc1,sc2_agg", 
                              joins = "sc1.s1id = sc2_agg.s1id"),
      table == "mc0" ~ list(tbls = "mc0", 
                            joins = NULL),
      table == "mc1" ~ list(tbls = "mc0,mc1", 
                            joins = "mc0.m0id = mc1.m0id"),
      table == "mc2" ~ list(tbls = "mc0,mc1,mc2", 
                            joins = "mc0.m0id = mc1.m0id AND mc1.m0id = mc2.m0id"),
      table == "mc3" ~ list(tbls = "mc0,mc1,mc3", 
                            joins = "mc0.m0id = mc1.m0id AND mc1.m0id = mc3.m0id"),
      table == "mcagg" ~ list(tbls = "mc3,mc4,mc4_agg", 
                              joins = "mc3.m3id = mc4_agg.m3id AND mc4.m4id = mc4_agg.m4id"),
      table == "mc4" && add.fld == FALSE ~ list(tbls = "mc4", 
                                                joins = NULL),
      table == "mc4" && add.fld == TRUE ~ list(tbls = "mc4,mc4_param", 
                                               joins = "mc4.m4id = mc4_param.m4id"),
      table == "mc5" && add.fld == FALSE ~ list(tbls = "mc4,mc5", 
                                                joins = "mc4.m4id = mc5.m4id"),
      table == "mc5" && add.fld == TRUE ~ list(tbls = "mc4,mc5,mc5_param", 
                                               joins = "mc4.m4id = mc5.m4id AND mc5.m5id = mc5_param.m5id"),
      table == "mc6" ~ list(tbls = "mc4,mc6", 
                            joins = "mc6.m4id = mc4.m4id"),
      table == "mc7" ~ list(tbls = "mc4,mc7", 
                            joins = "mc7.m4id = mc4.m4id"),
      TRUE ~ list(tbls = NULL, joins = NULL))

    if (is.null(tbls_joins$tbls)) stop("Invalid 'lvl' and 'type' combination.")
    
    qformat <- paste0("SELECT * FROM ", tbls_joins$tbls, ifelse(is.null(tbls_joins$joins), "", paste(" WHERE", tbls_joins$joins)))

    if (!is.null(fld)) {
      if (is.null(val)) stop("'val' cannot be NULL check that a valid value was provided for the specified field")
      
      fld <- .prepField(fld = fld, tbl = unlist(strsplit(tbls_joins$tbls, ",")), db = getOption("TCPL_DB"))
      
      if(add.fld) wtest <- FALSE
      wtest <- lvl %in% c(0) | (lvl == 2 & type == "sc")
      if(lvl == 4){
        if (!check_tcpl_db_schema() || !add.fld) wtest <- TRUE
      }
      
      qformat <- paste(qformat, if (wtest) "WHERE" else "AND")
      
      qformat <- paste0(
        qformat,
        "  ",
        paste(fld, "IN (%s)", collapse = " AND ")
      )
      qformat <- paste0(qformat, ";")
      
      if (!is.list(val)) val <- list(val)
      val <- lapply(val, function(x) paste0("\"", x, "\"", collapse = ","))
      
      qstring <- do.call(sprintf, args = c(qformat, val))
    } else {
      qstring <- qformat
    }
    
    dat <- suppressWarnings(tcplQuery(query = qstring, db = getOption("TCPL_DB"), tbl = tbls))
    
    # remove duplicate columns as a result of joins
    dat <- dat[, which(duplicated(names(dat))) := NULL]
    # remove unnecessary columns from output
    dat <- dat %>% select(-contains(c("created_date", "modified_date", "modified_by", "actp", "tmpi", "bval", "pval", "..")))
    
    # pivot table so 1 id per return and only return added fields
    if(add.fld & check_tcpl_db_schema()){
      if(lvl == 4L)    dat <- as.data.table(tidyr::pivot_wider(dat, names_from = c(model,model_param), values_from = model_val))
      if(lvl == 5L)    dat <- as.data.table(tidyr::pivot_wider(dat, names_from = c(hit_param), values_from = hit_val))
    }
    
    dat[]
  }
  
}

#-------------------------------------------------------------------------------
