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
#' @importFrom tidyr pivot_wider
#' @export

tcplLoadData <- function(lvl, fld = NULL, val = NULL, type = "mc", add.fld = TRUE) {
  #variable binding
  model <- model_param <- model_val <- NULL
  hit_param <- hit_val <- NULL
  
  if (length(lvl) > 1 | length(type) > 1) {
    stop("'lvl' & 'type' must be of length 1.")
  }
  
  tbls <- NULL
  
  drvr <- getOption("TCPL_DRVR")
  if (drvr == "example"){
    if (type == "sc"){
      if (lvl == 0L) {
        sc0 <- sc_vignette[["sc0"]]
        sc0 <- sc0[,c("s0id","spid","acid","apid","rowi","coli","wllt","wllq","conc","rval","srcf")]
        return(sc0)
      }
      else if (lvl == 1L) {
        sc1 <- sc_vignette[["sc1"]]
        sc1 <- sc1[,c("s0id","s1id","spid","acid","aeid","apid","rowi","coli","wllt","logc","resp")]
        return(sc1)
      }
      else if (lvl == 2L) {
        sc2 <- sc_vignette[["sc2"]]
        sc2 <- sc2[,c("s2id", "spid", "aeid", "bmad", "max_med", "hitc", "coff")]
        return(sc2)
      } 
      else stop("example tables for sc0, sc1, sc2 available.")
    }
    
    if (type == "mc" ) {
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
        mc3 <- mc3[,c("m0id","m1id","m2id","m3id","spid","aeid","logc","resp","cndx","wllt","apid","rowi","coli","repi")]
        return(mc3)
      } 
      else if (lvl == 4L) {
        mc4 <- mc_vignette[["mc4"]]
        if (!add.fld) {
          mc4 <- mc4[,c("m4id","aeid","spid","bmad","resp_max","resp_min","max_mean","max_mean_conc","max_med","max_med_conc",
                        "logc_max","logc_min","nconc","npts","nrep","nmed_gtbl")]
        } else {
          mc4 <- mc4[,!c("chid","casn","chnm","dsstox_substance_id","code","aenm","resp_unit","conc_unit")]
          setcolorder(mc4, c("m4id", "aeid", "spid"))
        }
        return(mc4)
      }
      else if (lvl == 5L) {
        mc5 <- mc_vignette[["mc5"]]
        if (!add.fld){
          mc5 <- mc5[,c("m5id","m4id","aeid","spid","bmad","resp_max","resp_min","max_mean","max_mean_conc","max_med",
                        "max_med_conc","logc_max","logc_min","nconc","npts","nrep","nmed_gtbl","hitc","modl","fitc","coff")]
        } else {
          mc5 <- mc5[,!c("chid","casn","chnm","dsstox_substance_id","code","aenm","resp_unit","conc_unit","tp","ga","q","la","ac50_loss")]
          setcolorder(mc5, c("m5id", "m4id","aeid", "spid"))
        }
        return(mc5)
      }
      else stop("example tables for mc0, mc1, mc2, mc3, mc4, mc5 available.")
    }
    
  else stop("Invalid 'lvl' and 'type' combination.")
    }
  
  if (drvr != "example"){
    
    if (lvl == 0L && type == "mc") {
      tbls <- c("mc0")
      cols <- c(
        "m0id",
        "spid",
        "acid",
        "apid",
        "rowi",
        "coli",
        "wllt",
        "wllq",
        "conc",
        "rval",
        "srcf"
      )
      
      if (check_tcpl_db_schema()) {
        cols <- c(cols, "clowder_uid", "git_hash")
      }
      
      col_str <- paste0(cols, collapse = ",")
      qformat <- paste0("SELECT ", col_str, " FROM mc0 ")
    }
    
    if (lvl == 0L && type == "sc") {
      tbls <- c("sc0")
      
      cols <- c(
        "s0id",
        "spid",
        "acid",
        "apid",
        "rowi",
        "coli",
        "wllt",
        "wllq",
        "conc",
        "rval",
        "srcf"
      )
      
      if (check_tcpl_db_schema()) {
        cols <- c(cols, "clowder_uid", "git_hash")
      }
      
      col_str <- paste0(cols, collapse = ",")
      qformat <- paste0("SELECT ", col_str, " FROM sc0 ")
    }
    
    if (lvl == 1L && type == "mc") {
      tbls <- c("mc0", "mc1")
      
      qformat <-
        "
      SELECT
        mc1.m0id,
        m1id,
        spid,
        mc1.acid,
        apid,
        rowi,
        coli,
        wllt,
        wllq,
        conc,
        rval,
        cndx,
        repi,
        srcf
      FROM
        mc0,
        mc1
      WHERE
        mc0.m0id = mc1.m0id
      "
    }
    
    if (lvl == 1L && type == "sc") {
      tbls <- c("sc0", "sc1")
      
      qformat <-
        "
      SELECT
        sc1.s0id,
        s1id,
        spid,
        sc1.acid,
        aeid,
        apid,
        rowi,
        coli,
        wllt,
        logc,
        resp
      FROM
        sc0,
        sc1
      WHERE
        sc0.s0id = sc1.s0id
      "
    }
    
    if (lvl == 2L && type == "mc") {
      tbls <- c("mc0", "mc1", "mc2")
      
      qformat <-
        "
      SELECT
        mc2.m0id,
        mc2.m1id,
        m2id,
        spid,
        mc2.acid,
        apid,
        rowi,
        coli,
        wllt,
        conc,
        cval,
        cndx,
        repi
      FROM
        mc0,
        mc1,
        mc2
      WHERE
        mc0.m0id = mc1.m0id
        AND
        mc1.m0id = mc2.m0id
      "
    }
    
    if (lvl == 2L && type == "sc") {
      tbls <- c("sc2")
      
      qformat <-
        "
      SELECT
        s2id,
        spid,
        aeid,
        bmad,
        max_med,
        hitc,
        coff
      FROM
        sc2
      "
    }
    
    if (lvl == "agg" && type == "sc") {
      tbls <- c("sc1", "sc2_agg")
      
      qformat <-
        "
      SELECT
        sc2_agg.aeid,
        sc2_agg.s2id,
        sc2_agg.s1id,
        sc2_agg.s0id,
        logc,
        resp
      FROM
        sc1,
        sc2_agg
      WHERE
        sc1.s1id = sc2_agg.s1id
      "
    }
    
    if (lvl == 3L && type == "mc") {
      tbls <- c("mc0", "mc1", "mc3")
      
      qformat <-
        "
      SELECT
        mc3.m0id,
        mc3.m1id,
        mc3.m2id,
        m3id,
        spid,
        aeid,
        logc,
        resp,
        cndx,
        wllt,
        apid,
        rowi,
        coli,
        repi
      FROM
        mc0,
        mc1,
        mc3
      WHERE
        mc0.m0id = mc1.m0id
        AND
        mc1.m0id = mc3.m0id
      "
    }
    
    if (lvl == "agg" && type == "mc") {
      tbls <- c("mc3", "mc4_agg", "mc4")
      
      qformat <-
        "
      SELECT
        mc4_agg.aeid,
        mc4_agg.m4id,
        mc4_agg.m3id,
        mc4_agg.m2id,
        mc4_agg.m1id,
        mc4_agg.m0id,
        mc4.spid,
        logc,
        resp
      FROM
        mc3,
        mc4_agg,
        mc4
      WHERE
        mc3.m3id = mc4_agg.m3id 
        AND
        mc4.m4id = mc4_agg.m4id
      "
    }
    if (lvl == 4L && type == "mc" && check_tcpl_db_schema()) {
      tbls <- c("mc4")
      if (!add.fld) {
        qformat <-
          "
      SELECT
        m4id,
        aeid,
        spid,
        bmad,
        resp_max,
        resp_min,
        max_mean,
        max_mean_conc,
        max_med,
        max_med_conc,
        logc_max,
        logc_min,
        nconc,
        npts,
        nrep,
        nmed_gtbl
        FROM
        mc4
        "
      } else {
        tbls <- c("mc4", "mc4_param")
        qformat <-
          "
      SELECT
        mc4.m4id,
        mc4.aeid,
        spid,
        bmad,
        resp_max,
        resp_min,
        max_mean,
        max_mean_conc,
        max_med,
        max_med_conc,
        logc_max,
        logc_min,
        nconc,
        npts,
        nrep,
        nmed_gtbl,
        model,
        model_param,
        model_val
        FROM
        mc4_param,
        mc4
      WHERE
        mc4.m4id = mc4_param.m4id 
        "
      }
    } else if (lvl == 4L && type == "mc") {
      tbls <- c("mc4")
      
      qformat <-
        "
      SELECT
        m4id,
        aeid,
        spid,
        bmad,
        resp_max,
        resp_min,
        max_mean,
        max_mean_conc,
        max_med,
        max_med_conc,
        logc_max,
        logc_min,
        cnst,
        hill,
        hcov,
        gnls,
        gcov,
        cnst_er,
        cnst_aic,
        cnst_rmse,
        cnst_prob,
        hill_tp,
        hill_tp_sd,
        hill_ga,
        hill_ga_sd,
        hill_gw,
        hill_gw_sd,
        hill_er,
        hill_er_sd,
        hill_aic,
        hill_rmse,
        hill_prob,
        gnls_tp,
        gnls_tp_sd,
        gnls_ga,
        gnls_ga_sd,
        gnls_gw,
        gnls_gw_sd,
        gnls_la,
        gnls_la_sd,
        gnls_lw,
        gnls_lw_sd,
        gnls_er,
        gnls_er_sd,
        gnls_aic,
        gnls_rmse,
        gnls_prob,
        nconc,
        npts,
        nrep,
        nmed_gtbl
      FROM
        mc4
      "
    }
    
    if (lvl == 5L && type == "mc" && check_tcpl_db_schema()) {
      if (!add.fld) {
        tbls <- c("mc4", "mc5")
        
        qformat <-
          "
      SELECT
        m5id,
        mc5.m4id,
        mc5.aeid,
        spid,
        bmad,
        resp_max,
        resp_min,
        max_mean,
        max_mean_conc,
        max_med,
        max_med_conc,
        logc_max,
        logc_min,
        nconc,
        npts,
        nrep,
        nmed_gtbl,
        hitc,
        modl,
        fitc,
        coff
      FROM
        mc4,
        mc5
      WHERE
        mc4.m4id = mc5.m4id
      "
      } else {
        tbls <- c("mc4", "mc5", "mc5_param")
        qformat <-
          "
      SELECT
        mc5.m5id,
        mc5.m4id,
        mc5.aeid,
        spid,
        bmad,
        resp_max,
        resp_min,
        max_mean,
        max_mean_conc,
        max_med,
        max_med_conc,
        logc_max,
        logc_min,
        nconc,
        npts,
        nrep,
        nmed_gtbl,
        hitc,
        modl,
        fitc,
        coff,
        hit_param,
        hit_val
      FROM
        mc4,
        mc5,
        mc5_param
      WHERE
        mc4.m4id = mc5.m4id
      AND
        mc5.m5id = mc5_param.m5id
        "
      }
    } else if (lvl == 5L && type == "mc") {
      tbls <- c("mc4", "mc5")
      
      qformat <-
        "
      SELECT
        m5id,
        mc5.m4id,
        mc5.aeid,
        spid,
        bmad,
        resp_max,
        resp_min,
        max_mean,
        max_mean_conc,
        max_med,
        max_med_conc,
        logc_max,
        logc_min,
        cnst,
        hill,
        hcov,
        gnls,
        gcov,
        cnst_er,
        cnst_aic,
        cnst_rmse,
        cnst_prob,
        hill_tp,
        hill_tp_sd,
        hill_ga,
        hill_ga_sd,
        hill_gw,
        hill_gw_sd,
        hill_er,
        hill_er_sd,
        hill_aic,
        hill_rmse,
        hill_prob,
        gnls_tp,
        gnls_tp_sd,
        gnls_ga,
        gnls_ga_sd,
        gnls_gw,
        gnls_gw_sd,
        gnls_la,
        gnls_la_sd,
        gnls_lw,
        gnls_lw_sd,
        gnls_er,
        gnls_er_sd,
        gnls_aic,
        gnls_rmse,
        gnls_prob,
        nconc,
        npts,
        nrep,
        nmed_gtbl,
        hitc,
        modl,
        fitc,
        coff,
        actp,
        modl_er,
        modl_tp,
        modl_ga,
        modl_gw,
        modl_la,
        modl_lw,
        modl_rmse,
        modl_prob,
        modl_acc,
        modl_acb,
        modl_ac10
      FROM
        mc4,
        mc5
      WHERE
        mc4.m4id = mc5.m4id
      "
    }
    
    if (lvl == 6L && type == "mc") {
      tbls <- c("mc4", "mc6")
      
      qformat <-
        "
      SELECT
        mc6.aeid,
        m6id,
        mc6.m4id,
        m5id,
        spid,
        mc6_mthd_id,
        flag,
        fval,
        fval_unit
      FROM
        mc4,
        mc6
      WHERE
        mc6.m4id = mc4.m4id
      "
    }
    
    if (lvl == 7L && type == "mc") {
      tbls <- c("mc7")
      
      qformat <-
        "
    SELECT
    mc7.*
    FROM
      mc4,
      mc7
    WHERE
      mc7.m4id = mc4.m4id
    "
    }
    
    if (is.null(tbls)) stop("Invalid 'lvl' and 'type' combination.")
    
    if (!is.null(fld)) {
      if (is.null(val)) stop("'val' cannot be NULL check that a valid value was provided for the specified field")
      
      #Check if where clause was used before this and append appropriate clause
      qformat <- paste(qformat, if (!grepl("where",tolower(qformat))) "WHERE" else "AND")
      
      #Add fld(s) as parameters to sql query
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
    
    # pivot table so 1 id per return and only return added fields
    if(add.fld & check_tcpl_db_schema()){
      if(lvl == 4L)    dat <- as.data.table(tidyr::pivot_wider(dat, names_from = c(model,model_param), values_from = model_val))
      if(lvl == 5L)    dat <- as.data.table(tidyr::pivot_wider(dat, names_from = c(hit_param), values_from = hit_val))
    }
    
    dat[]
  }
  
}

#-------------------------------------------------------------------------------
