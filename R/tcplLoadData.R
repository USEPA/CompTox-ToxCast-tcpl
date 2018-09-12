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
#'
#' @return A data.table containing data for the given fields.
#'
#' @seealso \code{\link{tcplQuery}}, \code{\link{data.table}}
#'
#' @import data.table
#' @export

tcplLoadData <- function(lvl, fld = NULL, val = NULL, type = "mc") {

  if (length(lvl) > 1 | length(type) > 1) {
    stop("'lvl' & 'type' must be of length 1.")
  }

  tbls <- NULL

  if (lvl == 0L && type == "mc") {

    tbls <- c("mc0")

    qformat <-
      "
      SELECT
        m0id,
        spid,
        acid,
        apid,
        rowi,
        coli,
        wllt,
        wllq,
        conc,
        rval,
        srcf
      FROM
        mc0
      "

  }

  if (lvl == 0L && type == "sc") {

    tbls <- c("sc0")

    qformat <-
      "
      SELECT
        s0id,
        spid,
        acid,
        apid,
        rowi,
        coli,
        wllt,
        wllq,
        conc,
        rval,
        srcf
      FROM
        sc0
      "

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

    tbls <- c("mc3", "mc4_agg")

    qformat <-
      "
      SELECT
        mc4_agg.aeid,
        mc4_agg.m4id,
        mc4_agg.m3id,
        mc4_agg.m2id,
        mc4_agg.m1id,
        mc4_agg.m0id,
        logc,
        resp
      FROM
        mc3,
        mc4_agg
      WHERE
        mc3.m3id = mc4_agg.m3id
      "

  }

  if (lvl == 4L && type == "mc") {

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

  if (lvl == 5L && type == "mc") {

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

    fld <- .prepField(fld = fld, tbl = tbls, db = getOption("TCPL_DB"))

    wtest <- lvl %in% c(0, 4) | (lvl == 2 & type == "sc")
    qformat <- paste(qformat, if (wtest) "WHERE" else "AND")

    qformat <- paste0(qformat,
                      "  ",
                      paste(fld, "IN (%s)", collapse = " AND "))
    qformat <- paste0(qformat, ";")

    if (!is.list(val)) val <- list(val)
    val <- lapply(val, function(x) paste0("\"", x, "\"", collapse = ","))

    qstring <- do.call(sprintf, args = c(qformat, val))

  } else {

    qstring <- qformat

  }

  dat <- suppressWarnings(tcplQuery(query = qstring, db = getOption("TCPL_DB"), tbl=tbls))

  dat[]

}

#-------------------------------------------------------------------------------
