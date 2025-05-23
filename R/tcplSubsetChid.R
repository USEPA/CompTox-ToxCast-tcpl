#-------------------------------------------------------------------------------
# tcplSubsetChid: Subset level 5 data to a single sample per chemical
#-------------------------------------------------------------------------------

#' @title Subset level 5 data to a single sample per chemical
#'
#' @description
#' \code{tcplSubsetChid} subsets level 5 data to a single tested sample per
#' chemical. In other words, if a chemical is tested more than once (a chid
#' has more than one spid) for a given assay endpoint, the function uses a
#' series of logic to select a single "representative" sample.
#'
#' @param dat data.table, a data.table with level 5 data
#' @param flag Integer, the mc6_mthd_id values to go into the flag count, see
#' details for more information
#' @param type Character of length 1, the data type, "sc" or "mc"
#' @param export_ready Boolean, default FALSE, should only export ready 1 values be included in calculation
#'
#' @details
#' \code{tcplSubsetChid} is intended to work with level 5 data that has
#' chemical and assay information mapped with \code{\link{tcplPrepOtpt}}.
#'
#' To select a single sample, first a "consensus hit-call" is made by majority
#' rule, with ties defaulting to active. After the chemical-wise hit call is
#' made, the samples corresponding to to chemical-wise hit call are logically
#' ordered using the fit category, the number of the flags, and AC50 (or modl_ga),
#' then the first sample for every chemical is selected.
#'
#' The \code{flag} param can be used to specify a subset of flags to be used in
#' the flag count. Leaving \code{flag} TRUE utilize all the available flags.
#' Setting \code{flag} to \code{FALSE} will do the subsetting without
#' considering any flags.
#'
#' @examples
#' \dontrun{
#' ## Load the example level 5 data
#' d1 <- tcplLoadData(lvl = 5, fld = "aeid", val = 797)
#' d1 <- tcplPrepOtpt(d1)
#'
#' ## Subset to an example of a duplicated chid
#' d2 <- d1[chid == 20182]
#' d2[, list(m4id, hitc, fitc, modl_ga)]
#'
#' ## Here the consensus hit-call is 1 (active), and the fit categories are
#' ## all equal. Therefore, if the flags are ignored, the selected sample will
#' ## be the sample with the lowest modl_ga.
#' tcplSubsetChid(dat = d2, flag = FALSE)[, list(m4id, modl_ga)]
#' }
#'
#' @return A data.table with a single sample for every given chemical-assay
#' pair.
#'
#' @seealso \code{\link{tcplPrepOtpt}}
#'
#' @import data.table
#' @export

tcplSubsetChid <- function(dat, flag = TRUE, type = "mc", export_ready = FALSE) {
  ## Variable-binding to pass R CMD Check
  chit <- hitc <- aeid <- casn <- fitc <- fitc.ordr <- m4id <- nflg <- NULL
  chid <- conc <- minc <- actc <- NULL

  if (!type %in% c("mc", "sc")) {
    stop("type must be sc (single concentration) or mc (multi-concentration)")
  }

  # if only using export ready, filter data by export ready status
  if (export_ready) {
    # try catch here to make sure db has available annotation information
    tryCatch(
      {
        assay_info <- tcplLoadAeid(fld = "aeid", val = dat$aeid, add.fld = "export_ready")
      },
      error = function(error) {
        message(error)
        if (grepl("Not all given fields available in query", error)) stop(paste0("\n'export_ready' column is not available in ", options()$TCPL_DB, ".  Consider setting export_ready to FALSE."))
      }
    )

    # join assay information
    dat <- dat[assay_info, on = .(aeid)]
    # filter export ready column
    dat <- dat[export_ready == 1]
  }

  if (type == "mc") {
    if (!"m5id" %in% names(dat)) {
      stop(
        "'dat' must be a data.table with level 5 data. See ?tcplLoadData for",
        " more information."
      )
    }
    if (!"casn" %in% names(dat)) dat <- tcplPrepOtpt(dat)
    # for now we treat all >=.9 hitcall equally
    dat[, actc := hitc >= .9]
    dat[, chit := mean(actc[actc %in% 0:1]) >= 0.5, by = list(aeid, chid)]
    dat <- dat[actc == chit | (is.na(chit) & (actc == -1 | is.na(m4id)))]


    dat[, fitc.ordr := NA_integer_]
    dat[fitc %in% c(37, 41, 46, 50), fitc.ordr := 0]
    dat[fitc %in% c(38, 42, 47, 51), fitc.ordr := 1]
    dat[fitc %in% c(36, 40, 45, 49), fitc.ordr := 2]
    if (is.null(flag)) flag <- TRUE

    if (flag[1] | length(flag) > 1) {
      tst <- is.logical(flag)
      prs <- if (tst) list() else list(fld = "mc6_mthd_id", val = flag)
      flg <- do.call(tcplLoadData, c(lvl = 6L, prs))
      flg <- flg[, list(nflg = .N), by = m4id]
      setkey(flg, m4id)
      setkey(dat, m4id)

      dat <- flg[dat]
      dat[is.na(nflg), nflg := 0]
    } else {
      dat[, nflg := FALSE]
    }

    # setkeyv(dat, c("aeid", "chid", "fitc.ordr", "nflg", "modl_ga"))
    ac50var <- ifelse(check_tcpl_db_schema(), "ac50", "modl_ga")
    setorderv(dat, c("aeid", "chid", "fitc.ordr", "nflg", ac50var, "max_med"), c(1, 1, 1, 1, 1, -1), na.last = TRUE)

    min_modl_ga <- dat[, list(ind = .I[1]), by = list(aeid, casn)]

    dat <- dat[min_modl_ga$ind]

    return(dat[])
  }

  if (type == "sc") {
    if (!"s2id" %in% names(dat)) {
      stop(
        "'dat' must be a single concentration data.table with level 2 data. See ?tcplLoadData for",
        " more information."
      )
    }
    if (!"casn" %in% names(dat)) dat <- tcplPrepOtpt(dat)

    dat[, chit := mean(hitc[hitc %in% 0:1]) >= 0.5, by = list(aeid, chid)]
    dat <- dat[hitc == chit]

    # select those that are at lowest concentration in the consensus hitc
    dat1 <- tcplLoadData("agg", fld = "s2id", val = dat$s2id, type = "sc")
    setkey(dat1, "s2id")
    setkey(dat, "s2id")
    dat <- dat[dat1]

    concvar <- if ("conc" %in% colnames(dat)) "conc" else "logc"
    setkeyv(dat, c("aeid", "chid", concvar))
    dat[, minc := min(get(concvar)), by = list(aeid, chid)]
    dat <- dat[get(concvar) == minc]
    dat <- unique(dat[, c("spid", "chid", "casn", "chnm", "dsstox_substance_id", "code", "aeid", "aenm", "s2id", "bmad", "max_med", "hitc", "coff", "resp_unit")])
    setkeyv(dat, c("aeid", "chid", "max_med"))

    # select top max_med per casn
    min_max_med <- dat[, list(ind = .I[.N]), by = list(aeid, casn)]

    # filter out all others
    dat <- dat[min_max_med$ind]

    return(dat[])
  }
}

#-------------------------------------------------------------------------------
