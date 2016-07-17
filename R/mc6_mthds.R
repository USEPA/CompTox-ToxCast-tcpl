#-------------------------------------------------------------------------------
# mc6_mthds: Load list of flag methods (to be used at level 6)
#-------------------------------------------------------------------------------

#' @name MC6_Methods
#' @title Load list of level 6 multiple-concentration flag methods
#' 
#' @description 
#' \code{mc6_mthds} returns a list of flag methods to be used 
#' during level 6 multiple-concentration processing.
#' 
#' @return A list functions
#' 
#' @seealso \code{\link{mc6}}, \code{\link{Method functions}} to query what
#' methods get applied to each aeid
#' 
#' @section Available Methods:
#' 
#' More information about the level 6 multiple-concentration processing is 
#' available in the package vignette, "Pipeline_Overview."
#' 
#' \describe{
#'   \item{row.dev.up}{The row.dev.up flag looks at the individual point data,
#'   searching for row effects across an apid. To get flagged the point has to
#'   be greater than 3 standard deviations above the mean response for the
#'   plate, and the row mean must be greater than 3 standard deviations above
#'   the row means for the plate.}
#'   \item{row.dev.dn}{The row.dev.dn flag is identical to the row.dev.up flag, 
#'   but identifies points falling in rows with decreased signals.}
#'   \item{col.dev.up}{The col.dev.up flag is identical to the row.dev.up flag, 
#'   but identifies points falling in columns with increased signals.}
#'   \item{col.dev.dn}{The col.dev.up flag is identical to the row.dev.up flag, 
#'   but identifies points falling in columns with decreased signals.}
#'   \item{plate.flare}{The plate.flare flag looks at the individual point data,
#'   searching for overly active regions across an apid. Intended for use in
#'   fluorometric assays that are read by a plate-reader that measures the 
#'   plate as a whole, rather than measuring individual wells. For each well
#'   the flare value is calculated as a weighted mean a 5 well by 5 well box 
#'   centered on the well where the weight given to each well in the box is the 
#'   euclidian distance from the center well. The flag then identifies points 
#'   with flare values greater than 3 standard deviations above the mean flare 
#'   values for the plate.}
#'   \item{plate.interlace}{The plate.interlace flag is specific to one 
#'   experimental design that plates chemicals from a 386 well chemical plate 
#'   to a 1536 well assay plate. The flag looks for any chemical-plate affects,
#'   by looking for an increased signal in the wells originating from the same
#'   chemical plate.}
#'   \item{rep.mismatch}{The rep.mismatch flag is still in development and is
#'   not suggested for use at this time.}
#'   \item{pintool}{Deprecated. The pintool flag uses a complicated algorithm 
#'   to look for signal potentially caused by residual in the pintool used to 
#'   deliver the chemical to assay plates in some experimental designs. The 
#'   gnls.lowconc is a faster and simpler way to identify where this problem 
#'   may be driving the activity or hit-call.}
#'   \item{singlept.hit.high}{The singlept.hit.high flag identifies 
#'   concentration series where the median response was greater than 3*bmad 
#'   only at the highest tested concentration and the series had an active 
#'   hit-call.}
#'   \item{singlept.hit.mid}{The singlept.hit.mid flag identifies concentration 
#'   series where the median response was greater than 3*bmad at only one 
#'   concentration (not the highest tested concentration) and the series had 
#'   an active hit-call.}
#'   \item{multipoint.neg}{The multipoint.neg flag identifies concentration 
#'   series with response medians greater than 3*bmad at multiple 
#'   concentrations and an inactive hit-call.}
#'   \item{gnls.lowconc}{The gnls.lowconc flag identifies concentration series
#'   where the gain-loss model won, the gain AC50 is less than the minimum 
#'   tested concentration, and the loss AC50 is less than the mean tested 
#'   concentration.}
#'   \item{noise}{The noise flag attempts to identify noisy concentration
#'   series by flagging series where the root mean square error for the series
#'   is greater than the cutoff for the assay endpoint.}
#'   \item{border.hit}{The border.hit flag identifies active concentration 
#'   series where the top parameter of the winning model was less than or equal 
#'   to 1.2*cut-off or the the activity probablity was less than 0.9.}
#'   \item{border.miss}{The border.miss flag identifies inactive concentration
#'   series where either the Hill or gain-loss top parameter was greater than 
#'   or equal to 0.8*cut-off and the activity probability was greater than 0.5.}
#'   \item{overfit.hit}{The overfit.hit flag recalculates the model winner 
#'   after applying a small sample correction factor to the AIC values. If the 
#'   hit-call would be changed after applying the small sample correction 
#'   factor the series is flagged. Series with less than 5 concentrations where
#'   the hill model won and series with less than 7 concentrations where the 
#'   gain-loss model won are automatically flagged.}
#'   \item{efficacy.50}{The efficacy.50 flag identifies concentration series 
#'   with efficacy values (either the modeled top parameter for the winning
#'   model or the maximum median response) are less than 50. Intended for use
#'   with biochemical assays where one might expect at least a 50\% change in 
#'   real responses.}
#' }

mc6_mthds <- function() {
  
  list(
    
    row.dev.up = function(mthd) {
      
      flag <- "Row-wise effect, increased signal"
      out  <- c("m5id", "m4id", "aeid", "mc6_mthd_id", 
                "flag", "fval", "fval_unit")
      init <- bquote(list(.(mthd), .(flag), NA_real_, "proportion", FALSE))
      e1 <- bquote(dr[ , .(c(out[4:7], "test")) := .(init), with = FALSE])
      e2 <- bquote(dr[wllt == "t", 
                      rowm := mean(resp, na.rm = TRUE), 
                      by = list(apid, rowi)])
      e3 <- bquote(dr[wllt == "t", 
                      athd := resp > mean(resp) + 3*sd(resp), by = apid])
      e4 <- bquote(dr[wllt == "t", 
                      test := rowm > mean(rowm) + 3*sd(rowm) & athd,
                      by = apid])
      e5 <- bquote(dr[ , fval := lw(test)/.N, by = m4id])
      e6 <- bquote(dr[fval < 0.25, test := FALSE])
      e7 <- bquote(f[[.(mthd)]] <- dr[which(test), 
                                      unique(.SD), 
                                      .SDcols = .(out)])
      cr <- c("mc6_mthd_id", "flag", "fval", "fval_unit", "test", "rowm", 
              "athd")
      e8 <- bquote(dr[ , .(cr) := NULL, with = FALSE])
      list(e1, e2, e3, e4, e5, e6, e7, e8)
      
    },
    
    row.dev.dn = function(mthd) {
      
      flag <- "Row-wise effect, decreased signal"
      out  <- c("m5id", "m4id", "aeid", "mc6_mthd_id", 
                "flag", "fval", "fval_unit")
      init <- bquote(list(.(mthd), .(flag), NA_real_, "proportion", FALSE))
      e1 <- bquote(dr[ , .(c(out[4:7], "test")) := .(init), with = FALSE])
      e2 <- bquote(dr[wllt == "t", 
                      rowm := mean(resp, na.rm = TRUE), 
                      by = list(apid, rowi)])
      e3 <- bquote(dr[wllt == "t", 
                      athd := resp < mean(resp) - 3*sd(resp), by = apid])
      e4 <- bquote(dr[wllt == "t", 
                      test := rowm < mean(rowm) - 3*sd(rowm) & athd,
                      by = apid])
      e5 <- bquote(dr[ , fval := lw(test)/.N, by = m4id])
      e6 <- bquote(dr[fval < 0.25, test := FALSE])
      e7 <- bquote(f[[.(mthd)]] <- dr[which(test), 
                                      unique(.SD), 
                                      .SDcols = .(out)])
      cr <- c("mc6_mthd_id", "flag", "fval", "fval_unit", "test", "rowm", 
              "athd")
      e8 <- bquote(dr[ , .(cr) := NULL, with = FALSE])
      list(e1, e2, e3, e4, e5, e6, e7, e8)
      
    },
    
    col.dev.up = function(mthd) {
      
      flag <- "Col-wise effect, increased signal"
      out  <- c("m5id", "m4id", "aeid", "mc6_mthd_id", 
                "flag", "fval", "fval_unit")
      init <- bquote(list(.(mthd), .(flag), NA_real_, "proportion", FALSE))
      e1 <- bquote(dr[ , .(c(out[4:7], "test")) := .(init), with = FALSE])
      e2 <- bquote(dr[wllt == "t", 
                      colm := mean(resp, na.rm = TRUE), 
                      by = list(apid, coli)])
      e3 <- bquote(dr[wllt == "t", 
                      athd := resp > mean(resp) + 3*sd(resp), by = apid])
      e4 <- bquote(dr[wllt == "t", 
                      test := colm > mean(colm) + 3*sd(colm) & athd,
                      by = apid])
      e5 <- bquote(dr[ , fval := lw(test)/.N, by = m4id])
      e6 <- bquote(dr[fval < 0.25, test := FALSE])
      e7 <- bquote(f[[.(mthd)]] <- dr[which(test), 
                                      unique(.SD), 
                                      .SDcols = .(out)])
      cr <- c("mc6_mthd_id", "flag", "fval", "fval_unit", "test", "colm", 
              "athd")
      e8 <- bquote(dr[ , .(cr) := NULL, with = FALSE])
      list(e1, e2, e3, e4, e5, e6, e7, e8)
      
    },
    
    col.dev.dn = function(mthd) {
      
      flag <- "Col-wise effect, decreased signal"
      out  <- c("m5id", "m4id", "aeid", "mc6_mthd_id", 
                "flag", "fval", "fval_unit")
      init <- bquote(list(.(mthd), .(flag), NA_real_, "proportion", FALSE))
      e1 <- bquote(dr[ , .(c(out[4:7], "test")) := .(init), with = FALSE])
      e2 <- bquote(dr[wllt == "t", 
                      colm := mean(resp, na.rm = TRUE), 
                      by = list(apid, coli)])
      e3 <- bquote(dr[wllt == "t", 
                      athd := resp < mean(resp) - 3*sd(resp), by = apid])
      e4 <- bquote(dr[wllt == "t", 
                      test := colm < mean(colm) - 3*sd(colm) & athd,
                      by = apid])
      e5 <- bquote(dr[ , fval := lw(test)/.N, by = m4id])
      e6 <- bquote(dr[fval < 0.25, test := FALSE])
      e7 <- bquote(f[[.(mthd)]] <- dr[which(test), 
                                      unique(.SD), 
                                      .SDcols = .(out)])
      cr <- c("mc6_mthd_id", "flag", "fval", "fval_unit", "test", "colm", 
              "athd")
      e8 <- bquote(dr[ , .(cr) := NULL, with = FALSE])
      list(e1, e2, e3, e4, e5, e6, e7, e8)
      
    },
    
    plate.flare = function(mthd) {
      
      flag <- "Includes potential flare region points"
      out  <- c("m5id", "m4id", "aeid", "mc6_mthd_id", 
                "flag", "fval", "fval_unit")
      init <- bquote(list(.(mthd), .(flag), NA_real_, "proportion", FALSE))
      e1 <- bquote(dr[ , .(c(out[4:7], "test")) := .(init), with = FALSE])
      e2 <- bquote(dr[wllt == "t",
                      flrv := flareFunc(resp, coli, rowi, apid, r = 4)])
      e3 <- bquote(dr[wllt == "t", 
                      test := flrv > mean(flrv) + 3*sd(flrv)])
      e4 <- bquote(dr[ , fval := lw(test)/.N, by = m4id])
      e5 <- bquote(dr[fval < 0.25, test := FALSE])
      e6 <- bquote(f[[.(mthd)]] <- dr[wllt == "t" & test & fval > 0.1, 
                                      unique(.SD), 
                                      .SDcols = .(out)])
      cr <- c("mc6_mthd_id", "flag", "fval", "fval_unit", "test", "flrv")
      e7 <- bquote(dr[ , .(cr) := NULL, with = FALSE])
      list(e1, e2, e3, e4, e5, e6, e7)
      
    },
    
    plate.interlace = function(mthd) {
           
    flag <- "Includes potential chemical plate interlace points"
    out  <- c("m5id", "m4id", "aeid", "mc6_mthd_id", 
              "flag", "fval", "fval_unit")
    init <- bquote(list(.(mthd), .(flag), NA_real_, "proportion", FALSE))
    e1 <- bquote(dr[ , .(c(out[4:7], "test")) := .(init), with = FALSE])
    e2 <- bquote(dr[wllt == "t" & is.odd(coli) & is.odd(rowi),  intq := 1])
    e3 <- bquote(dr[wllt == "t" & !is.odd(coli) & is.odd(rowi),  intq := 2])
    e4 <- bquote(dr[wllt == "t" & is.odd(coli) & !is.odd(rowi),  intq := 3])
    e5 <- bquote(dr[wllt == "t" & !is.odd(coli) & !is.odd(rowi), intq := 4])
    e6 <- bquote(dr[wllt =="t", colj := ceiling(coli/2)])
    e7 <- bquote(dr[wllt =="t", rowj := ceiling(rowi/2)])
    e8 <- bquote(dr[wllt == "t",
                    intv := interlaceFunc(val = resp,
                                          intq = intq, 
                                          coli = colj, 
                                          rowi = rowj, 
                                          apid = apid, 
                                          r = 3)])
    e9 <- bquote(dr[wllt == "t", test := (intv > mean(intv) + 3*sd(intv))])
    e10 <- bquote(dr[ , fval := lw(test)/.N, by = m4id])
    e11 <- bquote(dr[fval < 0.25, test := FALSE])
    e12 <- bquote(f[[.(mthd)]] <- dr[wllt == "t" & test & fval > 0.1, 
                                     unique(.SD), 
                                     .SDcols = .(out)])
    cr <- c("mc6_mthd_id", "flag", "fval", "fval_unit", "test", 
            "intv", "intq", "colj", "rowj")
    e13 <- bquote(dr[ , .(cr) := NULL, with = FALSE])
    list(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13)
           
    },
    
        
    rep.mismatch = function(mthd) {
      
      flag <- "Replicate mismatch"
      out  <- c("m5id", "m4id", "aeid", "mc6_mthd_id", 
                "flag", "fval", "fval_unit")
      init <- bquote(list(.(mthd), .(flag), NA_real_, "proportion", FALSE))
      e1 <- bquote(dr[ , .(c(out[4:7], "test")) := .(init), with = FALSE])
      e2  <- bquote(dr[wllt == "t",
                       c("l4mn", "N") := list(mean(resp), .N), by = m4id])
      e3  <- bquote(dr[wllt == "t",
                       excl.repmn := (l4mn * N - sum(resp)) / (N - .N), 
                       by = list(m4id, repi)]) 
      e4  <- bquote(dr[wllt == "t", repmn := mean(resp), by = list(m4id, repi)])
      e5  <- bquote(dr[wllt == "t", repsd := sd(resp), by = list(m4id, repi)])
      e6  <- bquote(dr[wllt == "t", t1 := excl.repmn > repmn + 2*repsd])
      e7  <- bquote(dr[wllt == "t", t2 := excl.repmn < repmn - 2*repsd])
      e8  <- bquote(dr[wllt == "t", test := t1 | t2])
      e9  <- bquote(dr[ , fval := lw(test)/.N, by = m4id])
      e10 <- bquote(f[[.(mthd)]] <- dr[which(test), 
                                      unique(.SD), 
                                      .SDcols = .(out)])
      cr <- c("mc6_mthd_id", "flag", "fval", "fval_unit", "test", "l4mn", "N", 
              "excl.repmn", "repsd", "repmn", "t1", "t2")
      e11 <- bquote(dr[ , .(cr) := NULL, with = FALSE])
      list(e1, e2, e3, e4, e5, e6, e7, e8, e9)
      
    },
    
    pintool = function(mthd) {
      
      flag <- "Potential pintool carryover"
      out  <- c("m5id", "m4id", "aeid", "mc6_mthd_id", 
                "flag", "fval", "fval_unit")
      init <- bquote(list(.(mthd), .(flag), NA_real_, "proportion", FALSE))
      e1 <- bquote(dr[ , .(c(out[4:7], "test")) := .(init), with = FALSE])
      e2  <- bquote(dr[wllt == "t",
                       t1 := max(resp[cndx == 1]) > 9*bmad,
                       by = list(m4id, repi)])
      e3  <- bquote(dr[wllt == "t",
                       t2 := max(resp[cndx == 2]) > 4.5*bmad, 
                       by = list(m4id, repi)])
      e4  <- bquote(dr[wllt == "t",
                       t3 := min(resp[cndx %in% 3:7]) < bmad,
                       by = list(m4id, repi)])
      e5  <- bquote(dr[wllt == "t", incl := t1 & t2 & t3])
      e6  <- bquote(setkey(dr, cndx)) 
      e7  <- bquote(dr[wllt == "t" & incl,
                       test := all(resp[cndx[1:3]] - resp[cndx[2:4]] > 0),
                       by = list(m4id, repi)])
      e8  <- bquote(dr[ , fval := lw(test)/.N, by = m4id])
      e9  <- bquote(f[[.(mthd)]] <- dr[which(test), 
                                       unique(.SD), 
                                       .SDcols = .(out)])
      cr <- c("mc6_mthd_id", "flag", "fval", "fval_unit", "test", "incl", 
              "t1", "t2", "t3")
      e10 <- bquote(dr[ , .(cr) := NULL, with = FALSE])
      list(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10)
      
    },
    
    singlept.hit.high = function(mthd) {
      
      flag <- "Only highest conc above baseline, active"
      out  <- c("m5id", "m4id", "aeid", "mc6_mthd_id", 
                "flag", "fval", "fval_unit")
      init <- bquote(list(.(mthd), .(flag), NA_real_, NA_character_, FALSE))
      e1 <- bquote(ft[ , .(c(out[4:7], "test")) := .(init), with = FALSE])
      e2 <- bquote(ft[ , lstc := max_med_conc == logc_max])
      e3 <- bquote(ft[ , test := nmed_gtbl == 1 & hitc == 1 & lstc])
      e4 <- bquote(f[[.(mthd)]] <- ft[which(test), .SD, .SDcols = .(out)])
      cr <- c("mc6_mthd_id", "flag", "fval", "fval_unit", "test", "lstc")
      e5 <- bquote(ft[ , .(cr) := NULL, with = FALSE])
      list(e1, e2, e3, e4, e5)
      
    },
    
    singlept.hit.mid = function(mthd) {
      
      flag <- "Only one conc above baseline, active"
      out  <- c("m5id", "m4id", "aeid", "mc6_mthd_id", 
                "flag", "fval", "fval_unit")
      init <- bquote(list(.(mthd), .(flag), NA_real_, NA_character_, FALSE))
      e1 <- bquote(ft[ , .(c(out[4:7], "test")) := .(init), with = FALSE])
      e2 <- bquote(ft[ , lstc := max_med_conc == logc_max])
      e3 <- bquote(ft[ , test := nmed_gtbl == 1 & hitc == 1 & !lstc])
      e4 <- bquote(f[[.(mthd)]] <- ft[which(test), .SD, .SDcols = .(out)])
      cr <- c("mc6_mthd_id", "flag", "fval", "fval_unit", "test", "lstc")
      e5 <- bquote(ft[ , .(cr) := NULL, with = FALSE])
      list(e1, e2, e3, e4, e5)
      
    },
    
    multipoint.neg = function(mthd) {
      
      flag <- "Multiple points above baseline, inactive"
      out  <- c("m5id", "m4id", "aeid", "mc6_mthd_id", 
                "flag", "fval", "fval_unit")
      init <- bquote(list(.(mthd), .(flag), NA_real_, NA_character_, FALSE))
      e1 <- bquote(ft[ , .(c(out[4:7], "test")) := .(init), with = FALSE])
      e2 <- bquote(ft[ , test := nmed_gtbl > 1 & hitc == 0])
      e3 <- bquote(f[[.(mthd)]] <- ft[which(test), .SD, .SDcols = .(out)])
      cr <- c("mc6_mthd_id", "flag", "fval", "fval_unit", "test")
      e4 <- bquote(ft[ , .(cr) := NULL, with = FALSE])
      list(e1, e2, e3, e4)
      
    },
    
    gnls.lowconc = function(mthd) {
      
      flag <- "Gain AC50 < lowest conc & loss AC50 < mean conc"
      out  <- c("m5id", "m4id", "aeid", "mc6_mthd_id", 
                "flag", "fval", "fval_unit")
      init <- bquote(list(.(mthd), .(flag), NA_real_, NA_character_, FALSE))
      e1 <- bquote(ft[ , .(c(out[4:7], "test")) := .(init), with = FALSE])
      conc_cols <- c("logc_max", "logc_min")
      e2 <- bquote(ft[ , cmen := rowMeans(.SD), .SDcols = .(conc_cols)])
      e3 <- bquote(ft[ , test := modl_ga < logc_min & modl_la < cmen])
      e4 <- bquote(f[[.(mthd)]] <- ft[which(test), .SD, .SDcols = .(out)])
      cr <- c("mc6_mthd_id", "flag", "fval", "fval_unit", "test", "cmen")
      e5 <- bquote(ft[ , .(cr) := NULL, with = FALSE])
      list(e1, e2, e3, e4, e5)
      
    },
    
    noise = function(mthd) {
      
      flag <- "Noisy data"
      out  <- c("m5id", "m4id", "aeid", "mc6_mthd_id", 
                "flag", "fval", "fval_unit")
      init <- bquote(list(.(mthd), .(flag), NA_real_, NA_character_, FALSE))
      e1 <- bquote(ft[ , .(c(out[4:7], "test")) := .(init), with = FALSE])
      e2 <- bquote(ft[ , test := modl_rmse > coff])
      e3 <- bquote(f[[.(mthd)]] <- ft[which(test), .SD, .SDcols = .(out)])
      cr <- c("mc6_mthd_id", "flag", "fval", "fval_unit", "test")
      e4 <- bquote(ft[ , .(cr) := NULL, with = FALSE])
      list(e1, e2, e3, e4)
      
    },    
    
    border.hit = function(mthd) {
      
      flag <- "Borderline active"
      out  <- c("m5id", "m4id", "aeid", "mc6_mthd_id", 
                "flag", "fval", "fval_unit")
      init <- bquote(list(.(mthd), .(flag), NA_real_, NA_character_, FALSE))
      e1 <- bquote(ft[ , .(c(out[4:7], "test")) := .(init), with = FALSE])
      e2 <- bquote(ft[ , t1 := actp < 0.9])
      e3 <- bquote(ft[ , t2 := modl_tp <= 1.2*coff | max_med <= 1.2*coff])
      e4 <- bquote(ft[ , test := hitc == 1 & (t1 | t2)])
      e5 <- bquote(f[[.(mthd)]] <- ft[which(test), .SD, .SDcols = .(out)])
      cr <- c("mc6_mthd_id", "flag", "fval", "fval_unit", "test")
      e6 <- bquote(ft[ , .(cr) := NULL, with = FALSE])
      list(e1, e2, e3, e4, e5, e6)
      
    },
    
    border.miss = function(mthd) {
      
      flag <- "Borderline inactive"
      out  <- c("m5id", "m4id", "aeid", "mc6_mthd_id", 
                "flag", "fval", "fval_unit")
      init <- bquote(list(.(mthd), .(flag), NA_real_, NA_character_, FALSE))
      e1 <- bquote(ft[ , .(c(out[4:7], "test")) := .(init), with = FALSE])
      e2 <- bquote(ft[ , tp.8 := gnls_tp >= 0.8*coff | hill_tp >= 0.8*coff])
      e3 <- bquote(ft[ , test := hitc == 0L & actp > 0.5 & tp.8])
      e4 <- bquote(f[[.(mthd)]] <- ft[which(test), .SD, .SDcols = .(out)])
      cr <- c("mc6_mthd_id", "flag", "fval", "fval_unit", "test", "tp.8")
      e5 <- bquote(ft[ , .(cr) := NULL, with = FALSE])
      list(e1, e2, e3, e4, e5)
      
    },
    
    overfit.hit = function(mthd) {
      
      flag <- "Hit-call potentially confounded by overfitting"
      out  <- c("m5id", "m4id", "aeid", "mc6_mthd_id", 
                "flag", "fval", "fval_unit")
      init <- bquote(list(.(mthd), .(flag), NA_real_, NA_character_, FALSE))
      e1  <- bquote(ft[ , .(c(out[4:7], "test")) := .(init), with = FALSE])
      e2  <- bquote(ft[modl == "hill" & npts < 5 & hitc == 1, test := TRUE])
      e3  <- bquote(ft[modl == "gnls" & npts < 7 & hitc == 1, test := TRUE])
      e4  <- bquote(ft[npts > 1, cna := cnst_aic +  4/(npts - 2)])
      e5  <- bquote(ft[npts > 4, hna := hill_aic + 40/(npts - 4)])
      e6  <- bquote(ft[npts > 6, gna := gnls_aic + 84/(npts - 7)])
      e7  <- bquote(ft[ , nma := pmin(cna, hna, gna, na.rm = TRUE)])
      e8  <- bquote(ft[gna == nma, nmdl := "gnls"])
      e9  <- bquote(ft[hna == nma, nmdl := "hill"])
      e10 <- bquote(ft[cna == nma, nmdl := "cnst"])
      e11 <- bquote(ft[ , nhc := FALSE])
      e12 <- bquote(ft[nmdl == "hill" & hill_tp >= coff & max_med >= coff, 
                       nhc := TRUE]) 
      e13 <- bquote(ft[nmdl == "gnls" & gnls_tp >= coff & max_med >= coff, 
                       nhc := TRUE])
      e14 <- bquote(ft[hitc == 1 & !nhc, test := TRUE])
      e15 <- bquote(f[[.(mthd)]] <- ft[which(test), .SD, .SDcols = .(out)])
      cr <- c("mc6_mthd_id", "flag", "fval", "fval_unit", "test", 
              "cna", "hna", "gna", "nma", "nmdl", "nhc")
      e16 <- bquote(ft[ , .(cr) := NULL, with = FALSE])
      list(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, 
           e11, e12, e13, e14, e15, e16)
      
    },
    
    efficacy.50 = function(mthd) {
      
      flag <- "Biochemical assay with < 50% efficacy"
      out  <- c("m5id", "m4id", "aeid", "mc6_mthd_id", 
                "flag", "fval", "fval_unit")
      init <- bquote(list(.(mthd), .(flag), NA_real_, NA_character_, FALSE))
      e1 <- bquote(ft[ , .(c(out[4:7], "test")) := .(init), with = FALSE])
      e2 <- bquote(ft[hitc == 1, test := modl_tp < 50 | max_med < 50])
      e3 <- bquote(f[[.(mthd)]] <- ft[which(test), .SD, .SDcols = .(out)])
      cr <- c("mc6_mthd_id", "flag", "fval", "fval_unit", "test")
      e4 <- bquote(ft[ , .(cr) := NULL, with = FALSE])
      list(e1, e2, e3, e4)
      
    }    
    
  )
  
}

#-------------------------------------------------------------------------------
