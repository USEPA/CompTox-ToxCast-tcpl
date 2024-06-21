#-------------------------------------------------------------------------------
# tcplVarMat: Create chemical by assay matrices
#-------------------------------------------------------------------------------

#' @title Create chemical by assay matrices
#' 
#' @description
#' \code{tcplVarMat} creates chemical by assay matrices.
#' 
#' @param dsstox_substance_id Integer, chemical ID values to subset on
#' @param aeid Integer, assay endpoint ID values to subset on
#' @param add.vars Character, mc4 or mc5 field(s) not included in the standard
#' list to add additional matrices 
#' @param flag Integer or Logical of length 1, passed to 
#' \code{\link{tcplSubsetChid}}
#' 
#' @details
#' The \code{tcplVarMat} function is used to create chemical by assay matrices
#' for different parameters. The standard list of matrices returned includes:
#' 
#' \enumerate{
#'  \item "ac50" -- The active concentration at 50% maximal response (ac50) for 
#'  the winning model.
#'  \item "ac50_verbose" -- The AC50 for the winning model, with text describing
#'  some situations.
#'  \item "acc" -- The active concentration at user-defined cutoff for the 
#'  winning model.
#'  \item "acc_verbose" -- The ACC for the winning model, with text describing
#'  some situations.
#'  \item "mc_hitc" -- The hit-call for the winning model in 
#'  multiple-concentration (mc) screening.
#'  \item "sc_hitc" -- The hit-call in single concentration (sc) screening.
#' }
#' 
#' \code{tcplVarMat} produces matrices of combined sc-mc output. For the ac50
#' and acc matrices specifically, values are inserted in place to show complete
#' views of what was tested and what the results were. ac50 and acc values are:
#' \itemize{
#'  \item set to 1e6 when the chemical is tested but negative in mc. In _verbose 
#'  matrices, these are indicated as "MC neg".
#'  \item set to 1e7 when the chemical is not tested in mc but was screened in 
#'  sc with a positive hitcall for the same aeid. In _verbose matrices, these 
#'  are indicated as "SC pos, No MC".
#'  \item set to 1e8 when the chemical is not tested in mc but was screened in 
#'  sc with a negative hitcall for the same aeid. In _verbose matrices, these 
#'  are indicated as "SC neg, No MC"
#'  \item not changed when chemical is tested in mc and positive, or not tested in
#' either mc or sc
#' } 
#' 
#' sc and mc data both are currently required to be included for these 
#' calculations.
#' 
#' To add additional matrices, the 'add.vars' parameter can be used to specify
#' the fields from the mc4 or mc5 tables to create matrices for.
#' 
#' When more than one sample is included for a chemical/assay pair, 
#' \code{tcplVarMat} aggregates multiple samples to a chemical level call 
#' utilizing \code{\link{tcplSubsetChid}}. The input
#' for the \code{tcplVarMat} 'flag' parameter is passed to the 
#' \code{tcplSubsetChid} call and used to parse down the data to create the 
#' matrices.
#' 
#' @return A list of chemical by assay matrices (data.tables) where the 
#' rows are given by the dsstox_substance_id and corresponding chnm (chemical
#' name) columns and the colnames are given by assay endpoint name (aenm).
#' 
#' @examples 
#' \dontrun{
#' ## Demonstrate the returned values.
#' varmat <- tcplVarMat()
#' 
#' ## Other changes can be made
#' aeids <- c(80)
#' dtxsid <- c("DTXSID80379721", "DTXSID10379991", "DTXSID7021106", 
#' "DTXSID1026081", "DTXSID9032589")
#' varmat <- tcplVarMat(aeid = aeids, dsstox_substance_id = dtxsid)
#' varmat <- tcplVarMat(aeid = aeids, add.vars = c("m4id", "resp_max", "max_med"))
#' 
#' ## To save output to file
#' library(writexl)
#' write_xlsx(varmat, path = "varmat_output.xlsx")
#' }
#' 
#' @seealso \code{\link{tcplSubsetChid}}
#' 
#' @import data.table
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr full_join
#' @export

tcplVarMat <- function(dsstox_substance_id = NULL,
                       aeid = NULL,
                       add.vars = NULL,
                       flag = TRUE) {
  
  # check input
  if (!is.null(aeid) & !is.vector(aeid)) stop("'aeid' must be a vector.")
  if (!is.null(dsstox_substance_id) & !is.vector(dsstox_substance_id)) stop("'dsstox_substance_id' must be a vector.")
  
  row.id <- "dsstox_substance_id"

  valid_var <- c(tcplListFlds("mc4"), tcplListFlds("mc5"))

  if (!all(add.vars %in% valid_var)) stop("Invald add.vars value(s).")
  
  ac50str = ifelse(check_tcpl_db_schema(),"ac50","modl_ga")
  
  std.vars <- c(ac50str, paste0(ac50str, "_verbose"), "acc", "acc_verbose", "hitc", "hitc.y")
  vars <- c(std.vars, add.vars)
  
  ## Load all possibilities to create matrix dimensions
  mc <- tcplQuery("SELECT DISTINCT mc5.aeid, spid FROM mc5 inner join mc4 on mc4.m4id = mc5.m4id;")
  sc <- tcplQuery("SELECT DISTINCT aeid, spid FROM sc2;")
  
  tst <- rbindlist(list(sc, mc))
  tst <- unique(tst)
  rm(sc, mc)
  
  ## Subset by aeid
  if (is.null(aeid)) {
    ae <- unique(tst$aeid)
  } else {
    ae <- aeid 
    tst <- tst[aeid %in% ae]
  }
  
  ## Load sc2 and mc5 data
  sc2 <- tcplLoadData(lvl=2,type='sc', fld='aeid',val=ae)
  
  mc5 <- tcplLoadData(lvl = 5, fld = "aeid", val = ae, type = "mc")
  
  if (nrow(sc2) == 0 || nrow(mc5) == 0) {
    stop("Missing sc or mc data. Consider expanding to include more aeids")
  }
  
  sc2 <- tcplSubsetChid(tcplPrepOtpt(sc2), type='sc')[!is.na(dsstox_substance_id)]
  
  # merge mc5 data with all mc/sc spids
  setkeyv(mc5, c("aeid", "spid"))
  setkeyv(tst, c("aeid", "spid"))
  mc5 <- merge(mc5, tst, all = TRUE)
  
  # get chemical/sample information
  mc5 <- tcplPrepOtpt(mc5)
  
  # Subset by dsstox_substance_id
  dtxsid <- dsstox_substance_id
  if (!is.null(dtxsid)) {
    mc5 <- mc5[dsstox_substance_id %in% dtxsid]
    sc2 <- sc2[dsstox_substance_id %in% dtxsid]
  }
  
  # subset to one sample per chemical
  mc5 <- tcplSubsetChid(dat = mc5, flag = flag)    
  
  # build matrices
  mc5 <- mc5[hitc %in% c(0,-1), c("ac50", "acc") := 1e6]
  long_sc2 <- sc2 %>% group_by(dsstox_substance_id,aenm,chnm) %>%
    summarise(hitc = max(hitc)) %>% filter(!is.na(dsstox_substance_id))
  
  build_matrix <- function(var, verbose = FALSE) {
    if (grepl("_verbose", var, fixed = TRUE)) {
      var <- sub("_verbose", "", var)
      verbose = TRUE
    }
    long_mc5 <- mc5 %>% group_by(dsstox_substance_id,aenm,chnm) %>% 
      summarise(across(all_of(sub("\\.y", "", var)), mean)) %>% filter(!is.na(dsstox_substance_id))
    long_all <- long_mc5 %>% full_join(long_sc2, by = c("dsstox_substance_id","aenm", "chnm"))
    long_res <- if (substr(var, 1, 2) == "ac") long_all %>% 
      mutate("{var}" := case_when(is.na(get(var)) && hitc == 0 ~ 1e8, 
                                  is.na(get(var)) && hitc == 1 ~ 1e7, 
                                  TRUE ~ get(var)),
             "{var}_verbose" := case_when(get(var) == 1e8 ~ "SC neg, No MC", 
                                          get(var) == 1e7 ~ "SC pos, No MC", 
                                  get(var) == 1e6 ~ "MC neg",
                                  TRUE ~ toString(get(var)))) else long_all
    colnames(long_res) = sub("\\.x", "", colnames(long_res))
    if (verbose) var <- paste0(var, "_verbose")
    long_res[ , c("dsstox_substance_id", "chnm", "aenm", var)] %>% 
      pivot_wider(names_from = aenm, values_from = var) %>% as.data.table()
  }
  
  mat_list <- lapply(vars, build_matrix)

  names(mat_list) = c("ac50", "ac50_verbose", "acc", "acc_verbose", "mc_hitc", "sc_hitc", add.vars)
  
  mat_list
  
}

#-------------------------------------------------------------------------------
