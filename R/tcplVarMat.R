#-------------------------------------------------------------------------------
# tcplVarMat: Create chemical by assay matrices
#-------------------------------------------------------------------------------

#' @title Create chemical by assay matrices
#' 
#' @description
#' \code{tcplVarMat} creates chemical by assay matrices.
#' 
#' @param chid Integer, chemical ID values to subset on
#' @param aeid Integer, assay endpoint ID values to subset on
#' @param add.vars Character, mc4 or mc5 field(s) not included in the standard
#' list to add additional matrices 
#' @param row.id Character, the chemical identifier to use in the output
#' @param flag Integer or Logical of length 1, passed to 
#' \code{\link{tcplSubsetChid}}
#' @param cyto.pars List, named list of arguments passed to 
#' \code{\link{tcplCytoPt}}
#' @param include.na.chid Logical of length 1, whether to include the chemicals
#' not listed in the tcpl databases (ie. controls)
#' @param odir Directory to write comma separated file(s)
#' @param file.prefix Character of length 1, prefix to the file name when odir
#' is not NULL
#' 
#' 
#' @details
#' The \code{tcplVarMat} function is used to create chemical by assay matrices
#' for different paramters. The standard list of matrices returned includes:
#' 
#' \enumerate{
#'  \item "modl_ga" -- The logAC50 (in the gain direction) for the winning 
#'  model. 
#'  \item "hitc" -- The hit-call for the winning model.
#'  \item "m4id" -- The m4id, listing the concentration series selected by 
#'  \code{tcplSubsetChid}.
#'  \item "zscore" -- The z-score based on the output from \code{tcplCytoPt}. 
#'  The formula used for calculating the z-score is 
#'  \eqn{-(\mathit{modl\_ga} - \mathit{cyto\_pt})/\mathit{global\_mad}}
#'  \item "tested" -- 1 or 0, 1 indicating the chemical/assay pair
#'  was tested in either the single- or multiple-concentration format
#'  \item "tested_sc" -- 1 or 0, 1 indicating the chemical/assay pair
#'  was tested in the single-concentration format
#'  \item "tested_mc" -- 1 or 0, 1 indicating the chemical/assay pair
#'  was tested in the multiple-concentration format
#'  \item "ac50" -- a modified AC50 table (in non-log units) where 
#'  assay/chemical pairs that were not tested, or tested and had a hitcall of 0
#'  or -1 have the value 1e6. 
#'  \item "neglogac50" -- -log(AC50/1e6) where assay/chemical pairs that were 
#'  not tested, or tested and had a hitcall of 0 or -1 have the value 0. 
#' }
#' 
#' To add addtitional matrices, the 'add.vars' parameter can be used to specify
#' the fields from the mc4 or mc5 tables to create matrices for.
#' 
#' When more than one sample is included for a chemical/assay pair, 
#' \code{tcplVarMat} aggregates multiple samples to a chemical level call 
#' utilizing \code{\link{tcplSubsetChid}}. 
#' 
#' By setting \code{odir} the function will write out a csv with, naming the 
#' file with the convention: "var_Matrix_date.csv" where 'var' is the name 
#' of the matrix. A prefix can be added to the output files using the 
#' 'file.prefix' paramter. 
#' 
#' When a concentration series has a sample id not listed in the \code{tcpl} 
#' database, and 'include.na.chid' is TRUE, the rowname for that series will 
#' be the concatenation of "SPID_" and the spid. Note, if the user gives a 
#' subset of chid values to the 'chid' parameter, 'include.na.chid' will be 
#' set to FALSE with a warning.
#' 
#' The tcplVarMat function calls both \code{tcplSubsetChid} and 
#' \code{tcplCytoPt} (which separately calls \code{tcplSubsetChid}). The input
#' for the \code{tcplVarMat} 'flag' parameter is passed to the 
#' \code{tcplSubsetChid} call used to parse down the data to create the 
#' matrices. The \code{tcplSubsetChid} called within \code{tcplCytoPt} (to 
#' parse down the cytotoxicity data used to define the "zscore" matrix) can 
#' be modified by passing a separate 'flag' element in the list defined by the 
#' 'cyto.pars' parameter.
#' 
#' @return A list of chemical by assay matrices where the rownames are given by
#' the 'row.id' paramter, and the colnames are given by assay endpoint name 
#' (aenm).
#' 
#' @examples 
#' ## Store the current config settings, so they can be reloaded at the end 
#' ## of the examples
#' conf_store <- tcplConfList()
#' tcplConfDefault()
#' 
#' ## Demonstrate the returned values. Note with no "burst" assays defined in
#' ## the example database, the user must provide which aeid values to use 
#' ## in calculating the cytotoxicity distributions for the 'zscore' matrix.
#' tcplVarMat(chid = 1:5, cyto.pars = list(aeid = 1:2))
#' 
#' ## Other changes can be made
#' tcplVarMat(chid = 1:5, row.id = "chnm", cyto.pars = list(aeid = 1:2))
#' tcplVarMat(chid = 1:5, add.vars = "max_med", cyto.pars = list(aeid = 1:2))
#' 
#' ## Reset configuration
#' options(conf_store)
#' 
#' @seealso \code{\link{tcplSubsetChid}}
#' 
#' @import data.table
#' @importFrom stats reformulate
#' @importFrom utils write.csv
#' @export

tcplVarMat <- function(chid = NULL,
                       aeid = NULL,
                       add.vars = NULL,
                       row.id = "code",
                       flag = TRUE,
                       cyto.pars = list(),
                       include.na.chid = FALSE,
                       odir = NULL,
                       file.prefix = NULL) {
  
  ## Variable-binding to pass R CMD Check
  sc_tst <- spid <- mc_tst <- acid <- cyto_pt <- global_mad <- zscore <- NULL
  modl_ga <- NULL
  
  if (length(file.prefix) > 1) {
    file.prefix <- file.prefix[1]
    warning("Length of file.prefix greater than 1, only first element used.")
  }
  
  if (!is.null(aeid) & !is.vector(aeid)) stop("'aeid' must be a vector.")
  if (!is.null(chid) & !is.vector(chid)) stop("'chid' must be a vector.")
  
  row.id <- row.id[1]
  if (!row.id %in% c("code", "casn", "chid", "chnm")) row.id <- "code"
  
  valid_var <- c(tcplListFlds("mc4"), tcplListFlds("mc5"))
  if (!all(add.vars %in% valid_var)) stop("Invald add.vars value(s).")
  
  std.vars <- c("modl_ga", "hitc", "m4id", "zscore")
  vars <- c(std.vars, add.vars)
  
  cform <- reformulate(termlabels = "aenm", response = row.id)
  
  ## Load all possibilities to create matrix dimensions
  sc <- tcplQuery("SELECT DISTINCT acid, spid FROM sc0;")
  mc <- tcplQuery("SELECT DISTINCT acid, spid FROM mc0;")
  
  tst <- rbindlist(list(sc, mc))
  tst <- unique(tst)
  tst[ , sc_tst := spid %in% sc$spid]
  tst[ , mc_tst := spid %in% mc$spid]
  rm(sc, mc)
  
  ## Expand acid to aeid
  aeid_info <- tcplLoadAeid("acid", tst[ , unique(acid)])
  setkey(aeid_info, acid)
  setkey(tst, acid)
  tst <- aeid_info[ , list(acid, aeid)][tst, allow.cartesian = TRUE]
  
  ## Subset by aeid
  if (is.null(aeid)) {
    ae <- unique(tst$aeid)
  } else {
    ae <- aeid 
    tst <- tst[aeid %in% ae]
  }
  
  ## Load level 5 data
  dat <- tcplLoadData(lvl = 5, fld = "aeid", val = ae, type = "mc")
  
  setkeyv(dat, c("aeid", "spid"))
  setkeyv(tst, c("aeid", "spid"))
  dat <- merge(dat, tst, all = TRUE)
  
  dat <- tcplPrepOtpt(dat)
  
  if (!is.null(chid)) {
    if (include.na.chid) {
      warning("'include.na.chid' cannot be TRUE when 'chid' is not NULL.")
      include.na.chid <- FALSE
    }
    ch <- chid 
    dat <- dat[chid %in% ch]
  }
  
  if(include.na.chid) {
    dat[ , chid := as.character(chid)]
    dat[is.na(chid), 
        c("casn", "chid", "code", "chnm") := paste0("SPID_", spid)]
  } else {
    dat <- dat[!is.na(chid)]
  }
  
  dat <- tcplSubsetChid(dat = dat, flag = flag)    
  
  if (is.null(cyto.pars)) cyto.pars <- list()
  zdst <- do.call(what = tcplCytoPt, args = cyto.pars)
  
  if(include.na.chid) zdst[ , chid := as.character(chid)]
  
  setkey(zdst, chid)
  setkey(dat, chid)
  
  dat <- zdst[ , list(chid, cyto_pt, global_mad)][dat]
  dat[ , zscore := -(modl_ga - cyto_pt)/global_mad]
  
  mat.tested <- dcast(dat, 
                      formula = cform, 
                      fun.aggregate = lu,
                      value.var = "chid")
  
  mat.sc_tst <- dcast(dat, 
                      formula = cform, 
                      fun.aggregate = any,
                      value.var = "sc_tst")
  
  mat.mc_tst <- dcast(dat, 
                      formula = cform, 
                      fun.aggregate = any,
                      value.var = "mc_tst")
  
  rnames <- mat.tested[ , get(row.id)]
  e1 <- bquote(.(row.id) := NULL)
  
  mat.tested[ , eval(e1)]
  mat.tested <- as.matrix(mat.tested)
  row.names(mat.tested) <- rnames
  
  mat.sc_tst[ , eval(e1)]
  mat.sc_tst <- as.matrix(mat.sc_tst)
  row.names(mat.sc_tst) <- rnames
  
  mat.mc_tst[ , eval(e1)]
  mat.mc_tst <- as.matrix(mat.mc_tst)
  row.names(mat.mc_tst) <- rnames
  
  ddt <- function(x) {
    mat <- dcast(data = dat, formula = cform, value.var = x)
    mat[ , eval(e1)]
    mat <- as.matrix(mat)
    row.names(mat) <- rnames
    mat
  }

  mat.list <- lapply(vars, ddt)
  names(mat.list) <- vars
  
  mat.list[["tested"]] <- mat.tested
  mat.list[["tested_mc"]] <- mat.mc_tst
  mat.list[["tested_sc"]] <- mat.sc_tst

  mat_ac <- 10^mat.list[["modl_ga"]]
  mat_ac[mat.list[["hitc"]] != 1] <- 1e6
  mat_ac[is.na(mat.list[["modl_ga"]]) & mat.list[["tested"]] == 1] <- 1e6
  mat_lac <- -log10(mat_ac/1e6)
  
  mat.list[["ac50"]] <- mat_ac
  mat.list[["neglogac50"]] <- mat_lac
  
  if (!is.null(odir)) {
    
    fdate <- format(Sys.Date(), "%y%m%d.csv")
    fname <- paste(names(mat.list), "Matrix", fdate, sep = "_")
    if (!is.null(file.prefix)) fname <- paste(file.prefix, fname, sep = "_")
    for(i in 1:length(mat.list)) {
      write.csv(mat.list[[i]], file.path(odir, fname[i]), row.names = TRUE)
    }
  }
  
  mat.list
  
}

#-------------------------------------------------------------------------------
