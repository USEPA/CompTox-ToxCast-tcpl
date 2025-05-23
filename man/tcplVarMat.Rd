% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tcplVarMat.R
\name{tcplVarMat}
\alias{tcplVarMat}
\title{Create chemical by assay matrices}
\usage{
tcplVarMat(
  dsstox_substance_id = NULL,
  aeid = NULL,
  std.vars = c("ac50", "ac50_verbose", "acc", "acc_verbose", "hitc_mc", "hitc_sc",
    "zscore"),
  add.vars = NULL,
  flag = TRUE,
  cyto.pars = list()
)
}
\arguments{
\item{dsstox_substance_id}{Integer, chemical ID values to subset on}

\item{aeid}{Integer, assay endpoint ID values to subset on}

\item{std.vars}{Character, standard set of matrices; use this parameter to 
subset this list}

\item{add.vars}{Character, mc4 or mc5 field(s) not included in the standard
list to add additional matrices}

\item{flag}{Integer or Logical of length 1, passed to 
\code{\link{tcplSubsetChid}}}

\item{cyto.pars}{List, named list of arguments passed to 
\code{\link{tcplCytoPt}} for z-score matrix}
}
\value{
A list of chemical by assay matrices (data.tables) where the 
rows are given by the dsstox_substance_id and corresponding chnm (chemical
name) columns and the colnames are given by assay endpoint name (aenm).
}
\description{
\code{tcplVarMat} creates chemical by assay matrices.
}
\details{
The \code{tcplVarMat} function is used to create chemical by assay matrices
for different parameters. The standard list of matrices returned includes:

\enumerate{
 \item "ac50" -- The active concentration at 50% maximal response (ac50) for 
 the winning model.
 \item "ac50_verbose" -- The AC50 for the winning model, with text describing
 some situations.
 \item "acc" -- The active concentration at user-defined cutoff for the 
 winning model.
 \item "acc_verbose" -- The ACC for the winning model, with text describing
 some situations.
 \item "hitc_mc" -- The hit-call for the winning model in 
 multiple-concentration (mc) screening.
 \item "hitc_sc" -- The hit-call in single concentration (sc) screening.
 \item "zscore" -- The z-score based on the output from \code{tcplCytoPt}. 
 The formula used for calculating the z-score is 
 \eqn{-(\mathit{ac50} - \mathit{cyto\_pt})/\mathit{global\_mad}}
}

\code{tcplVarMat} produces matrices of combined sc-mc output. For the ac50
and acc matrices specifically, values are inserted in place to show complete
views of what was tested and what the results were. ac50 and acc values are:
\itemize{
 \item set to 1e6 when the chemical is tested but negative in mc. In _verbose 
 matrices, these are indicated as "MC neg".
 \item set to 1e7 when the chemical is not tested in mc but was screened in 
 sc with a positive hitcall for the same aeid. In _verbose matrices, these 
 are indicated as "SC pos, No MC".
 \item set to 1e8 when the chemical is not tested in mc but was screened in 
 sc with a negative hitcall for the same aeid. In _verbose matrices, these 
 are indicated as "SC neg, No MC"
 \item not changed when chemical is tested in mc and positive, or not tested in
either mc or sc
} 

sc and mc data both are currently required to be included for these 
calculations. As a result, the "API" driver is not currently supported since
it does not return sc data.

To add additional matrices, the 'add.vars' parameter can be used to specify
the fields from the mc4 or mc5 tables to create matrices for.

When more than one sample is included for a chemical/assay pair, 
\code{tcplVarMat} aggregates multiple samples to a chemical level call 
utilizing \code{\link{tcplSubsetChid}}. The tcplVarMat function calls both 
\code{tcplSubsetChid} and \code{tcplCytoPt} (which separately calls 
\code{tcplSubsetChid}). The input for the \code{tcplVarMat} 'flag' parameter
is passed to the \code{tcplSubsetChid} call and used to parse down the data 
to create the matrices. The \code{tcplSubsetChid} called within \code{tcplCytoPt}
(to parse down the cytotoxicity data used to define the "zscore" matrix) can 
be modified by passing a separate 'flag' element in the list defined by the 
'cyto.pars' parameter.
}
\examples{
\dontrun{
## Demonstrate the returned values.
varmat <- tcplVarMat()

## Other changes can be made
aeids <- c(80)
dtxsid <- c("DTXSID4034653", "DTXSID2032683", "DTXSID6032358", 
"DTXSID0032651", "DTXSID8034401")
varmat <- tcplVarMat(aeid = aeids, dsstox_substance_id = dtxsid)
varmat <- tcplVarMat(aeid = aeids, std.vars = c("ac50", "zscore"))
varmat <- tcplVarMat(aeid = aeids, add.vars = c("m4id", "resp_max", "max_med"))

## To save output to file
library(writexl)
write_xlsx(varmat, path = "varmat_output.xlsx")
}

}
\seealso{
\code{\link{tcplSubsetChid}}
}
