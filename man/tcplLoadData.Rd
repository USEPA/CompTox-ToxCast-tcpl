% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tcplLoadData.R
\name{tcplLoadData}
\alias{tcplLoadData}
\title{Load tcpl data}
\usage{
tcplLoadData(lvl, fld = NULL, val = NULL, type = "mc", add.fld = TRUE)
}
\arguments{
\item{lvl}{Integer of length 1, the level of data to load}

\item{fld}{Character, the field(s) to query on}

\item{val}{List, vectors of values for each field to query on. Must be in
the same order as 'fld'.}

\item{type}{Character of length 1, the data type, "sc" or "mc"}

\item{add.fld}{Boolean if true we want to return 
the additional parameters fit with tcplfit2}
}
\value{
A data.table containing data for the given fields.
}
\description{
\code{tcplLoadData} queries the tcpl databases and returns a data.table with
data for the given level and data type.
}
\details{
The data type can be either 'mc' for mutliple concentration data, or 'sc'
for single concentration data. Multiple concentration data will be loaded
into the 'mc' tables, whereas the single concentration will be loaded into
the 'sc' tables.

Setting 'lvl' to "agg" will return an aggregate table containing the m4id
with the concentration-response data and m3id to map back to well-level
information.

Leaving \code{fld} NULL will return all data.

Valid \code{fld} inputs are based on the data level and type:
\tabular{ccl}{
type \tab lvl \tab  Queried tables \cr
sc \tab 0 \tab sc0 \cr
sc \tab 1 \tab sc0, sc1 \cr
sc \tab agg \tab sc1, sc2_agg \cr
sc \tab 2 \tab sc2 \cr
mc \tab 0 \tab mc0 \cr
mc \tab 1 \tab mc0, mc1 \cr
mc \tab 2 \tab mc0, mc1, mc2 \cr
mc \tab 3 \tab mc0, mc1, mc3 \cr
mc \tab agg \tab mc3, mc4_agg \cr
mc \tab 4 \tab mc4 \cr
mc \tab 5 \tab mc4, mc5 \cr
mc \tab 6 \tab mc4, mc6 \cr
mc \tab 7 \tab mc4, mc7
}
}
\examples{
## Store the current config settings, so they can be reloaded at the end
## of the examples
conf_store <- tcplConfList()
tcplConfExample()

## Load all of level 0 for multiple-concentration data, note 'mc' is the
## default value for type
tcplLoadData(lvl = 0)

## Load all of level 1 for single-concentration
tcplLoadData(lvl = 1, type = "sc")

## List the fields available for level 1, coming from tables mc0 and mc1
tcplListFlds(tbl = "mc0")
tcplListFlds(tbl = "mc1")

## Load level 0 data where the well type is "t" and the concentration
## index is 3 or 4
tcplLoadData(lvl = 1, fld = c("wllt", "cndx"), val = list("t", c(3:4)))

## Reset configuration
options(conf_store)
}
\seealso{
\code{\link{tcplQuery}}, \code{\link{data.table}}
}
