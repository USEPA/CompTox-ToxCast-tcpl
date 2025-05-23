% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tcplPlotLoadData.R
\name{tcplPlotLoadData}
\alias{tcplPlotLoadData}
\title{Utility function to load data for tcplPlot}
\usage{
tcplPlotLoadData(type = "mc", fld = "m4id", val, flags = FALSE)
}
\arguments{
\item{type}{Character of length 1, the data type, "sc" or "mc"}

\item{fld}{Character, the field(s) to query on.}

\item{val}{List, vectors of values for each field to query on. Must be in
the same order as 'fld'.}

\item{flags}{Boolean, by default FALSE. If TRUE, level 6 flags are loaded
for use in tcplPlot. Must be set to TRUE if tcplPlot 'flags' also is/will be
set to TRUE}
}
\value{
A data.table containing plot-ready data for the given fields.
}
\description{
\code{tcplPlotLoadData} queries the tcpl databases and returns a data.table
with data for the given field, value, level, and data type prepared in a
format tcplPlot can use to generate plots.
}
\details{
This utility function is used by \code{tcplPlot} to load and prepare data from 
\code{tcplLoadData} for use in generating plots. It is exported for use in 
advanced comparison plots where users create plots using multiple data sources. 
After saving the response from \code{tcplPlotLoadData}, switch data source
config and pass the data to \code{tcplPlot} \code{dat} parameter.

The data \code{type} can be either 'mc' for multiple concentration data, or
'sc' for single concentration data.
}
\examples{
\dontrun{
## load mc plot data for an entire endpoint
dat <- tcplPlotLoadData(fld = "aeid", val = 703)

## load sc plot data for an entire endpoint
dat <- tcplPlotLoadData(type = "sc", fld = "aeid", val = 703)

## load plot data for two endpoint-samples and include loading of flags
## flags must equal TRUE if tcplPlot will/does
dat <- tcplPlotLoadData(fld = c("spid", "aeid"), 
                        val = list(c("TP0000269F11", "TP0000395A09"),703),
                        flags = TRUE)
                        
## if desired, switch connections
tcplConf()

## use dat in tcplPlot
tcplPlot(dat = dat, 
         fld = c("spid", "aeid"), 
         val = list(c("TP0000269F11", "TP0000395A09"),703), 
         compare.val = list(c("LEGTV002B01", "LEGTV003A06"),703),
         output = "pdf", 
         flags = TRUE,
         fileprefix="example")
}

}
\seealso{
\code{\link{tcplPlot}}
}
