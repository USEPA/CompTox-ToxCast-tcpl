% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tcplRun.R
\name{tcplRun}
\alias{tcplRun}
\title{Perform data processing}
\usage{
tcplRun(
  asid = NULL,
  slvl,
  elvl,
  id = NULL,
  type = "mc",
  mc.cores = NULL,
  outfile = NULL,
  runname = NULL,
  ready_only = FALSE
)
}
\arguments{
\item{asid}{Integer, assay source id}

\item{slvl}{Integer of length 1, the starting level to process}

\item{elvl}{Integer of length 1, the ending level to process}

\item{id}{Integer, rather than assay source id, the specific assay 
component or assay endpoint id(s) (optional)}

\item{type}{Character of length 1, the data type, "sc" or "mc"}

\item{mc.cores}{Integer of length 1, the number of cores to use, set to 1 
when using Windows operating system}

\item{outfile}{Character of length 1, the name of the log file (optional)}

\item{runname}{Character of length 1, the name of the run to be used in the 
outfile (optional)}

\item{ready_only}{Boolean, whether to only process endpoints marked as 
export_ready = 1.}
}
\value{
A list containing the results from each level of processing. Each 
level processed will return a named logical vector, indicating the success 
of the processing for the id.
}
\description{
\code{tcplRun} is the function for performing the data processing, for both
single-concentration and multiple-concentration formats.
}
\details{
The \code{tcplRun} function is the core processing function within the 
package. The function acts as a wrapper for individual processing functions, 
(ie. \code{mc1}, \code{sc1}, etc.) that are not exported. If possible, the
processing is done in parallel by 'id' by utilizing the 
\code{\link{mclapply}} function within the parallel package. 

If slvl is less than 4, 'id' is interpreted as acid and if slvl is 4 or 
greater 'id' is interpreted as aeid. Must give either 'asid' or 'id'. If an 
id fails no results get loaded into the database, and the id does not get 
placed into the cue for subsequent level processing.

The 'type' parameter specifies what type of processing to complete: "mc" for
multiple-concentration processing, and "sc" for single-concentration 
processing.
}
\concept{data processing functions}
