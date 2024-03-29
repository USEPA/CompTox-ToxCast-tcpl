% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sc1.R
\name{sc1}
\alias{sc1}
\title{Perform level 1  
single-concentration processing}
\usage{
sc1(ac, wr = FALSE)
}
\arguments{
\item{ac}{Integer of length 1, assay component id (acid) for processing.}

\item{wr}{Logical, whether the processed data should be written to the tcpl
database}
}
\value{
A boolean of length 1, indicating the success of the processing, or 
when 'wr' is FALSE, a list where the first element is a boolean indicating 
the success of processing and the second element is a data.table containing 
the processed data
}
\description{
\code{sc1} loads level 0 data from the tcpl 
database for the given id and performs level 1 
single-concentration processing. 
The processed data is then loaded into the sc1 table and 
all subsequent data is deleted with \code{\link{tcplCascade}}. See details 
for more information.

The individual processing functions are no longer exported, as it is 
typically more convenient and suggested to use the \code{\link{tcplRun}} 
wrapper function.
}
\details{
Level 1 single-concentration processing includes mapping assay component 
to assay endpoint, duplicating the data when the assay component has 
multiple assay endpoints, and any normalization of the data. Data 
normalization based on methods listed in sc1_aeid and sc1_methods tables.
}
\seealso{
\code{\link{Method functions}}, \code{\link{SC1_Methods}}

Other single-concentration: 
\code{\link{sc2}()}
}
\concept{single-concentration}
