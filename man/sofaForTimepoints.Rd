% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/timepoints.R
\name{sofaForTimepoints}
\alias{sofaForTimepoints}
\title{Get SOFA for specific timepoint}
\usage{
sofaForTimepoints(icm, tp, lag = 0L, lagOnlyLaboratory = TRUE,
  verbose = interactive())
}
\arguments{
\item{icm}{\code{data.frame}, ICM data}

\item{tp}{\code{data.frme}, timepoint data, first column Id, other columns
timepoints}

\item{lag}{\code{numeric}, lag seconds added to reference date and extend the
range to 24 h + lag seconds (e.g. laboratory values take some time)}

\item{lagOnlyLaboratory}{\code{logical} add lag seconds only to the laboratory
values?}

\item{verbose}{\code{logical}, verbose output?}
}
\value{
\code{data.frame}
}
\description{
Get SOFA for specific timepoint
}
