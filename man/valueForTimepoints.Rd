% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/timepoints.R
\name{valueForTimepoints}
\alias{valueForTimepoints}
\title{Get value for specific timepoint}
\usage{
valueForTimepoints(icm, tp, type, lag = 0L, prelag = 0L,
  fun = .maxNa, verbose = interactive())
}
\arguments{
\item{icm}{\code{data.frame}, ICM data}

\item{tp}{\code{data.frme}, timepoint data, first column Id, other columns
timepoints}

\item{type}{\code{character}, type of value}

\item{lag}{\code{numeric}, lag seconds added to reference date and extend the
range to 24 h + lag seconds (e.g. laboratory values take some time)}

\item{prelag}{\code{numeric}, lag seconds added to reference date-24h and extend the
range to -24 h + lag seconds}

\item{fun}{\code{function}, to apply over the values}

\item{verbose}{\code{logical}, verbose output?}
}
\value{
\code{data.frame}
}
\description{
Get value for specific timepoint
}
