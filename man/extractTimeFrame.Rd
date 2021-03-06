% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/timepoints.R
\name{extractTimeFrame}
\alias{extractTimeFrame}
\title{Extract specific time range/frame.}
\usage{
extractTimeFrame(icm, tp, lag = 0L)
}
\arguments{
\item{icm}{\code{data.frame}, ICM data}

\item{tp}{\code{data.frme}, timepoint data, first column Id, second column
timepoints}

\item{lag}{\code{numeric}, lag seconds added to reference date and extend the
range to 24 h + lag seconds (e.g. laboratory values take some time)}
}
\value{
\code{data.frame}
}
\description{
Extract specific time range/frame.
}
