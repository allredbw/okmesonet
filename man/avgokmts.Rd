\name{avgokmts}
\alias{avgokmts}
\title{Average an Oklahoma Mesonet time series data frame}
\usage{
  avgokmts(mts, by, metric = "mean")
}
\arguments{
  \item{mts}{data frame returned by \code{okmts}.}

  \item{by}{character string indicating time period to
  average over. May include "hour", "day", "month", or
  "year".}

  \item{metric}{function to summarize with. Default is
  "mean" (average), but may also include "min" and "max"
  for minimum and maximum, respectively.}
}
\value{
  A data frame summarizing Mesonet measurements by station
  and given time period.
}
\description{
  Summarize an Oklahoma Mesonet time series (MTS) data
  frame returned by \code{\link{okmts}}. Summary can be by
  hour, day, month, or year. Precipitation (variable RAIN)
  is returned as totals within the given time period.
}
\examples{
\dontrun{
## Retrieve Bessie station MTS files for 00:00 Jun 01, 1997
## through 23:55 Oct 31, 1997
bess.mts <- okmts(begintime="1997-06-01 00:00:00",
 endtime="1997-10-31 23:55", station="bess")

## Average MTS data by day.
bess.mts.avg  <- avgokmts(bess.mts, by="day")
}
}

