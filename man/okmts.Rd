\name{okmts}
\alias{okmts}
\title{Retrieve an Oklahoma Mesonet time series file}
\usage{
  okmts(begintime, endtime, station = NULL, lat = NULL,
    lon = NULL, getvar = "ALL", localtime = TRUE,
    mcores = FALSE)
}
\arguments{
  \item{begintime}{character string or POSIXct object.
  Start time of time period. Character strings must be
  formated as 'YYYY-MM-DD HH:MM:SS'.}

  \item{endtime}{character string or POSIXct object. End
  time of time period. Character strings must be formated
  as 'YYYY-MM-DD HH:MM:SS'.}

  \item{station}{character string. Four letter Mesonet
  station identifier. See 'Details'.}

  \item{lat}{numeric: latitude of point of interest in
  decimal degrees.}

  \item{lon}{numeric: longitude of point of interest in
  decimal degrees.}

  \item{getvar}{character string. Mesonet variables to
  retrieve. See 'Details'.}

  \item{localtime}{logical; if \code{TRUE}, input and
  output time is local to Oklahoma. If \code{FALSE}, input
  and output time is Coordinated Universal Time (UTC or
  GMT). See 'Details'.}

  \item{mcores}{logical; if \code{TRUE}, use multiple cores
  for file retrieval.  See 'Details'.}
}
\value{
  A data frame with values from MTS files for the given
  station, time period, and desired variables. Timestamps
  for each measurement are returned as POSIXct class;
  timezone is determined by \code{localtime}.
}
\description{
  Retrieve an \href{http://www.mesonet.org/}{Oklahoma
  Mesonet} time series (MTS) file for a given time period
  and station. Alternatively, if station is omitted and
  latitude and longitude are given, it retrieves the MTS
  for the closest operating station during the given time
  period.
}
\details{
  The Oklahoma Mesonet is a network of automated climate
  monitoring stations throughout the state of Oklahoma,
  USA; data collection began on Jan 01, 1994. As of
  February 2012, there are 120 active stations, with an
  additional 15 stations decommissioned but with available
  data. Measurements are recorded every five minutes and
  sent to a central facility for verification and quality
  control by the Oklahoma Climatological Survey.

  The timestamps used to define the time period for
  \code{okmts} can be either character strings or POSIXct
  objects. Character strings should be in the format
  "\code{2009-09-08 09:05}" or "\code{2005-12-13
  00:00:00}". POSIXct objects need to have a timezone
  specified; \code{okmts} converts timezones appropriately
  to download correct MTS files.

  Four letter Mesonet station identifier can be found in
  \code{\link{okstations}} or on the
  \href{http://www.mesonet.org/}{Oklahoma Mesonet} website.

  Available Mesonet variables and units are described in
  the
  \href{http://www.mesonet.org/index.php/site/about/mdf_mts_files}{MDF/MTS
  Files} page, 'Parameter Description'
  \href{http://www.mesonet.org/files/parameter_description_readme.pdf}{readme}
  file or
  \href{http://www.mesonet.org/wiki/Public:MDF_Format}{MTS
  specification}.

  Time records of Oklahoma MTS files are stored in
  Coordinated Universal Time (UTC or GMT). To easily
  convert to local Oklahoma time, \code{localtime=TRUE}
  indicates that times used to define the time period are
  local Oklahoma time. Timezone conversion is done
  internally, and accounts for Daylight Savings Time (as
  reliably as R can; see \link{timezone}).
  \code{localtime=TRUE} will also direct \code{okmts} to
  output in local Oklahoma time. \code{localtime=FALSE}
  indicates that UTC or GMT is used for both time input and
  output. If time inputs are of POSIXct class,
  \code{localtime} only affects time output.

  The use of multiple cores can decrease retrieval time for
  lengthy time periods. \code{mcores=TRUE} will direct
  \code{okmts} to use the number cores in the current
  machine (determined by
  \code{\link[parallel]{detectCores}}).
}
\examples{
\dontrun{
## Retrieve Bessie station MTS files for 00:00 Jun 01, 1997
## through 23:55 Oct 31, 1997
bess.mts <- okmts(begintime="1997-06-01 00:00:00",
 endtime="1997-10-31 23:55", station="bess")

## Use POSIXct class to retrieve Medicine Park station air
## temperature for 09:30 through 20:30 Aug 12, 2004
## Set times, using 'America/Chicago' for Oklahoma timezone
medi.time <- c(as.POSIXct("2004-08-12 09:30", tz="America/Chicago"),
 as.POSIXct("2004-08-12 20:30", tz="America/Chicago"))
medi.air <- okmts(begintime=medi.time[1], endtime=medi.time[2],
 station="medi", getvar="TAIR")

## Download all data for 2001 for station closest to
## 36.575284 latitude, -99.478455 longitude, using multiple cores
stn.mts <- okmts(begintime="2001-01-01 00:00:00",
 endtime="2001-12-31 23:55:00", lat=36.575284, lon=-99.478455, mcores=T)

## Retrieve Idabel station MTS data for 00:00 through 12:00 UTC (GMT)
## Nov 23, 2003
## Time values are returned in UTC
idab.mts <- okmts(begintime="2003-11-23 00:00:00",
 endtime="2003-11-23 12:00:00", station="idab", localtime=F)

## Combine air temperature with bison movement data.
## Retrieve Foraker station MTS files for 00:00 Jan 31, 2011
## through 15:00 Feb 05, 2011
fora.mts <- okmts(begintime="2011-01-31 00:00:00",
 endtime="2011-02-05 15:00:00", station="fora")
## Round bison timestamp down to five minute mark
bison$newtime <- as.POSIXlt(bison$timestamp)
bison$newtime$sec <- round(bison$newtime$sec, -2)
bison$newtime$min <- as.integer(format(bison$newtime, "\%M")) \%/\% 5 * 5
bison$newtime <- as.POSIXct(bison$newtime)
## Add Foraker station air temperature to bison data
bison$TAIR <- fora.mts$TAIR[match(bison$newtime, fora.mts$TIME)]
}
}
\seealso{
  \code{\link{avgokmts}} to summarize MTS files.
}

