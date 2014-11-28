\docType{data}
\name{okstations}
\alias{okstations}
\title{List of Oklahoma Mesonet stations}
\format{Data frame}
\description{
  List of \href{http://www.mesonet.org/}{Oklahoma Mesonet}
  stations. The Oklahoma Mesonet is a network of automated
  climate monitoring stations throughout Oklahoma, USA.
  Data collection began January 01, 1994; as of November
  2014, there are 120 active stations. Measurements are
  recorded every five minutes and sent to a central
  facility for verification and quality control by the
  Oklahoma Climatological Survey.
}
\details{
  Station list was updated Nov 28, 2014. See \code{\link{updatestn}} to update 
  station list.

  Variables associated with each station include: \itemize{
  \item Identifier: four letter station identifier \item
  Number: station identification number \item Name: station
  name \item Town: closest town \item County: Oklahoma
  county \item Latitude: station latitude, decimal degrees
  \item Longitude: station longitude, decimal degrees \item
  Elevation: station elevation, meters \item Commissioned:
  date station was commissioned \item Decommissioned: date
  station was decommissioned. "2099-12-31" represents
  currently active }
}
\seealso{
  \code{\link{updatestn}} to manually update station list.
}
\keyword{datasets}

