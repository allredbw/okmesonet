\name{okcurobs}
\alias{okcurobs}
\title{Retrieve Oklahoma Mesonet current observations}
\usage{
  okcurobs(SI = TRUE, localtime = TRUE)
}
\arguments{
  \item{SI}{logical; if \code{TRUE}, convert observations
  to SI units. See 'Details'.}

  \item{localtime}{logical; if \code{TRUE} output time is
  local to Oklahoma.  If \code{FALSE}, output time is
  Coordinated Universal Time (UTC).}
}
\value{
  A data frame with current observations.
}
\description{
  Retrieve current observations from all
  \href{http://www.mesonet.org/}{Oklahoma Mesonet}
  stations.
}
\details{
  See
  \href{http://www.mesonet.org/index.php/site/about/current_observations_csv}{Current
  Observations} for variable and unit description. Current
  observations are stored in 'American' units.
  \code{SI=TRUE} converts to the 'International System of
  Units' (though Celsius is used instead of Kelvin for
  temperature).

  A TIME variable of class POSIXct is added.
  \code{localtime=TRUE} returns TIME as local Oklahoma
  time; \code{localtime=FALSE} returns TIME as UTC.
}
\examples{
\dontrun{
## Retrieve current observations.
curobs <- okcurobs()
}
}
\seealso{
  \code{\link{okmts}}
}

