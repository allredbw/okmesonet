\name{updatestn}
\alias{updatestn}
\title{Update list of Oklahoma Mesonet stations}
\usage{
  updatestn()
}
\description{
  The list of Oklahoma Mesonet stations is updated each
  time \code{okmesonet} is loaded. If this fails (e.g.,
  internet connectivity is temporarily down), use
  \code{updatestn} to manually update the list.
}
\examples{
\dontrun{
## Update Oklahoma Mesonet station list
okstations <- updatestn()
}
}
\seealso{
  \code{\link{okstations}}
}

