## roxygen documentaiton for okmesonet package and subsequent datasets

#' Access available Oklahoma Mesonet climate data
#'
#' The \href{http://www.mesonet.org/}{Oklahoma Mesonet} is a network of 
#' automated climate monitoring stations throughout the state of Oklahoma, USA; 
#' data collection began on Jan 01, 1994.
#' As of February 2012, there are 120 active stations, with an additional 15
#' stations that have been decommissioned (but with available data). 
#' Measurements are recorded 
#' every five minutes and sent to a central facility for verification and 
#' quality control by the Oklahoma Climatological Survey.
#'
#' @name okmesonet-package
#' @aliases okmesonet
#' @docType package
NULL
#' Bison GPS coordinates
#'
#' GPS location information for one American bison (\emph{Bison bison}) at The
#' Nature Conservancy Tallgrass Prairie Preserve, Jan 31 - Feb 5, 2011.
#' Information includes timestamp, latitude, and longitude.
#' @docType data
#' @name bison
#' @format Data frame
NULL
#' List and attributes of Oklahoma Mesonet stations
#'
#' List of \href{http://www.mesonet.org/}{Oklahoma Mesonet} stations. The
#' Oklahoma Mesonet is a network of 
#' automated climate monitoring stations throughout the state of Oklahoma, USA; 
#' data collection began on Jan 01, 1994.
#' As of February 2012, there are 120 active stations, with an additional 15
#' stations that have been decommissioned (but with available data). 
#' Measurements are recorded 
#' every five minutes and sent to a central facility for verification and 
#' quality control by the Oklahoma Climatological Survey.
#'
#' Station list is updated each time \code{okmesonet} is loaded. Variables 
#' associated with each station include:
#' \itemize{
#'  \item Identifier: four letter station identifier
#'  \item Number: station identification number
#'  \item Name: station name
#'  \item Town: closest town
#'  \item County: Oklahoma county
#'  \item Latitude: station latitude, decimal degrees
#'  \item Longitude: station longitude, decimal degrees
#'  \item Elevation: station elevation, meters
#'  \item Commissioned: date station was commissioned
#'  \item Decommissioned: date station was decommissioned. "2099-12-31" 
#'    represents currently active
#' }
#'
#' @docType data
#' @name okstations
#' @format Data frame
NULL