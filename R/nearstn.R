nearstn <- function(pnt.lon, pnt.lat, startdate, enddate) {
  ## Calculate the nearest OK Mesonet station given latitude and longitude
  ## coordinates.
  ## Retrieves station coordinates from stations data frame
  ## 
  ## Arguments:
  ##  pnt.lon: longitude of location, given in decimal degrees
  ##  pnt.lat: latitude of location, given in decimal degrees
  ##  startdate:  start date of desired data
  ##  enddate: end date of desired data
  ## Returns: four letter station identifier as character object
  
  ## load mapproj package
  if(require(mapproj)==F) stop(c("Using lat/long coordinates requires the ",
                                 "'mapproj' package. Please install with ",
                                 "'install.packages()'"))
  
  ## subset active stations for given time frame
  stations.active <- subset(stations, Commissioned<=startdate & 
                            Decommissioned>enddate)
  
  ## calculate number of stations
  stnnumber <- nrow(stations.active)
  
  ## reproject station and point coordinates to cartesian plane
  coord <- mapproject(x=c(stations.active$Longitude, pnt.lon), 
                      y=c(stations.active$Latitude, pnt.lat), 
                      projection="mercator")
  
  ## calculate station distances from point location
  ## stnnumber+1 represents point location
  stndistance <- sqrt((coord$x[stnnumber+1]-coord$x[1:stnnumber])^2
                      +(coord$y[stnnumber+1]-coord$y[1:stnnumber])^2)
  
  ## determine nearest station
  nearstn <- which.min(stndistance)
  
  ## print message displaying station
  cat(paste("Using ",stations.active$Name[nearstn], " (", 
            stations.active$Identifier[nearstn], ") station. Commissioned: ",
            stations.active$Commissioned[nearstn], ". Decommissioned: ",
            stations.active$Decommissioned[nearstn], ".", sep=""))
  
  ## return four letter station identifier as lowercase
  return(tolower(stations.active$Identifier[nearstn]))
}