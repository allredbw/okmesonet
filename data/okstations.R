data.checkgeoinfo <- function() {
  ## Check to verify internet connectivity and access to 
  ## http://www.mesonet.org/index.php/api/siteinfo/from_all_active_with_geo_fields/format/csv/ 
  ## is available.
  ##
  ## Arguments: none
  ## Returns: logical indicating success
  path.geoinfo <- paste("http://www.mesonet.org/index.php/api/siteinfo/",
                        "from_all_active_with_geo_fields/format/csv/", sep ="")
  internetoption <- getOption("internet.info")
  options(show.error.messages=FALSE, internet.info=3)
  checkconn <- try(readLines(path.geoinfo, n=1))
  options(show.error.messages=TRUE, internet.info=internetoption)
  if (inherits(checkconn,"try-error")) return(FALSE) else return(TRUE)
}

data.downloadstn <- function() {
  ## Retrieve Oklahoma Mesonet station information, including:
  ## station identifier, station number, station name, nearest town, latitude,
  ## longitude, elevation, date commissioned, date decommissioned
  ##
  ## Arguments: none
  ## Returns: data frame with above information
  
  ## use .checkgeoinfo() to check connectivity
  path.geoinfo <- paste("http://www.mesonet.org/index.php/api/siteinfo/",
                        "from_all_active_with_geo_fields/format/csv/", sep ="")
  if(data.checkgeoinfo()==FALSE) {
    warn.msg <- paste("Oklahoma Mesonet station list unavailable. Check", 
                      path.geoinfo, "for", 
                      "connectivity and run updatestn() to update station", 
                      "list.")
    warning(warn.msg, call.=F)
  } else {
    geoinfo <- read.csv(path.geoinfo, header=T, as.is=T)
    stationlist <- data.frame(Identifier=geoinfo$stid, Number=geoinfo$stnm,
                              Name=geoinfo$name, Town=geoinfo$city, 
                              County=geoinfo$cnty, Latitude=geoinfo$nlat, 
                              Longitude=geoinfo$elon, Elevation=geoinfo$elev,
                              Commissioned=strptime(geoinfo$datc, 
                                                    format="%Y%m%d",
                                                    tz="America/Chicago"),
                              Decommissioned=strptime(geoinfo$datd, 
                                                      format="%Y%m%d",
                                                      tz="America/Chicago"),
                              stringsAsFactors=F)
    return(stationlist)
  }
}

okstations <- data.downloadstn()