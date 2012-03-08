checkgeomeso <- function() {
  ## Check to verify internet connectivity and access to 
  ## http://www.mesonet.org/sites/geomeso.csv is available.
  ##
  ## Arguments: none
  ## Returns: logical indicating success
  internetoption <- getOption("internet.info")
  options(show.error.messages=FALSE, internet.info=3)
  checkconn <- try(readLines("http://www.mesonet.org/sites/geomeso.csv")[1])
  options(show.error.messages=TRUE, internet.info=internetoption)
  if (inherits(checkconn,"try-error")) return(FALSE)
  else return(TRUE)
}

updatestn <- function() {
  ## Retrieve Oklahoma Mesonet station information, including:
  ## station identifier, station number, station name, nearest town, latitude,
  ## longitude, elevation, date commissioned, date decommissioned
  ##
  ## Arguments: none
  ## Returns: data frame with above information
  geomeso <- read.csv("http://www.mesonet.org/sites/geomeso.csv", 
                      skip=122, header=F, as.is=T)
  stationlist <- data.frame(Identifier=geomeso$V2, Number=geomeso$V1,
                            Name=geomeso$V3, Town=geomeso$V4, County=geomeso$V7,
                            Latitude=geomeso$V8, Longitude=geomeso$V9,
                            Elevation=geomeso$V10,
                            Commissioned=strptime(geomeso$V55, format="%Y%m%d",
                                                  tz="America/Chicago"),
                            Decommissioned=strptime(geomeso$V56, format="%Y%m%d",
                                                    tz="America/Chicago"),
                            stringsAsFactors=F)
  return(stationlist)
}

if(checkgeomeso()==TRUE) {
  okstations <- updatestn()
} else {
  cat(paste("Check http://www.mesonet.org/sites/geomeso.csv for connectivity\n",
            "and run data(okstations, package=", dQuote("okmesonet"),")", sep=""))
}