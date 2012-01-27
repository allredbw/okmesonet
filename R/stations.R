stations <- function() {
  ## Retrieve and display Oklahoma Mesonet station information, including:
  ## station identifier, station number, station name, nearest town, latitude,
  ## longitude, elevation, date commissioned, date decommissioned
  ##
  ## Arguments: none
  ## Returns: data frame with above information
  
  ## check to see if file has been downloaded by previous function execution
  ## if so, read file
  if(length(Sys.glob(file.path(tempdir(),"stationfile*")))==1) {
    geomeso <- read.csv(Sys.glob(file.path(tempdir(), "stationfile*")), 
                        skip=122, header=F)
  } else {
    ## if file has not been downloaded, download and read file
    download.file(url="http://www.mesonet.org/sites/geomeso.csv", quiet=T,
                  destfile=tempfile(pattern="stationfile", tmpdir=tempdir()))
    geomeso <- read.csv(Sys.glob(file.path(tempdir(), "stationfile*")), 
                        skip=122, header=F)
  }
  stationlist <- data.frame(Identifier=geomeso$V2, Number=geomeso$V1,
                            Name=geomeso$V3, Town=geomeso$V4, County=geomeso$V7,
                            Latitude=geomeso$V8, Longitude=geomeso$V9,
                            Elevation=geomeso$V10,
                            Commissioned=strptime(geomeso$V55, format="%Y%m%d",
                                                  tz="America/Chicago"),
                            Decommisioned=strptime(ifelse(geomeso$V56<20500101,
                                                          geomeso$V56, NA), 
                                                   format="%Y%m%d",
                                                   tz="America/Chicago")
                            )
  return(stationlist)
}