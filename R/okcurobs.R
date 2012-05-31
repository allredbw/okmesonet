okcurobs <- function(SI=TRUE) {
  ## Retreive current observations for all stations from Mesonet website
  ## Arguments:
  ##  SI: logical to indicate conversion to SI units
  ## Returns: dataframe of current observations, TIME variable added as POSIXct
  ##  class
  
  ## set path and read current observation file
  path <- paste("http://www.mesonet.org/data/public/mesonet/current/",
                "current.csv.txt", sep="")
  curobs <- read.csv(file=path,
                     colClasses=c("character", "character", "character",
                                  "numeric", "numeric", "integer", "integer",
                                  "integer", "integer", "integer",
                                  "integer", "integer", "integer", "integer",
                                  "integer", "character", "integer", "integer",
                                  "numeric", "integer", "integer", "numeric"))
  
  ## stitch timestamp together
  time.stitched <- paste(curobs$YR, sprintf("%02d", curobs$MO),
                         sprintf("%02d", curobs$DA),
                         sprintf("%02d", curobs$HR),
                         sprintf("%02d", curobs$MI), "-0600", sep="")
  ## add TIME variable
  curobs$TIME <- as.POSIXct(time.stitched, format="%Y%m%d%H%M%z")
  
  ## if SI=TRUE, convert to SI units
  if(SI==T) {
    ## temperatures
    curobs$TAIR <- round((5/9) * (curobs$TAIR - 32), 1)
    curobs$TDEW <- round((5/9) * (curobs$TDEW - 32), 1)
    curobs$CHIL <- round((5/9) * (curobs$CHIL - 32), 1)
    curobs$HEAT <- round((5/9) * (curobs$HEAT - 32), 1)
    curobs$TMAX <- round((5/9) * (curobs$TMAX - 32), 1)
    curobs$TMIN <- round((5/9) * (curobs$TMIN - 32), 1)
    
    ## wind speeds
    curobs$WSPD <- round(curobs$WSPD * 0.44704, 1)
    curobs$WMAX <- round(curobs$WMAX * 0.44704, 1)
    
    ## rain
    curobs$RAIN <- round(curobs$RAIN * 25.4, 1)
  }
  return(curobs)
}