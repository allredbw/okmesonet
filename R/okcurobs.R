okcurobs <- function() {
  ## set path and read current observation file
  path <- paste("http://www.mesonet.org/data/public/mesonet/current/",
                "current.csv.txt", sep="")
  curobs <- read.csv(file=path, as.is=T)
  
  ## stitch timestamp together
  time.stitched <- paste(curobs$YR, sprintf("%02d", curobs$MO),
                         sprintf("%02d", curobs$DA),
                         sprintf("%02d", curobs$HR),
                         sprintf("%02d", curobs$MI), "-0600", sep="")
  
  curobs$TIME <- as.POSIXct(time.stitched, format="%Y%m%d%H%M%z")
}