mts <- function(begintime, endtime, station, getvar, localtime=T, mcores=F) {
  ## Gets Mesonet MTS file from Mesonet homepage
  ## Args:
  ##  begintime: beginning date,given as 'YYYY-MM-DD 00:00'
  ##  endtime: end date, given as 'YYYY-MM-DD 00:00'
  ##  station: four letter character ID for Mesonet station, lowercase
  ##  getvar: variables to retrieve
  ##  localtime: logical to indicate the use of Oklahoma local time, else 
  ##    use GMT
  ##  mcores: logical to indicate use of foreach and multiple cores
  ## Returns: dataframe of MTS files
  library(plyr)
  
  ## available variables
  variables <- c("STID", "STNM", "RELH", "TAIR", "WSPD", "WVEC", "WDIR", "WDSD", 
                 "WSSD", "WMAX", "RAIN", "PRES", "SRAD", "TA9M", "WS2M", "TS10", 
                 "TB10", "TS05", "TB05", "TS30", "TR05", "TR25", "TR60", "TR75",
                 "ALL")
  
  ## convert getvar to uppercase
  getvar <- toupper(getvar)
  
  ## check to see if getvar matches available variables
  if(all(getvar %in% variables)==FALSE) {
    stop(c("Desired variables do not match available variables. ",
           "See http://www.mesonet.org/files/parameter_description_readme.pdf ",
         "for list.")) }
  
  ## if getvar contains "ALL", remove anything else
  if(any(getvar=="ALL")==TRUE) getvar <- "ALL"
  
  ##  format start and end dates according to timezone specification
  ##  if local time is desired, format dates to CST/CDT
  ##  dates for CST/CDT will be system dependent, hopefully standard
  if(localtime==T) {
    ##  Classify 'begintime' and 'endtime' as CST/CDT
    begintime.local <- as.POSIXct(begintime, tz="America/Chicago")
    endtime.local  <- as.POSIXct(endtime, tz="America/Chicago")
    begintime.gmt <- as.POSIXct(format(begintime.local, tz="GMT"), tz="GMT")
    endtime.gmt <- as.POSIXct(format(endtime.local, tz="GMT"), tz="GMT")
    ##  sequence days from begin to end
    dates.gmt <- seq.POSIXt(trunc(begintime.gmt, units="days"),
                              trunc(endtime.gmt, units="days"), by="days")
  } else {
    ##  Classify 'begintime' and 'endtime' as GMT
    begintime.gmt <- as.POSIXct(begintime, tz="GMT")
    endtime.gmt  <- as.POSIXct(endtime, tz="GMT")
    ##  sequence days from begin to end
    dates.gmt <- seq.POSIXt(trunc(begintime.gmt, units="days"),
                            trunc(endtime.gmt, units="days"), by="days")
  }
	
  ## create empty lists
  hold <- data.frame()
	all.MTS <- vector(mode="list", length=length(dates.gmt))
  
  ## use multiple cores if indicated by mcores=T
  if(mcores==T & .Platform$OS.type=="unix") {
    library(parallel)
    all.MTS <- mclapply(dates.gmt, FUN=retrievemts, station=station,
                        getvar=getvar, mc.cores=detectCores())
  } else if(mcores==T & .Platform$OS.type=="windows") {
    NULL
  } else {
    all.MTS <- lapply(dates.gmt, FUN=retrievemts, station=station, 
                      getvar=getvar)
  }
  
  ##  If localtime==T, convert back to CST/CDT and subset to begin/end time
  if(localtime==T) {
    all.MTS <- lapply(all.MTS,
                      function(x) {
                        x$timestamp <- 
                          as.POSIXct(format(x$timestamp, tz="America/Chicago"))
                        return(x)
                      })
    ##  Subset data according to begin and end time
    all.MTS[[1]] <- subset(all.MTS[[1]], timestamp>=begintime.local & 
                           timestamp<=endtime.local)
    all.MTS[[length(all.MTS)]] <- subset(all.MTS[[length(all.MTS)]],
                                         timestamp>=begintime.local & 
                                           timestamp<=endtime.local)
  } else {
    ##  Subset data according to begin and end time
    all.MTS[[1]] <- subset(all.MTS[[1]], timestamp>=begintime.gmt & 
                           timestamp<=endtime.gmt)
    all.MTS[[length(all.MTS)]] <- subset(all.MTS[[length(all.MTS)]],
                                         timestamp>=begintime.gmt & 
                                           timestamp<=endtime.gmt)
  }
  
  ## use ldply from plyr package to return list as dataframe
  return(ldply(all.MTS))
}
