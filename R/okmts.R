#' Retrieve an Oklahoma Mesonet time series file
#' 
#' Function to retrieve an \href{http://www.mesonet.org/}{Oklahoma Mesonet} 
#' time series (MTS) file for a given time period and station. Alternatively, 
#' if station is omitted and latitude and longitude are given, it retrieves the 
#' MTS for the closest operating station during the given time period.
#'
#' The \href{http://www.mesonet.org/}{Oklahoma Mesonet} is a network of 
#' automated climate monitoring stations throughout the state of Oklahoma, USA; 
#' data collection began on Jan 01, 1994.
#' As of February 2012, there are 120 active stations, with an additional 15
#' stations decommissioned but with available data. Measurements are recorded 
#' every five minutes and sent to a central facility for verification and 
#' quality control by the Oklahoma Climatological Survey.
#'
#' The timestamps used to define the time period for \code{mts} can be either
#' character strings or POSIXct objects. Character strings should be in the 
#' format "\code{2009-09-08 09:05}" or "\code{2005-12-13 00:00:00}". POSIXct 
#' objects need to have a timezone specified; \code{mts} converts timezones
#' appropriately to download correct MTS files.
#'
#' Four letter Mesonet station identifier can be found in \code{\link{stations}}
#' or on the \href{http://www.mesonet.org/}{Oklahoma Mesonet} website.
#'
#' Available Mesonet variables and units are described in the 'Parameter
#' Description' 
#' \href{http://www.mesonet.org/files/parameter_description_readme.pdf}{readme}
#' file or \href{http://www.mesonet.org/wiki/Public:MDF_Format}{MTS 
#' specification}.
#'
#' Time records of Oklahoma MTS files are stored in Coordinated Universal Time
#' (UTC or GMT). To easily convert to local Oklahoma time, \code{localtime=TRUE}
#' indicates that times used to define the time period are local Oklahoma time.
#' Timezone conversion is done internally, and accounts for Daylight Savings
#' Time (as reliably as R can; see \link{timezone}).
#' \code{localtime=TRUE} will also direct \code{mts} to output in local Oklahoma
#' time. \code{localtime=FALSE} indicates that UTC or GMT is used for both time
#' input and output. If time inputs are of POSIXct class, \code{localtime} only 
#' affects time output.
#'
#' The use of multiple cores can decrease the time used to retrieve data for
#' lengthy time periods. \code{mcores=TRUE} will direct \code{mts} to use the 
#' number cores in the current machine (determined by 
#' \code{\link[parallel]{detectCores}}).
#'
#' @param begintime character string or POSIXct object. Start time of time 
#' period. Character strings must be formated as 'YYYY-MM-DD HH:MM:SS'.
#' @param endtime character string or POSIXct object. End time of time 
#' period. Character strings must be formated as 'YYYY-MM-DD HH:MM:SS'.
#' @param station character string. Four letter Mesonet station identifier. See 
#' 'Details'.
#' @param lat numeric: latitude of point of interest in decimal degrees.
#' @param lon numeric: longitude of point of interest in decimal degrees.
#' @param getvar character string. Mesonet variables to retrieve. See 'Details'.
#' @param localtime logical; if \code{TRUE}, input and output time is local to
#'  Oklahoma. If \code{FALSE}, input and output time is Coordinated Universal 
#'  Time (UTC or GMT). See 'Details'.
#' @param mcores logical; if \code{TRUE}, use multiple cores for file retrieval.
#'  See 'Details'.

#' @export

#' @return A data frame with values from MTS files for the given station, time 
#' period, and desired variables. Timestamps for each measurement are returned 
#' as POSIXct class; timezone is determined by \code{localtime}.

#' @examples
#' \dontrun{
#' ## Retrieve Bessie station MTS files for 00:00 Jun 01, 1997
#' ## through 23:55 Oct 31, 1997
#' bess.mts <- mts(begintime="1997-06-01 00:00:00",
#'  endtime="1997-10-31 23:55", station="bess")
#'
#' ## Use POSIXct class to retrieve Medicine Park station air
#' ## temperature for 09:30 through 20:30 Aug 12, 2004
#' ## Set times, using 'America/Chicago' for Oklahoma timezone
#' medi.time <- c(as.POSIXct("2004-08-12 09:30", tz="America/Chicago"),
#'  as.POSIXct("2004-08-12 20:30", tz="America/Chicago"))
#' medi.air <- mts(begintime=medi.time[1], endtime=medi.time[2],
#'  station="medi", getvar="TAIR")
#'
#' ## Download all data for 2001 for station closest to 
#' ## 36.575284 latitude, -99.478455 longitude, using multiple cores
#' stn.mts <- mts(begintime="2001-01-01 00:00:00", 
#'  endtime="2001-12-31 23:55:00", lat=36.575284, long=-99.478455, mcores=T)
#'
#' ## Retrieve Idabel station MTS data for 00:00 through 12:00 UTC (GMT)
#' ## Nov 23, 2003
#' ## Time values are returned in UTC
#' idab.mts <- mts(begintime="2003-11-23 00:00:00", 
#'  endtime="2003-11-23 12:00:00", station="idab", localtime=F)
#'
#' ## Combine air temperature with bison movement data.
#' ## Retrieve Foraker station MTS files for 00:00 Jan 31, 2011 
#' ## through 15:00 Feb 05, 2011
#' fora.mts <- mts(begintime="2011-01-31 00:00:00", 
#'  endtime="2011-02-05 15:00:00", station="fora")
#' ## Round bison timestamp down to five minute mark
#' bison$newtime <- as.POSIXlt(bison$timestamp)
#' bison$newtime$sec <- round(bison$newtime$sec, -2)
#' bison$newtime$min <- as.integer(format(bison$newtime, "%M")) %/% 5 * 5
#' bison$newtime <- as.POSIXct(bison$newtime)
#' ## Add Foraker station air temperature to bison data
#' bison$TAIR <- fora.mts$TAIR[match(bison$newtime, fora.mts$TIME)]
#' }

mts <- function(begintime, endtime, station=NULL, lat=NULL, lon=NULL, 
                getvar="ALL", localtime=TRUE, mcores=FALSE) {
  ## Gets Mesonet MTS file from Mesonet homepage
  ## Arguments:
  ##  begintime: beginning date,given as 'YYYY-MM-DD HH:MM:SS'
  ##  endtime: end date, given as 'YYYY-MM-DD HH:MM:SS'
  ##  station: four letter character ID for Mesonet station
  ##  lat: latitude of point location, decimal degrees
  ##  lon: longitude of point locaiton, decimal degrees
  ##  getvar: variables to retrieve
  ##  localtime: logical to indicate the use of Oklahoma local time, else 
  ##    use GMT
  ##  mcores: logical to indicate use of foreach and multiple cores
  ## Returns: dataframe of MTS files
  
  ## load plyr package
  if(require(plyr)==F) stop(c("okmesonet requires the 'plyr' package. ",
                                "Please install with 'install.packages()'"))
  
  ## check to see if begintime and endtime are of class character or POSIXct 
  ## set *.local and *.gmt appropriately
  if(is.character(begintime)==T & is.character(endtime)==T) {
    if(localtime==T) {
      ## convert character timestamp to POSIXct, timezone America/Chicago
      begintime.local <- as.POSIXct(begintime, tz="America/Chicago")
      endtime.local  <- as.POSIXct(endtime, tz="America/Chicago")
      ## Convert to timezone GMT for file retrieval
      begintime.gmt <- as.POSIXct(format(begintime.local, tz="GMT"), tz="GMT")
      endtime.gmt <- as.POSIXct(format(endtime.local, tz="GMT"), tz="GMT")
    } else {
      ## convert character timestamp to POSIXct, timezone GMT for file retrieval
      begintime.gmt <- as.POSIXct(begintime, tz="GMT")
      endtime.gmt  <- as.POSIXct(endtime, tz="GMT")
      ## convert begintime.gmt to timezone America/Chicago for continuity
      begintime.local <- as.POSIXct(format(begintime.gmt, tz="America/Chicago"),
                                    tz="America/Chicago")
      endtime.local <- as.POSIXct(format(endtime.gmt, tz="America/Chicago"),
                                  tz="America/Chicago")
    }
  } else if(any(class(begintime)=="POSIXct") & any(class(endtime)=="POSIXct")) {
    if(localtime==T) {
      ## convert POSIXct timestamp to timezone America/Chicago
      ## used for subsetting desired variables
      begintime.local <- as.POSIXct(format(begintime, tz="America/Chicago"),
                                    tz="America/Chicago")
      endtime.local <- as.POSIXct(format(endtime, tz="America/Chicago"),
                                  tz="America/Chicago")
      ## convert POSIXct timestamp to timezone GMT for file retrieval
      begintime.gmt <- as.POSIXct(format(begintime, tz="GMT"), tz="GMT")
      endtime.gmt <- as.POSIXct(format(endtime, tz="GMT"), tz="GMT")
    } else {
      ## convert POSIXct timestamp to timezone GMT for file retrieval
      begintime.gmt <- as.POSIXct(format(begintime, tz="GMT"), tz="GMT")
      endtime.gmt <- as.POSIXct(format(endtime, tz="GMT"), tz="GMT")
      ## convert begintime.gmt to timezone America/Chicago for continuity
      begintime.local <- as.POSIXct(format(begintime.gmt, tz="America/Chicago"),
                                    tz="America/Chicago")
      endtime.local <- as.POSIXct(format(endtime.gmt, tz="America/Chicago"),
                                  tz="America/Chicago")
    }
  } else {
    ## if not character or POSIXct, stop and give error message
    stop(c("begintime and endtime must both be entered as YYYY-MM-DD HH:MM:SS",
           " or a POSIXct class"))
  }
  
  ## if station is NULL and lat and long are given, retrieve closest staion
  ## with nearstn()
  if(length(station)==0 & is.numeric(lat)==T & is.numeric(lon)==T) {
    station <- nearstn(pnt.lon=lon, pnt.lat=lat, startdate=begintime.local, 
                       enddate=endtime.local)
  }
  
  ## available Mesonet variables
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
         "for available variables.")) }
  
  ## if getvar contains "ALL", remove anything else
  if(any(getvar=="ALL")==TRUE) getvar <- "ALL"
  
  ## convert station to lowercase
  station <- tolower(station)
  
  ##  sequence GMT days from begin to end for file retrieval
  dates.gmt <- seq.POSIXt(trunc(begintime.gmt, units="days"),
                          trunc(endtime.gmt, units="days"), by="days")
  ## create empty lists
	all.MTS <- vector(mode="list", length=length(dates.gmt))
  
  ## use multiple cores if indicated by mcores=T
  if(mcores==T & .Platform$OS.type=="unix") {
    library(parallel)
    all.MTS <- mclapply(dates.gmt, FUN=retrievemts, station=station,
                        getvar=getvar, mc.cores=detectCores())
  } else if(mcores==T & .Platform$OS.type=="windows") {
    library(parallel)
    c1 <- makeCluster(getOption("cl.cores", detectCores()))
    clusterExport(c1, varlist = list("retrievemts", "dates.gmt"))
    all.MTS <- parLapply(c1, dates.gmt, fun=retrievemts, station=station, 
                         getvar=getvar)
    stopCluster(c1)
  } else {
    all.MTS <- lapply(dates.gmt, FUN=retrievemts, station=station, 
                      getvar=getvar)
  }
  
  ##  If localtime==T, convert back to CST/CDT and subset to begin/end time
  if(localtime==T) {
    all.MTS <- lapply(all.MTS,
                      function(x) {
                        x$TIME <- 
                          as.POSIXct(format(x$TIME, tz="America/Chicago"))
                        return(x)
                      })
    ##  Subset data according to begin and end time
    all.MTS[[1]] <- subset(all.MTS[[1]], TIME>=begintime.local & 
                           TIME<=endtime.local)
    all.MTS[[length(all.MTS)]] <- subset(all.MTS[[length(all.MTS)]],
                                         TIME>=begintime.local & 
                                           TIME<=endtime.local)
  } else {
    ##  Subset data according to begin and end time
    all.MTS[[1]] <- subset(all.MTS[[1]], TIME>=begintime.gmt & 
                           TIME<=endtime.gmt)
    all.MTS[[length(all.MTS)]] <- subset(all.MTS[[length(all.MTS)]],
                                         TIME>=begintime.gmt & 
                                           TIME<=endtime.gmt)
  }
  
  ## use ldply from plyr package to return list as dataframe
  return(ldply(all.MTS))
}
