mts <- function(begintime, endtime, station, getvar, localtime=T, .mcores=F) {
  ## Gets Mesonet MTS file from Mesonet homepage
  ## Args:
  ##  begintime: beginning date,given as 'YYYY-MM-DD 00:00'
  ##  endtime: end date, given as 'YYYY-MM-DD 00:00'
  ##  station: four letter character ID for Mesonet station, lowercase
  ##  getvar: variables to retrieve
  ##  localtime: logical to indicate the use of Oklahoma local time, else 
  ##    use GMT
  ##  .mcores: logical to indicate use of foreach and multiple cores
  ## Returns: dataframe of MTS files
  library(plyr)
  
  ## available variables
  variables <- c("STID", "STNM", "RELH", "TAIR", "WSPD", "WVEC", "WDIR", "WDSD", 
                 "WSSD", "WMAX", "RAIN", "PRES", "SRAD", "TA9M", "WS2M", "TS10", 
                 "TB10", "TS05", "TB05", "TS30", "TR05", "TR25", "TR60", "TR75",
                 "all")
  
  ## check to see if getvar matches available variables
  if(all(getvar %in% variables)==FALSE) {
    stop(c("Desired variables do not match available variables. ",
           "See http://www.mesonet.org/files/parameter_description_readme.pdf ",
         "for list.")) }
  
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
  
  ## use multiple cores if indicated by .mcores=T
  if(.mcores==T) {
    library(doMC)
    registerDoMC()
    
    ## Use foreach and %dopar% to utilize multiple cores
    ## errorhandling set to 'remove'; if an evaluation error occurs, the result
    ## will be removed
    all.MTS <- foreach(i = 1:length(dates.gmt),  
                       .errorhandling = "remove") %dopar% {
  		date.long <- format.POSIXct(dates.gmt[i], format="%Y%m%d")
      
      ## read MTS from Mesonet website
      hold <- read.csv(paste("http://www.mesonet.org/index.php/dataMdfMts/dataController/getFile/", 
                             date.long, station, "/mts/TEXT/", sep = ""), 
                       skip = 2, header = T, as.is = T, sep = "", nrows = 288)
      
  		## IMPORTANT: convert 'TIME' field to timestamp
      ## TIME represents the number of minutes from base time specific 
      ## in MTS file
      ## see http://www.mesonet.org/wiki/Public:MDF_Format
      ## this appears to be always 00:00:00 UTC
      hold$timestamp <- as.POSIXct(hold$TIME*60, origin=dates.gmt[i])
      
      if(all(getvar != "all")==T) {
        return(hold[, c("STID", "STNM", "timestamp", getvar)])
      } else {
          return(hold)
      }
  	}
  } else {
    for(i in 1:length(dates.gmt)) {
      date.long <- format.POSIXct(dates.gmt[i],format="%Y%m%d")
      
      ## read MTS from Mesonet website
      all.MTS[[i]] <- read.csv(paste("http://www.mesonet.org/index.php/dataMdfMts/dataController/getFile/",
                                     date.long, station, "/mts/TEXT/", sep = ""),
                               skip = 2, header = T, as.is = T, sep = "",
                               nrows = 288)
      
    	## IMPORTANT: convert 'TIME' field to timestamp
      ## TIME represents the number of minutes from base time specific in 
      ## MTS file
      ## see http://www.mesonet.org/wiki/Public:MDF_Format
      ## this appears to be always 00:00:00 UTC
      all.MTS[[i]]$timestamp <- as.POSIXct(all.MTS[[i]]$TIME*60, 
                                           origin=dates.gmt[i])
      
      if(all(getvar != "all")==T) {
        all.MTS[[i]] <- all.MTS[[i]][, c("STID", "STNM", "timestamp", getvar)]
      }
    }
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
