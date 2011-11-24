getMTS <- function(begintime, endtime, station, getvar) {
  ## Gets Mesonet MTS file from Mesonet homepage
  ## Args:
  ##  begintime: beginning date,given as 'YYYY-MM-DD'
  ##  endtime: end date, given as 'YYYY-MM-DD'
  ##  station: four letter character ID for Mesonet station, lowercase
  ##  getvar: variables to retrieve
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
  
  ## format start and end dates as 
  dates<-seq.POSIXt(as.POSIXct(begintime, tz="GMT"), 
                    as.POSIXct(endtime, tz="GMT"), "days")  
	
  ## create empty lists
  hold<-data.frame()
	all.MTS<-vector(mode="list",length=length(dates))
  
  ## Use foreach and %dopar% to utilize multiple cores
  all.MTS<-foreach(i = 1:length(dates), #.combine = rbind, 
                   .errorhandling = "remove") %dopar% {
		date.long<-format.POSIXct(dates[i],format="%Y%m%d")
    
    ## read MTS from Mesonet website
    hold<-read.csv(paste("http://www.mesonet.org/index.php/dataMdfMts/dataController/getFile/", 
                         date.long, station, "/mts/TEXT/", sep = ""), skip = 2, 
                   header = T, as.is = T, sep = "", nrows = 288)
    
		## IMPORTANT: convert 'TIME' field to timestamp
    ## TIME represents the number of minutes from base time specific in MTS file
    ## see http://www.mesonet.org/wiki/Public:MDF_Format
    ## this appears to be always 00:00:00 UTC
    hold$timestamp<-as.POSIXct(hold$TIME*60, origin=dates[i])
    
    if(getvar == "all") {
      return(hold)
      } else {
        return(hold[, c("STID", "STNM", "timestamp", getvar)])
      }
	}
  return(ldply(all.MTS))
}
