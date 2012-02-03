retrievemts <- function(station, datemts, getvar) {
  ## Retrieve MTS file from Mesonet website
  ## Arguments:
  ##  station: four letter character ID for Mesonet station, lowercase
  ##  datemts: date of MTS file to retrieve, POSIXct format
  ##  getvar: variables to retrieve
  ## Returns: dataframe containing MTS file with timestamp
  
  ## read MTS from Mesonet website
  hold <- read.csv(paste("http://www.mesonet.org/index.php/dataMdfMts/dataController/getFile/", 
                         format.POSIXct(datemts, format="%Y%m%d"), station, 
                         "/mts/TEXT/", sep = ""), 
                   skip = 2, header = T, as.is = T, sep = "", nrows = 288)
  
  ## IMPORTANT: convert 'TIME' field to timestamp
  ## TIME represents the number of minutes from base time specific 
  ## in MTS file
  ## see http://www.mesonet.org/wiki/Public:MDF_Format
  ## this appears to be always 00:00:00 UTC
  hold$TIME <- as.POSIXct(hold$TIME*60, origin=datemts)
  
  ## remove MTS TIME variable: can be confusing with timestamp present
  #hold$TIME <- NULL
  
  ## reorder data frame with STID, STNM, and timestamp first
  
  ## variables to put at end
  endvar <- colnames(hold)[names(hold)!="STID" & names(hold)!="STNM" 
                           & names(hold)!="TIME"]
  
  ## replace data frame with desired column order by subsetting
  hold <- hold[,c("STID","STNM","TIME",endvar)]
  
  ## return data frame with desired vairables
  if(getvar == "ALL") {
    return(hold)
  } else {
    return(hold[, c("STID", "STNM", "TIME", getvar)])
  }
}