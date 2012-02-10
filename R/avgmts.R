avgmts <- function(mts, groups) {
  ## Averages MTS data frame by station, hour, day, month, or year
  ## Arguments:
  ##  mts: MTS data frame provided by mts()
  ##  by: character, indicating groups to average over; combination of station,
  ##    hour, day, month, and/or year
  ## Returns: data frame
  
  ## change by to lowercase
  groups <- tolower(groups)
  
  ## check by for appropriate grouping
  if(all(groups %in% c("station", "hour", "day", "month", "year"))==FALSE) {
    stop(c("Grouping variables must be station, hour, day, month, or year"))
  }
  
  ## set list for grouping variables
  group.list <- vector(mode="list", length=length(groups)+1)
  
  ## set first grouping to station, identified by mts$STID
  group.list[[1]] <- mts$STID
  
  ## for loop to set grouping variables equal to 'by'
  for(i in 1:length(groups)) {
    if(groups[i]=="hour") group.list[[i+1]] <- format(mts$TIME, "%H")
    else if(groups[i]=="day") group.list[[i+1]]  <- format(mts$TIME, "%d")
    else if(groups[i]=="month") group.list[[i+1]] <- format(mts$TIME, "%m")
    else if(groups[i]=="year") group.list[[i+1]] <- format(mts$TIME, "%Y")
  }
  
  ## add names to group list
  names(group.list) <- c("STID",toupper(groups))
  
  ## variables to average
  avg.var <- colnames(mts)[names(mts)!="STID" & names(mts)!="STNM" 
                          & names(mts)!="TIME"]
  
  ## calculate averages based on grouping variables
  mts.avg <- aggregate(mts[,avg.var], by=group.list, FUN=mean, na.rm=T)
  return(mts.avg)
}