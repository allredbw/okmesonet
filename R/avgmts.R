avgmts <- function(mts, groups, metric="mean") {
  ## Averages MTS data frame by hour, day, month, or year
  ## Arguments:
  ##  mts: MTS data frame provided by mts()
  ##  by: character, indicating groups to average over:
  ##    hour, day, month, and/or year
  ## Returns: data frame
  
  ## change by to lowercase
  groups <- tolower(groups)
  
  ## check by for appropriate grouping
  if(all(groups %in% c("station", "hour", "day", "month", "year"))==FALSE) {
    stop(c("Grouping variables must be hour, day, month, or year"))
  }
  
  ## check metric for mean, max, or min
  if(any(metric %in% c("mean","max","min"))==FALSE) {
    stop(c("metric must be mean, max, or min"))
  }
  
  ## set list for grouping variables
  group.list <- vector(mode="list", length=length(groups)+2)
  
  ## set first grouping to station, identified by mts$STID
  group.list[[1]] <- mts$STID
  ## set second grouping to station number, identified by mts$STNM
  group.list[[2]] <- mts$STNM
  
  ## for loop to set grouping variables equal to 'by'
  for(i in 1:length(groups)) {
    if(groups[i]=="hour") group.list[[i+2]] <- format(mts$TIME, "%H")
    else if(groups[i]=="day") group.list[[i+2]]  <- format(mts$TIME, "%d")
    else if(groups[i]=="month") group.list[[i+2]] <- format(mts$TIME, "%m")
    else if(groups[i]=="year") group.list[[i+2]] <- format(mts$TIME, "%Y")
  }
  
  ## add names to group list
  names(group.list) <- c("STID", "STNM", toupper(groups))
  
  ## variables to average
  ## RAIN is excluded due to its cumulative nature
  avg.var <- colnames(mts)[names(mts)!="STID" & names(mts)!="STNM" 
                          & names(mts)!="TIME" & names(mts)!="RAIN"]
  
  ## calculate averages based on grouping variables
  mts.avg <- aggregate(mts[,avg.var], by=group.list, FUN=metric, na.rm=T)
  return(mts.avg)
}