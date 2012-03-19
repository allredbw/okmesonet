avgmts <- function(mts, timeframe, metric="mean") {
  ## Averages MTS data frame by hour, day, month, or year
  ## Arguments:
  ##  mts: MTS data frame provided by mts()
  ##  timeframe: character, indicating timeframe to average over:
  ##    hour, day, month, and/or year
  ## Returns: data frame
  
  ## change by to lowercase
  timeframe <- tolower(timeframe)
  
  if(length(timeframe)>1) {
    stop(paste("Only one grouping (timeframe) variable allowed"))
  }
  
  ## check by for appropriate grouping
  if(any(timeframe %in% c("hour", "day", "month", "year"))==FALSE) {
    stop(c("Grouping (timeframe) variable must be hour, day, month, or year"))
  }
  
  ## check metric for mean, max, or min
  if(any(metric %in% c("mean","max","min"))==FALSE) {
    stop(c("metric must be mean, max, or min"))
  }
  
  ## set list for grouping variables
  timeframe.list <- vector(mode="list", length=length(timeframe)+2)
  
  ## set first grouping to station, identified by mts$STID
  timeframe.list[[1]] <- mts$STID
  ## set second grouping to station number, identified by mts$STNM
  timeframe.list[[2]] <- mts$STNM
  

  ## set grouping variables
  if(timeframe=="hour") {
    timeframe.list[[3]] <- format(mts$TIME, "%H")
    timeframe.list[[4]] <- format(mts$TIME, "%d")
    timeframe.list[[5]] <- format(mts$TIME, "%m")
    timeframe.list[[6]] <- format(mts$TIME, "%Y")
    names(timeframe.list) <- c("STID", "STNM", "HOUR", "DAY", "MONTH", "YEAR")
  } else if (timeframe=="day") {
      timeframe.list[[3]] <- format(mts$TIME, "%d")
      timeframe.list[[4]] <- format(mts$TIME, "%m")
      timeframe.list[[5]] <- format(mts$TIME, "%Y")
      names(timeframe.list) <- c("STID", "STNM", "DAY", "MONTH", "YEAR")
  } else if(timeframe=="month") {
    timeframe.list[[3]] <- format(mts$TIME, "%m")
    timeframe.list[[4]] <- format(mts$TIME, "%Y")
    names(timeframe.list) <- c("STID", "STNM","MONTH", "YEAR")
  } else if(timeframe=="year") {
    timeframe.list[[3]] <- format(mts$TIME, "%Y")
    names(timeframe.list) <- c("STID", "STNM", "YEAR")
  }

  ## variables to average
  ## RAIN is excluded due to its cumulative nature
  avg.var <- colnames(mts)[names(mts)!="STID" & names(mts)!="STNM" 
                          & names(mts)!="TIME" & names(mts)!="RAIN"]
  
  ## calculate averages based on grouping variables
  mts.avg <- aggregate(mts[,avg.var], by=timeframe.list, FUN=metric, na.rm=T)
  return(mts.avg)
}