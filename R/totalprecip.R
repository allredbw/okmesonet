totalprecip <- function(mts, timeframe) {
  ## Called by avgmts()
  ## Calculate total rainfall according to grouping (timeframe) variables.
  ## Arguments:
  ##  mts: MTS data frame provided by okmts()
  ##  timeframe.list: character given by avgmts()
  ## Returns: vector
  
  ## YYYY-MM-DD 00:00:00 UTC represents the previous day total rainfall
  ## substract 1 second to get appropriate day/hour
  
  mts.subset <- mts
  mts.subset$TIME.gmt <- as.POSIXct(format(mts.subset$TIME, tz="GMT"),
                                    tz="GMT")
  mts.subset$TIME[which(format(mts.subset$TIME.gmt, 
                               "%H:%M:%S")=="00:00:00")] <- 
    mts.subset$TIME[which(format(mts.subset$TIME.gmt, 
                                 "%H:%M:%S")=="00:00:00")] - 1
  
  ## remove anything outside original timestamps
  mts.subset <- subset(mts.subset, TIME>=min(mts$TIME) & TIME<=max(mts$TIME))
  
  ## aggregate by hour
  
  ## set list for grouping variables
  timeframe.list <- vector(mode="list", length=6)
  ## set first grouping to station, identified by mts$STID
  timeframe.list[[1]] <- mts.subset$STID
  ## set second grouping to station number, identified by mts$STNM
  timeframe.list[[2]] <- mts.subset$STNM
  
  ## set grouping variables
  timeframe.list[[3]] <- format(mts.subset$TIME, "%H")
  timeframe.list[[4]] <- format(mts.subset$TIME, "%d")
  timeframe.list[[5]] <- format(mts.subset$TIME, "%m")
  timeframe.list[[6]] <- format(mts.subset$TIME, "%Y")
  names(timeframe.list) <- c("STID", "STNM", "HOUR", "DAY", "MONTH", "YEAR")
  
  ## calculate rain totals using max-min for each hour
  rain.total.hour <- aggregate(mts.subset$RAIN, by=timeframe.list, 
                               FUN=function(RAIN) max(RAIN) - min(RAIN))
  
  if(any(timeframe %in% c("day", "month", "year"))) {
    ## set list for grouping variables
    day.list <- list(rain.total.hour$STID, rain.total.hour$STNM,
                     rain.total.hour$MONTH, rain.total.hour$YEAR,
                     rain.total.hour$DAY)
    names(day.list) <- c("STID", "STNM", "MONTH", "YEAR", "DAY")
    rain.total.day <- aggregate(rain.total.hour$x, by=day.list,
                                FUN=sum, na.rm=T)
    
    if(any(timeframe %in% c("month", "year"))) {
      month.list <- list(rain.total.day$STID, rain.total.day$STNM,
                         rain.total.day$MONTH, rain.total.day$YEAR)
      names(month.list) <- c("STID", "STNM", "MONTH", "YEAR")
      rain.total.month <- aggregate(rain.total.day$x, by=month.list,
                                    FUN=sum, na.rm=T)
      if(timeframe=="year") {
        year.list <- list(rain.total.month$STID, rain.total.month$STNM,
                          rain.total.month$YEAR)
        names(year.list) <- c("STID", "STNM", "YEAR")
        rain.total.year <- aggregate(rain.total.month$x, by=year.list,
                                      FUN=sum, na.rm=T)
      }
    }
  }
  
  if(timeframe=="hour") return(rain.total.hour$x)
  else if (timeframe=="day") return(rain.total.day$x)
  else if (timeframe=="month") return(rain.total.month$x)
  else if (timeframe=="year") return(rain.total.year$x)
}
