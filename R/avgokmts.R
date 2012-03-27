#' Average an Oklahoma Mesonet time series data frame
#' 
#' Averages or summarizes an Oklahoma Mesonet time series (MTS) data 
#' frame returned by \code{\link{okmts}}. Summary can be over hour, day, 
#' month, or year. Precipitation (RAIN) is also returned as totals within a 
#' given time period.
#'
#' @param mts data frame returned by \code{okmts}.
#' @param timeframe character string indicating timeframe to average over. May
#'  include "hour", "day", "month", or "year".
#' @param metric function to summarize with. Default is "mean" (average), but
#' may also include "min" and "max" for minimun and maximum, respectively.

#' @export

#' @return A data frame summarizing Mesonet measurements by station and given
#' time period.

#' @examples
#' \dontrun{
#' ## Retrieve Bessie station MTS files for 00:00 Jun 01, 1997
#' ## through 23:55 Oct 31, 1997
#' bess.mts <- okmts(begintime="1997-06-01 00:00:00",
#'  endtime="1997-10-31 23:55", station="bess")
#'
#' ## Average MTS data by day.
#' bess.mts.avg  <- avgokmts(bess.mts, timeframe="day")
#' }


avgokmts <- function(mts, timeframe, metric="mean") {
  ## Averages MTS data frame by hour, day, month, or year
  ## Arguments:
  ##  mts: MTS data frame provided by okmts()
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
  avg.var <- colnames(mts)[names(mts)!="STID" & names(mts)!="STNM" 
                          & names(mts)!="TIME" ]
  
  ## calculate averages based on grouping variables
  mts.avg <- aggregate(mts[,avg.var], by=timeframe.list, FUN=metric, na.rm=T)
  
  ## if avg.var is only one variable, it is returned as "x" in mts.avg
  ## change "x" to appropriate name
  if(length(avg.var)==1) {
    colnames(mts.avg)[colnames(mts.avg)=="x"] <- avg.var
  }
  
  ## calculate rain average
  if(any(colnames(mts) %in% "RAIN")) {
    mts.avg$RAIN  <- totalprecip(mts, timeframe)
  }
  return(mts.avg)
}