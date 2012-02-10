avgmts <- function(mts, by=c("station")) {
  ## Averages MTS data frame by station, hour, day, month, or year
  ## Arguments:
  ##  mts: MTS data frame provided by mts()
  ##  by: character, indicating groups to average over; combination of station,
  ##    hour, day, month, and/or year
  ## Returns: data frame
  
  ## change by to lowercase
  by <- tolower(by)
  
  ## check by for appropriate grouping
  if(all(by %in% c("station", "hour", "day", "month", "year"))==FALSE) {
    stop(c("Grouping variables must be station, hour, day, month, or year"))
  }
  
  
}