#' GetValues
#'
#' This function gets the time series data values from the WaterML web service
#'
#' @import XML
#' @param server The URL of the web service ending with .asmx,
#'  for example: http://worldwater.byu.edu/interactive/rushvalley/services/index.php/cuahsi_1_1.asmx
#' @param siteCode The site code. To get a list of available site codes, see GetSites() function
#'  and use the FullSiteCode field.
#' @param variableCode The variable code. To get a list of possible variable codes, see GetVariables()
#'  function and use the FullVariableCode field
#' @param startDate (optional) The start date in "yyyy-mm-dd" format
#' @param endDate (optional) The end date in "yyyy-mm-dd" format
#' @param daily (optional) If you set daily="max", daily="min" or daily="mean", then the
#' data values are aggreagted to daily time step.
#' @keywords waterml
#' @export
#' @examples
#' #example 1: Get Values from a known site and variable from RushValley server
#' GetValues("http://worldwater.byu.edu/app/index.php/rushvalley/services/cuahsi_1_1.asmx",
#'            site="Ru5BMMA", variable="SRS_Nr_NDVI", startDate="2014-11-01", endDate="2014-11-02",
#'            daily="max")
#'

GetValues <- function(server, siteCode, variableCode, startDate=NULL, endDate=NULL, daily=NULL) {
  m <- regexpr(".asmx", server)
  base.url <- substr(server, 0, m+nchar(".asmx")-1)
  values.url <- paste(base.url, "/GetValuesObject", sep="")

  #check startDate, endDate if it is null
  startDateParam <- ifelse(is.null(startDate), "", strftime(as.POSIXct(startDate), "%Y-%m-%dT%H:%M:%S"))
  endDateParam <- ifelse(is.null(endDate), "", strftime(as.POSIXct(endDate), "%Y-%m-%dT%H:%M:%S"))

  url = paste(values.url, "?location=", siteCode,
              "&variable=", variableCode,
              "&startDate=",startDateParam, "&endDate=",endDateParam, "&authToken=",
              sep="")
  print(paste("fetching values from ", url))

  doc <- xmlRoot(xmlTreeParse(url, getDTD=FALSE, useInternalNodes = TRUE))

  variableElement <- doc[[2]][["variable"]]
  if (is.null(variableElement)) {
    print(paste("no data values found:", url))
    return(NULL)
  }
  variable <- xmlToList(doc[[2]][["variable"]])
  noData <- as.numeric(variable$noDataValue)


  vals <- doc[[2]][["values"]]

  hasValues <- FALSE
  if (is.null(vals)){
    print(paste("no data values found:", url))
    return(NULL)

  }
  if (xmlValue(vals) == "") {
    print(paste("no data values found:", url))
    return(NULL)
  }

  valCount = xmlSize(vals)
  xmNames = xmlSApply(vals, xmlName)
  val = c()
  dt = c()
  for (j in 1:valCount){
    if(xmlName(vals[[j]]) == 'value') {
      dt <- c(dt, as.character(xmlAttrs(vals[[j]])["dateTime"]))
      val <- c(val, as.numeric(xmlValue(vals[[j]])))
    }
  }

  if (length(val) == 0) {
    print(paste("no data values found:", url))
    return(NULL)
  }

  #normal case: no aggregation
  df <- data.frame("time"=as.POSIXct(strptime(dt, "%Y-%m-%dT%H:%M:%S")), "DataValue"=val)
  df[df$DataValue == noData,2] <- NA

  #special case: daily data aggregation
  if (!is.null(daily)) {
    validdata <- na.omit(df)
    if (nrow(validdata) == 0) {
      print("no valid data found!")
      return (NULL)
    }
    validdata$time <- as.Date(as.POSIXct(validdata$time))
    dailyValues <- aggregate(validdata$DataValue, list(validdata$time), daily)
    names(dailyValues)[1] <- "time"
    names(dailyValues)[2] <- "DataValue"
    return(dailyValues)
  }

  return(df)
}
