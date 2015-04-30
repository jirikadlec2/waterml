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
#' @param startDate The start date in "yyyy-mm-dd" format
#' @param endDate The end date in "yyyy-mm-dd" format
#' @param daily Defaults to NULL. If you set daily="max", daily="min" or daily="mean", then the
#' data values are aggreagted to daily time step.
#' @keywords waterml
#' @export
#' @examples
#' #example 1: Get Values from a known site and variable from RushValley server
#' GetValues("http://worldwater.byu.edu/app/index.php/rushvalley/services/cuahsi_1_1.asmx",
#'            site="Ru5BMMA", variable="SRS_Nr_NDVI", startDate="2014-11-01", endDate="2014-11-02",
#'            daily="max")
#'
#' #example 2: Get values from a random  variable at a site
#' server <- "http://hydrodata.info/chmi-h/cuahsi_1_1.asmx?wsdl"
#' siteCode <- "CHMI-H:764"
#' siteInfo <- GetSiteInfo(server, siteCode)
#' series <- siteInfo[sample(nrow(siteInfo),1),]
#' values <- GetValues(server, series$FullSiteCode,
#'           series$FullVariableCode, Sys.Date()-10, Sys.Date())
#' if(!is.null(values)) {
#'  plot(values, type="l", main=series$SiteName,
#'  ylab=paste(series$VariableName,
#'  series$UnitAbbreviation))
#' }
#'

GetValues <- function(server, siteCode, variableCode, startDate, endDate, daily=NULL) {
  m <- regexpr(".asmx", server)
  base.url <- substr(server, 0, m+nchar(".asmx")-1)
  values.url <- paste(base.url, "/GetValuesObject", sep="")

  url = paste(values.url, "?location=", siteCode,
              "&variable=", variableCode,
              "&startDate=",startDate, "&endDate=",endDate, "&authToken=",
              sep="")
  print("fetching values from server...")

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
      dt <- c(dt, xmlAttrs(vals[[j]])["dateTime"])
      val <- c(val, as.numeric(xmlValue(vals[[j]])))
    }
  }

  if (length(val) == 0) {
    print(paste("no data values found:", url))
    return(NULL)
  }


  df <- data.frame("time"=as.POSIXct(dt), "DataValue"=val)
  df[df$DataValue == noData,2] <- NA

  if (!is.null(daily)) {
    validdata <- na.omit(df)
    if (nrow(validdata) == 0) {
      print("no valid data found!")
      return (NULL)
    }
    validdata$time <- as.Date(as.POSIXct(validdata$time))
    if (daily=="max") {
      dailyMax = aggregate(validdata$DataValue, list(validdata$time), max)
      names(dailyMax)[1] <- "time"
      names(dailyMax)[2] <- "DataValue"
      return(dailyMax)
    } else if (daily=="mean") {
      dailyMean = aggregate(validdata$DataValue, list(validdata$time), mean)
      names(dailyMean)[1] <- "time"
      names(dailyMax)[2] <- "DataValue"
      return(dailyMean)
    }
  }

  return(df)
}
