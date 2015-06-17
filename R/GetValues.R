#' GetValues
#'
#' This function gets the time series data values from the WaterML web service
#'
#' @import XML
#' @param server The URL of the web service ending with .asmx,
#'  for example: http://worldwater.byu.edu/interactive/rushvalley/services/index.php/cuahsi_1_1.asmx?WSDL
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
#' GetValues("http://worldwater.byu.edu/app/index.php/rushvalley/services/cuahsi_1_1.asmx?WSDL",
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

  # if server ends with ?WSDL or ?wsdl, we assume that service is SOAP
  # otherwise, assume that service is REST
  SOAP <- TRUE
  m <- regexpr("?WSDL|wsdl", server)
  if (m > 1) {
    url <- substr(server, 0, m - 2)
    SOAP <- TRUE
  } else {
    SOAP <- FALSE
  }

  #if the service is SOAP:
  if (SOAP) {
    versionInfo <- WaterOneFlowVersion(server)
    namespace <- versionInfo$Namespace
    version <- versionInfo$Version
    methodName <- "GetValuesObject"

    SOAPAction <- paste(namespace, methodName, sep="")
    envelope <- MakeSOAPEnvelope(namespace, methodName, c(location=siteCode,
                                                          variable=variableCode,
                                                          startDate=startDate,
                                                          endDate=endDate))
    response <- POST(url, body = envelope,
                     add_headers("Content-Type" = "text/xml", "SOAPAction" = SOAPAction))
    status.code <- http_status(response)$category
    print(paste("GetValues from", url, "...", status.code))

  } else {
    #REST
    response <- GET(server)
    status.code <- http_status(response)$category
    print(paste("GetValues from", url, "...", status.code))
  }

  ######################################################
  # Parsing the WaterML XML Data                       #
  ######################################################

  print("reading data values WaterML ...")
  doc <- tryCatch({
    content(response)
  }, warning = function(w) {
    print("Error reading WaterML: Bad XML format.")
    return(NULL)
  }, error = function(e) {
    print("Error reading WaterML: Bad XML format.")
    return(NULL)
  }
  )
  if (is.null(doc)) {
    return(NULL)
  }

  # specify the namespace information
  ns <- WaterOneFlowNamespace(version)

  # extract the data columns with XPath
  val = xpathSApply(doc, "//sr:value", xmlValue, namespaces=ns)
  dateTime = xpathSApply(doc, "//sr:value", xmlGetAttr, name="dateTime", namespaces=ns)
  timeOffset = xpathSApply(doc, "//sr:value", xmlGetAttr, name="timeOffset", namespaces=ns)
  censorCode = xpathSApply(doc, "//sr:value", xmlGetAttr, name="censorCode", namespaces=ns)
  dateTimeUTC = xpathSApply(doc, "//sr:value", xmlGetAttr, name="dateTime", namespaces=ns)
  methodCode = xpathSApply(doc, "//sr:value", xmlGetAttr, name="methodCode", namespaces=ns)
  sourceCode = xpathSApply(doc, "//sr:value", xmlGetAttr, name="sourceCode", namespaces=ns)
  qcCode = xpathSApply(doc, "//sr:value", xmlGetAttr, name="qualityControlLevelCode", namespaces=ns)

  nodata = as.numeric(xpathSApply(doc, "//sr:noDataValue"))
  #make the data frame
  df <- data.frame(
    time=as.POSIXct(strptime(dateTime, "%Y-%m-%dT%H:%M:%S")),
    DataValue=as.numeric(val),
    UTCOffset=as.numeric(timeOffset),
    CensorCode=censorCode,
    DateTimeUTC=as.POSIXct(strptime(dateTimeUTC, "%Y-%m-%dT%H:%M:%S")),
    MethodCode=methodCode,
    SourceCode=sourceCode,
    QualityControlLevelCode=qcCode,
    stringsAsFactors=FALSE
  )

  if (nrow(df) > 0) {
    print(paste("no data values found:", url))
    return(NULL)
  }

  #normal case: no aggregation
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
