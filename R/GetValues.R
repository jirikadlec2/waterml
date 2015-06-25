#' GetValues
#'
#' This function gets the time series data values from the WaterML web service
#'
#' @import XML
#' @import httr
#' @param server The URL of the web service ending with .asmx,
#'  for example: http://worldwater.byu.edu/interactive/rushvalley/services/index.php/cuahsi_1_1.asmx?WSDL
#' @param siteCode The site code. To get a list of available site codes, see GetSites() function
#'  and use the FullSiteCode field.
#' @param variableCode The variable code. To get a list of possible variable codes, see GetVariables()
#'  function and use the FullVariableCode field
#' @param startDate (optional) The start date in "yyyy-mm-dd" format
#' @param endDate (optional) The end date in "yyyy-mm-dd" format
#' @param methodID (optional) The ID of the observation method. To get a list of possible method IDs, see
#' methodID column in the output of GetSiteInfo(). If methodID is not specified, then the observations
#' in the output data.frame won't be filtered by method.
#' @param sourceID (optional) The ID of the source. To get a list of possible source IDs, see
#' sourceID column in the output of GetSiteInfo(). If sourceID is not specified, then the observations
#' in the output data.frame won't be filtered by source.
#' @param qcID (optional) The ID of the quality control level. Typically 0 is used for raw data and 1 is
#' used for quality controlled data. To get a list of possible quality control level IDs, see
#' QualityControlLevelID column in the output of GetSiteInfo(). If qcID is not specified, then the
#' observations in the output data.frame won't be filtered by quality control level.
#' @param daily (optional) If you set daily="max", daily="min" or daily="mean", then the
#' data values are aggreagted to daily time step.
#' @return a data.frame of data values with the following columns:
#' \itemize{
#' \item time: The local date/time of the observation. The data type is POSIXct.
#' \item DataValue: The observed data value
#' \item UTCOffset: The difference between local time and UTC time in hours
#' \item CensorCode: The code for censored observations. Possible values are nc (not censored),
#'             gt (greater than), lt (less than),
#'             nd (non-detect), pnq (present but not quantified)
#' \item DateTimeUTC: The UTC time of the observation. The data type is POSIXct.
#' \item MethodCode: The code of the method or instrument used for the observation
#' \item SourceCode: The code of the data source
#' \item QualityControlLevelCode: The code of the quality control level. Possible values are
#'             -9999 (Unknown), 0 (Raw data), 1 (Quality controlled data),
#'             2 (Derived products), 3 (Interpreted products), 4 (Knowledge products)
#' }
#' The output data.frame also has attributes with information about the status:
#' download.time, parse.time, download.status, parse.status
#' These attributes can be used for troubleshooting WaterOneFlow/WaterML server errors.
#' If parse status is "NO_VALUES_FOUND",
#' then this time series doesn't have any available data for the selected time period.
#' @keywords waterml
#' @export
#' @examples
#' #example 1: Get Values from a known site and variable from RushValley server
#' v1 <- GetValues("http://worldwater.byu.edu/app/index.php/rushvalley/services/cuahsi_1_1.asmx?WSDL",
#'            site="Ru5BMMA", variable="SRS_Nr_NDVI", startDate="2014-11-01", endDate="2014-11-02",
#'            daily="max")
#' #example 2: Get values from an external REST URL (in this case the Provo USGS NWIS site id 10163000)
#' url <- "http://waterservices.usgs.gov/nwis/dv/?format=waterml,1.1&sites=10163000&parameterCd=00060"
#' v2 <- GetValues(url)

GetValues <- function(server, siteCode, variableCode, startDate=NULL, endDate=NULL,
                      methodID=NULL, sourceID=NULL, qcID=NULL, daily=NULL) {

  # declare empty return data frame
  df <- data.frame()

  # trim any leading and trailing whitespaces in server
  server <- gsub("^\\s+|\\s+$", "", server)

  SOAP <- TRUE

  # if server ends with .asmx, we also assume that the service is SOAP and we add ?WSDL
  m1 <- regexpr("asmx$", server)
  if (m1 > 1) {
    server <- paste(server, "WSDL", sep="?")
  }

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

    #format the variable with the methodID, sourceID, qcID parameters
    if (!is.null(methodID)) {
      variableCode <- paste(variableCode, ":methodCode=", methodID, sep="")
    }
    if (!is.null(sourceID)) {
      variableCode <- paste(variableCode, ":sourceCode=", sourceID, sep="")
    }
    if (!is.null(qcID)) {
      variableCode <- paste(variableCode, ":qualityControlLevelCode=", qcID, sep="")
    }

    SOAPAction <- paste(namespace, methodName, sep="")
    envelope <- MakeSOAPEnvelope(namespace, methodName, c(location=siteCode,
                                                          variable=variableCode,
                                                          startDate=startDateParam,
                                                          endDate=endDateParam))
    headers <- c("Content-Type" = "text/xml", "SOAPAction" = SOAPAction)

    print(paste("downloading values from:", url, "..."))

    downloaded <- FALSE
    download.time <- system.time(
      err <- tryCatch({
        response <- POST(url, body = envelope, add_headers(headers))
        status <- http_status(response)$message
        downloaded <- TRUE
      },error = function(e) {
        print(conditionMessage(e))
      }
      )
    )
    if (!downloaded) {
      attr(df, "download.time") <- download.time["elapsed"]
      attr(df, "download.status") <- err
      attr(df, "parse.time") <- NA
      attr(df, "parse.status") <- NA
      return(df)
    }

    status.code <- http_status(response)$category
    print(paste("download time:", download.time["elapsed"], "seconds, status:", status.code))
    # check for bad status code
    if (status.code != "success") {
      status.message <- http_status(response)$message
      attr(df, "download.time") <- download.time["elapsed"]
      attr(df, "download.status") <- status.message
      attr(df, "parse.time") <- NA
      attr(df, "parse.status") <- NA
      return(df)
    }
  } else {
    #REST
    version <- "1.1"

    print(paste("downloading values from:", server, "..."))

    downloaded <- FALSE
    download.time <- system.time(
      err <- tryCatch({
        response <- GET(server)
        status <- http_status(response)$message
        downloaded <- TRUE
      },error = function(e) {
        print(conditionMessage(e))
      }
      )
    )
    if (!downloaded) {
      attr(df, "download.time") <- download.time["elapsed"]
      attr(df, "download.status") <- err
      attr(df, "parse.time") <- NA
      attr(df, "parse.status") <- NA
      return(df)
    }

    status.code <- http_status(response)$category
    print(paste("download time:", download.time["elapsed"], "seconds, status:", status.code))
    # check for bad status code
    if (status.code != "success") {
      status.message <- http_status(response)$message
      attr(df, "download.time") <- download.time["elapsed"]
      attr(df, "download.status") <- status.message
      attr(df, "parse.time") <- NA
      attr(df, "parse.status") <- NA
      return(df)
    }
  }
  attr(df, "download.time") <- download.time["elapsed"]
  attr(df, "download.status") <- status.code

  ######################################################
  # Parsing the WaterML XML Data                       #
  ######################################################

  print("reading data values WaterML ...")
  doc <- NULL
  err <- tryCatch({
    doc <- content(response)
  }, warning = function(w) {
    print("Error reading WaterML: Bad XML format.")
    attr(df, "parse.status") <- "Bad XML format"
    attr(df, "parse.time") <- 0
    return(df)
  }, error = function(e) {
    print("Error reading WaterML: Bad XML format.")
    attr(df, "parse.status") <- "Bad XML format"
    attr(df, "parse.time") <- 0
    return(df)
  }
  )
  if (is.null(doc)) {
    print("Error reading WaterML: Bad XML format.")
    attr(df, "parse.status") <- "Bad XML format"
    attr(df, "parse.time") <- 0
    return(df)
  }

  # specify the namespace information
  ns <- WaterOneFlowNamespace(version)

  #try to find faultstring to look for an error
  fault <- xpathSApply(doc, "//soap:Fault", xmlValue, namespaces=ns)
  if (length(fault) > 0) {
    print(paste("SERVER ERROR in GetValues ", as.character(fault), sep=":"))
    return(df)
  }

  #again check for the status code
  if (status.code == "server error") {
    print(paste("SERVER ERROR in GetValues ", http_status(response)$message))
    return(df)
  }


  # extract the data columns with XPath
  val = xpathSApply(doc, "//sr:value", xmlValue, namespaces=ns)
  N <- length(val)

  # if N is 0: the document is probably not valid
  if (N == 0) {
    timeSeriesElement <- unlist(xpathSApply(doc, "//sr:timeSeries", xmlValue, namespaces=ns))
    if (is.null(timeSeriesElement)) {
      print("Error reading WaterML: Bad XML format. <timeSeries> tag not found.")
      attr(df, "parse.status") <- "Bad XML format. <timeSeries> tag not found."
      attr(df, "parse.time") <- 0
      return(df)
    }
  }

  bigData <- 10000
  if (N > bigData) {
    print(paste("found", N,"data values"))
    print("processing dateTime...")
  }
  dateTimeRaw = xpathSApply(doc, "//sr:value", xmlGetAttr, name="dateTime", namespaces=ns)
  DateTime <- as.POSIXct(strptime(dateTimeRaw, "%Y-%m-%dT%H:%M:%S"))

  if (N > bigData) { print("processing censorCode...") }
  censorCode = xpathSApply(doc, "//sr:value", xmlGetAttr, name="censorCode", namespaces=ns)
  censorCode <- unlist(censorCode)
  if (is.null(censorCode)) {
    censorCode <- rep("nc", N)
  }
  if (N > bigData) { print("processing qualifiers...") }
  qualifier <- xpathSApply(doc, "//sr:value", xmlGetAttr, name="qualifiers", namespaces=ns)
  qualifier <- unlist(qualifier)
  if (is.null(qualifier)) {
    qualifier <- rep(NA, N)
  }
  # currently we require that all values have a qualifier attached to it,
  # or none of the values have a qualifier
  if (length(qualifier) < N) {
    qualifier <- rep(NA, N)
  }

  if (version == "1.1") {

    if (N > bigData) { print("processing dateTimeUTC...") }
    dateTimeUTC = xpathSApply(doc, "//sr:value", xmlGetAttr, name="dateTimeUTC", namespaces=ns)

    if (N > bigData) { print("converting date and time...") }
    DateTimeUTC <- as.POSIXct(strptime(dateTimeUTC, "%Y-%m-%dT%H:%M:%S"))
    UTCOffset = DateTime - DateTimeUTC

    if (N > bigData) { print("processing methodCode...") }
    methodCode = xpathSApply(doc, "//sr:value", xmlGetAttr, name="methodCode", namespaces=ns)
    methodCode <- unlist(methodCode)
    if (is.null(methodCode)) { methodCode <- NA }

    if (N > bigData) { print("processing sourceCode...") }
    sourceCode = xpathSApply(doc, "//sr:value", xmlGetAttr, name="sourceCode", namespaces=ns)
    sourceCode <- unlist(sourceCode)
    if (is.null(sourceCode)) { sourceCode <- NA }

    if (N > bigData) { print("processing qualityControlLevelCode...") }
    qcCode = xpathSApply(doc, "//sr:value", xmlGetAttr, name="qualityControlLevelCode", namespaces=ns)
    qcCode <- unlist(sourceCode)
    if (is.null(qcCode)) { qcCode <- NA }

    nodata = as.numeric(xpathSApply(doc, "//sr:noDataValue", xmlValue, namespaces=ns))

  } else {

    #WaterML 1.0 usually doesn't provide information on UTC offset
    DateTimeUTC <- DateTime
    UTCOffset <- rep(0, N)

    if (N > bigData) { print ("processing methodID...")}
    methodCode <-  xpathSApply(doc, "//sr:value", xmlGetAttr, name="methodID", namespaces=ns)
    methodCode <- unlist(methodCode)
    if (is.null(methodCode)) { methodCode <- NA }

    if (N > bigData) { print ("processing sourceID...")}
    sourceCode <- xpathSApply(doc, "//sr:value", xmlGetAttr, name="sourceID", namespaces=ns)
    sourceCode <- unlist(sourceCode)
    if (is.null(sourceCode)) { sourceCode <- NA }

    if (N > bigData) { print ("processing qualityControlLevel...")}
    qcCode = xpathSApply(doc, "//sr:value", xmlGetAttr, name="qualityControlLevel", namespaces=ns)
    qcCode <- unlist(qcCode)
    if (is.null(qcCode)) { qcCode <- NA }
    if (length(qcCode) < N) { qcCode <- NA }

    nodata = as.numeric(xpathSApply(doc, "//sr:NoDataValue", xmlValue, namespaces=ns))
  }


  #make the data frame
  df <- data.frame(
    time=DateTime,
    DataValue=as.numeric(val),
    UTCOffset=UTCOffset,
    Qualifier=qualifier,
    CensorCode=censorCode,
    DateTimeUTC=DateTimeUTC,
    MethodCode=methodCode,
    SourceCode=sourceCode,
    QualityControlLevelCode=qcCode,
    stringsAsFactors=TRUE
  )

  if (nrow(df) == 0) {
    print(paste("no data values found:", url))
    return(df)
  }

  #normal case: no aggregation
  df[df$DataValue == nodata,2] <- NA

  #special case: daily data aggregation
  if (!is.null(daily)) {
    validdata <- na.omit(df)
    if (nrow(validdata) == 0) {
      print("no valid data found!")
      return(df)
    }
    validdata$time <- as.Date(as.POSIXct(validdata$time))
    dailyValues <- aggregate(validdata$DataValue, list(validdata$time), daily)
    names(dailyValues)[1] <- "time"
    names(dailyValues)[2] <- "DataValue"
    return(dailyValues)
  }

  return(df)
}
