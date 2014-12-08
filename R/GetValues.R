#' GetValues
#'
#' This function gets the time series data values from the WaterML web service
#'
#' @import XML
#' @importFrom RCurl getURL
#' @param server The URL of the web service ending with .asmx,
#'  for example: http://worldwater.byu.edu/interactive/rushvalley/services/index.php/cuahsi_1_1.asmx
#' @param site The site code. To get a list of available site codes, see GetSites() function
#' @param variable The variable code. To get a list of possible variable codes, see GetVariables()
#' @param startDate The start date in "yyyy-mm-dd" format
#' @param endDate The end date in "yyyy-mm-dd" format
#' @param daily Defaults to NULL. If you set daily="max", daily="min" or daily="mean", then the
#' data values are aggreagted to daily time step.
#' @keywords waterml
#' @export
#' @examples
#' GetValues("http://worldwater.byu.edu/interactive/rushvalley/services/index.php/cuahsi_1_1.asmx",
#'            site="Ru5BMMA", variable="SRS_Nr_NDVI", startDate="2014-11-01", endDate="2014-11-21",
#'            daily="max")

GetValues <- function(server, site, variable, startDate, endDate, daily=NULL) {
  base_url <- paste(server, "/GetValuesObject", sep="")
  network = "WWO"
  url = paste(base_url, "?location=", network, ":", site,
              "&variable=", network, ":", variable, sep="",
              "&startDate=",startDate, "&endDate=",endDate)
  print(url)

  text = RCurl::getURL(url)
  doc = xmlRoot(xmlTreeParse(text, getDTD=FALSE, useInternalNodes = TRUE))

  variable <- xmlToList(doc[[2]][[2]])
  noData <- as.numeric(variable$noDataValue)


  vals <- doc[[2]][[3]]

  if (is.null(vals)){
    print(paste("no data values found:", url))
    return(NULL)
  }
  if (xmlValue(vals) == "") {
    print(paste("no data values found:", url))
    return(NULL)
  }

  valCount = xmlSize(vals)
  print(paste("valCount:", valCount))
  xmNames = xmlSApply(vals, xmlName)
  val = c()
  dt = c()
  for (j in 1:valCount){
    if(xmlName(vals[[j]]) == 'value') {
      dt <- c(dt, xmlAttrs(vals[[j]])["dateTime"])
      val <- c(val, as.numeric(xmlValue(vals[[j]])))
    }
  }

  print(length(val))
  if (length(val) == 0) {
    return (NULL)
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
